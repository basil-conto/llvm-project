//===- PrintFunctionNames.cpp ---------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Example clang plugin which simply prints the names of all the top-level decls
// in the input file.
//
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/Expr.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/AST/Stmt.h"
#include "clang/AST/Type.h"
#include "clang/Basic/LLVM.h"
#include "clang/Basic/Specifiers.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendPluginRegistry.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/IR/Analysis.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Type.h"
#include "llvm/Passes/OptimizationLevel.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
using namespace clang;

namespace {

// TODO:
// - Don't use dyn_cast on AST!
// - RecordDecl / RecordType / getAsRecordDecl / getAsStructureType
// - StmtIterator
// - InstVisitor
// - def-use chain X->users()

static bool cmp_name(MemberExpr *a, MemberExpr *b) {
  return a->getMemberDecl()->getName() < b->getMemberDecl()->getName();
}

std::set<MemberExpr*, decltype(cmp_name)*> EmacsGlobals{cmp_name};
std::map<std::string, std::set<std::string>> EmacsMap;

class PrintPass final : public llvm::AnalysisInfoMixin<PrintPass> {
  friend struct llvm::AnalysisInfoMixin<PrintPass>;

public:
  using Result = llvm::PreservedAnalyses;

  Result run(llvm::Module &M, llvm::ModuleAnalysisManager &MAM) {
    // Whether we inserted something.
    bool changed = false;

    auto &CTX = M.getContext();
    llvm::PointerType *PrintfArgTy = llvm::PointerType::getUnqual(
      llvm::Type::getInt8Ty(CTX));

    llvm::dbgs() << "(llvm-plugin compile) printf exists: "
                 << (M.getFunction("printf") ? "yes" : "no")
                 << '\n';

    // Inject printf declaration.
    llvm::FunctionType *PrintfTy = llvm::FunctionType::get(
      llvm::IntegerType::getInt32Ty(CTX),
      PrintfArgTy,
      true);
    llvm::FunctionCallee Printf = M.getOrInsertFunction("printf", PrintfTy);

    // Set printf attributes.
    llvm::Function *PrintfF = dyn_cast<llvm::Function>(Printf.getCallee());
    PrintfF->setDoesNotThrow();
    PrintfF->addParamAttr(0, llvm::Attribute::NoCapture);
    PrintfF->addParamAttr(0, llvm::Attribute::ReadOnly);

    // Inject printf format string as global variable.
    llvm::Constant *PrintfFormatStr = llvm::ConstantDataArray::getString(
      CTX, "(llvm-plugin run) %s has variable %s\n");

    llvm::Constant *PrintfFormatStrVar =
      M.getOrInsertGlobal("PrintfFormatStr", PrintfFormatStr->getType());
    dyn_cast<llvm::GlobalVariable>(PrintfFormatStrVar)->setInitializer(PrintfFormatStr);

    // Inject printf into each function.
    for (auto &F : M) {
      if (F.isDeclaration())
        continue;

      auto& vars = EmacsMap[F.getName().str()];

      if (vars.empty())
        continue;

      llvm::dbgs() << "(llvm-plugin compile) Instrumenting: " << F.getName() << "\n";

      // Make IR builder inserting at start of function.
      llvm::IRBuilder<> Builder(&*F.getEntryBlock().getFirstInsertionPt());

      // Inject function name as global variable.
      auto *FuncName = Builder.CreateGlobalStringPtr(F.getName());

      // Cast array to pointer.
      llvm::Value *FormatStrPtr =
        Builder.CreatePointerCast(PrintfFormatStrVar, PrintfArgTy, "formatStr");

      for (auto& v : vars) {
        auto *VarName = Builder.CreateGlobalStringPtr(v);

        // Inject printf call.
        Builder.CreateCall(Printf, {FormatStrPtr, FuncName, VarName});
      }

      changed = true;
    }

    llvm::dbgs() << "<- Final mapping:\n";
    for (const auto& [fn, vars] : EmacsMap) {
      if (vars.empty()) continue;
      llvm::dbgs() << "   " << fn << '\n';
      for (const auto& v : vars) {
        llvm::dbgs() << "     " << v << '\n';
      }
    }
    llvm::dbgs() << "<- Done\n";

    return changed
      ? llvm::PreservedAnalyses::none()
      : llvm::PreservedAnalyses::all();
  }

  // Run pass even with optimizations disabled.
  static bool isRequired() { return true; }
};

void PrintCallback(llvm::PassBuilder &PB) {
  PB.registerPipelineStartEPCallback(
    [](llvm::ModulePassManager &MPM, llvm::OptimizationLevel) {
      MPM.addPass(PrintPass());
    });
}

class PrintFunctionsConsumer : public ASTConsumer {

public:
  PrintFunctionsConsumer(CompilerInstance &Instance) {
    Instance.getCodeGenOpts().PassBuilderCallbacks.push_back(PrintCallback);
  }

  bool HandleTopLevelDecl(DeclGroupRef DG) override {
    for (const Decl *D : DG) {
      if (const auto *ND = dyn_cast<NamedDecl>(D))
        llvm::dbgs() << "NamedDecl:\t\"" << ND->getNameAsString() << "\"\n";
    }

    return true;
  }

  void HandleTranslationUnit(ASTContext& context) override {

    struct VVisitor : public RecursiveASTVisitor<VVisitor> {
      std::set<std::string>& vars;
      VVisitor(std::set<std::string>& vars) : vars(vars) {}

      bool VisitMemberExpr(MemberExpr *ME) {
        if (const auto *declref = dyn_cast<DeclRefExpr>(ME->getBase())) {
          auto tname = declref->getDecl()->getType().getAsString();
          if (tname == "struct emacs_globals") {
            // llvm::dbgs() << ".MemberDecl:\t\"" << ME->getMemberDecl()->getName() << "\"\n";
            EmacsGlobals.insert(ME);
            vars.insert(std::string(ME->getMemberDecl()->getName()));
          }
        }
        return true;
      }

    };

    struct FVisitor : public RecursiveASTVisitor<FVisitor> {
      ASTContext &context;
      FVisitor(ASTContext& context) : context(context) {}

      bool VisitFunctionDecl(FunctionDecl *FD) {
        llvm::dbgs() << "Visiting: " << FD->getName() << "\n";

        std::set<std::string> vars;
        VVisitor v{vars};
        v.TraverseDecl(FD);
        EmacsMap[std::string(FD->getName())] = vars;

        for (const auto& v : vars)  {
          llvm::dbgs().indent(4) << v << '\n';
        }

        return true;
      }

    } fv(context);

    fv.TraverseDecl(context.getTranslationUnitDecl());
  }
};

class PrintFunctionNamesAction : public PluginASTAction {
protected:
  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                 llvm::StringRef) override {
    return std::make_unique<PrintFunctionsConsumer>(CI);
  }

  bool ParseArgs(const CompilerInstance &CI,
                 const std::vector<std::string> &args) override {
    return true;
  }

  PluginASTAction::ActionType getActionType() override {
    return AddBeforeMainAction;
  }

};

} // namespace

static FrontendPluginRegistry::Add<PrintFunctionNamesAction>
X("print_fns", "print function names");
