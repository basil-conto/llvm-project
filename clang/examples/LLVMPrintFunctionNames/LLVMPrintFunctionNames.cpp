//===- LLVMPrintFunctionNames.cpp
//---------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Example clang plugin which simply prints the names of all the functions
// within the generated LLVM code.
//
//===----------------------------------------------------------------------===//

#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendPluginRegistry.h"
#include "clang/Sema/Sema.h"
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
      CTX, "(llvm-plugin run) %s has variable …\n");

    llvm::Constant *PrintfFormatStrVar =
      M.getOrInsertGlobal("PrintfFormatStr", PrintfFormatStr->getType());
    dyn_cast<llvm::GlobalVariable>(PrintfFormatStrVar)->setInitializer(PrintfFormatStr);

    // Inject printf into each function.
    for (auto &F : M) {
      if (F.isDeclaration())
        continue;

      llvm::dbgs() << "(llvm-plugin compile) Instrumenting: "
                   << F.getName() << "\n";

      // Make IR builder inserting at start of function.
      llvm::IRBuilder<> Builder(&*F.getEntryBlock().getFirstInsertionPt());

      // Inject function name as global variable.
      auto *FuncName = Builder.CreateGlobalStringPtr(F.getName());

      // Cast array to pointer.
      llvm::Value *FormatStrPtr =
        Builder.CreatePointerCast(PrintfFormatStrVar, PrintfArgTy, "formatStr");

      // Inject printf call.
      Builder.CreateCall(Printf, {FormatStrPtr, FuncName});

      changed = true;
    }

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

class LLVMPrintFunctionsConsumer : public ASTConsumer {
public:
  LLVMPrintFunctionsConsumer(CompilerInstance &Instance) {
    Instance.getCodeGenOpts().PassBuilderCallbacks.push_back(PrintCallback);
  }
};

class LLVMPrintFunctionNamesAction : public PluginASTAction {
protected:
  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                 llvm::StringRef) override {
    return std::make_unique<LLVMPrintFunctionsConsumer>(CI);
  }
  bool ParseArgs(const CompilerInstance &,
                 const std::vector<std::string> &) override {
    return true;
  }
  PluginASTAction::ActionType getActionType() override {
    return AddBeforeMainAction;
  }
};

} // namespace

static FrontendPluginRegistry::Add<LLVMPrintFunctionNamesAction>
    X("llvm-print-fns", "print function names, llvm level");
