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

#include "clang/Frontend/FrontendPluginRegistry.h"
#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Sema/Sema.h"
#include "llvm/Support/raw_ostream.h"
#include <iostream>
using namespace clang;

namespace {

class PrintFunctionsConsumer : public ASTConsumer {
  [[maybe_unused]] CompilerInstance &Instance;
  std::set<std::string> ParsedTemplates;

// MemberExpr 0xabc <line:1:2,col:3> 'Lisp_Object':'struct Lisp_X *' lvalue .f_Vafter_init_time 0xabc
// `-DeclRefExpr 0xabc <col:26> 'struct emacs_globals':'struct emacs_globals' lvalue Var 0xabc 'globals' 'struct emacs_globals':'struct emacs_globals'

public:
  PrintFunctionsConsumer(CompilerInstance &Instance,
                         std::set<std::string> ParsedTemplates)
      : Instance(Instance), ParsedTemplates(ParsedTemplates) {}

  bool HandleTopLevelDecl(DeclGroupRef DG) override {
    for (DeclGroupRef::iterator i = DG.begin(), e = DG.end(); i != e; ++i) {
      const Decl *D = *i;
      if (const NamedDecl *ND = dyn_cast<NamedDecl>(D))
        llvm::errs() << "NamedDecl:\t\"" << ND->getNameAsString() << "\"\n";
    }

    return true;
  }

  void HandleTranslationUnit(ASTContext& context) override {
    struct Visitor : public RecursiveASTVisitor<Visitor> {
      bool VisitMemberExpr(MemberExpr *ME) {
        llvm::errs() << "MemberExpr:\t\"" << ME << "\"\n";
        if (DeclRefExpr* declref = dyn_cast<DeclRefExpr>(ME->getBase())) {
          auto tname = declref->getDecl()->getType().getAsString();
          llvm::errs() << ".DeclRefExpr:\t\"" << tname << "\"\n";
          if (tname == "struct emacs_globals") {
            llvm::errs() << ".MemberDecl:\t\"" << ME->getMemberDecl()->getName() << "\"\n";
          }
        }
        return true;
      }

      std::set<FunctionDecl*> LateParsedDecls;
    } v;
    v.TraverseDecl(context.getTranslationUnitDecl());
  }
};

class PrintFunctionNamesAction : public PluginASTAction {
  std::set<std::string> ParsedTemplates;
protected:
  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                 llvm::StringRef) override {
    return std::make_unique<PrintFunctionsConsumer>(CI, ParsedTemplates);
  }

  bool ParseArgs(const CompilerInstance &CI,
                 const std::vector<std::string> &args) override {
    for (unsigned i = 0, e = args.size(); i != e; ++i) {
      llvm::errs() << "PrintFunctionNames arg = " << args[i] << "\n";

      // Example error handling.
      DiagnosticsEngine &D = CI.getDiagnostics();
      if (args[i] == "-an-error") {
        unsigned DiagID = D.getCustomDiagID(DiagnosticsEngine::Error,
                                            "invalid argument '%0'");
        D.Report(DiagID) << args[i];
        return false;
      }
      if (args[i] == "-parse-template") {
        if (i + 1 >= e) {
          D.Report(D.getCustomDiagID(DiagnosticsEngine::Error,
                                     "missing -parse-template argument"));
          return false;
        }
        ++i;
        ParsedTemplates.insert(args[i]);
      }
    }
    if (!args.empty() && args[0] == "help")
      PrintHelp(llvm::errs());

    return true;
  }
  void PrintHelp(llvm::raw_ostream& ros) {
    ros << "Help for PrintFunctionNames plugin goes here\n";
  }

};

} // namespace

static FrontendPluginRegistry::Add<PrintFunctionNamesAction>
X("print_fns", "print function names");
