//===--- SwapIfBranches.cpp --------------------------------------*- C++-*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#include "ParsedAST.h"
#include "SourceCode.h"
#include "refactor/Tweak.h"
#include "support/Logger.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/ExprConcepts.h"
#include "clang/AST/Stmt.h"
#include "clang/Basic/LangOptions.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Tooling/Core/Replacement.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Error.h"

namespace clang {
namespace clangd {
namespace {
/// Test tweak for Vina
class DoMagic : public Tweak {
public:
  const char *id() const final;

  bool prepare(const Selection &Inputs) override;
  Expected<Effect> apply(const Selection &Inputs) override;
  std::string title() const override { return "Do magic stuff"; }
  llvm::StringLiteral kind() const override {
    return CodeAction::REFACTOR_KIND;
  }

private:
  const ConceptSpecializationExpr *ConceptSpecializationExpression = nullptr;
};

REGISTER_TWEAK(DoMagic)

bool DoMagic::prepare(const Selection &Inputs) {
  const auto *Node = Inputs.ASTSelection.commonAncestor()->Parent;
  const auto *Expression = Node->ASTNode.get<Expr>();

  ConceptSpecializationExpression = dyn_cast_or_null<ConceptSpecializationExpr>(Expression);

  if(ConceptSpecializationExpression == nullptr)
  {
    return false;
  }

  return true;
}

Expected<Tweak::Effect> DoMagic::apply(const Selection &Inputs) {
  const auto *Node = Inputs.ASTSelection.commonAncestor()->Parent;
  auto &Ctx = Inputs.AST->getASTContext();
  auto &SrcMgr = Inputs.AST->getSourceManager();

  auto TypeRng = toHalfOpenFileRange(SrcMgr, Ctx.getLangOpts(),
                      ConceptSpecializationExpression->getSourceRange());

  if(!TypeRng)
    return error("Could not obtain range of the 'type' branch. Macros?");

  auto TypeCode = toSourceCode(SrcMgr, *TypeRng);

  tooling::Replacements Result;
  if(auto Err = Result.add(tooling::Replacement(Ctx.getSourceManager(),
                                  TypeRng->getBegin(),
                                  TypeCode.size(),
                                  TypeCode)))
    return std::move(Err);

  return Effect::mainFileEdit(SrcMgr, std::move(Result));//->showMessage(TypeCode);

  //return Effect::showMessage("I'm a concept! Magic happened!! \\(*0*)/");
}

} // namespace
} // namespace clangd
} // namespace clang
