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
#include "clang/AST/Stmt.h"
#include "clang/AST/ExprConcepts.h"
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
class Test : public Tweak {
public:
  const char *id() const final;

  bool prepare(const Selection &Inputs) override;
  Expected<Effect> apply(const Selection &Inputs) override;
  std::string title() const override { return "Test"; }
  llvm::StringLiteral kind() const override {
    return CodeAction::REFACTOR_KIND;
  }
private:
  const ConceptSpecializationExpr *ConceptSpecializationExpression;
  const TemplateTypeParmDecl *TemplateTypeParameterDeclaration;
};

REGISTER_TWEAK(Test)

bool Test::prepare(const Selection &Inputs) {
  const auto *Node = Inputs.ASTSelection.commonAncestor()->Parent; // TODO: Check why we need the parent
  const auto *Expression = Node->ASTNode.get<Expr>(); // TODO: Check if we should do error handling here

  ConceptSpecializationExpression = dyn_cast_or_null<ConceptSpecializationExpr>(Expression);
  if (ConceptSpecializationExpression == nullptr) {
    return false;
  }

  auto TemplateArguments = ConceptSpecializationExpression->getSpecializationDecl()->getTemplateArguments();
  if (TemplateArguments.size() != 1) {
    return false;
  }

  const auto *TemplateArgument = &TemplateArguments[0];
  if (TemplateArgument->getKind() != TemplateArgument->Type) {
    return false;
  }

  auto TemplateArgumentType = TemplateArgument->getAsType();
  if (!TemplateArgumentType->isTemplateTypeParmType()) {
    return false;
  }

  const auto *TemplateTypeParamType = TemplateArgumentType->getAs<TemplateTypeParmType>();

  const FunctionTemplateDecl *FunctionTemplateDeclaration = nullptr;
  for (const SelectionTree::Node *N = Node->Parent; N && !FunctionTemplateDeclaration; N = N->Parent) {
    FunctionTemplateDeclaration = dyn_cast_or_null<FunctionTemplateDecl>(N->ASTNode.get<Decl>());
  }

  if (FunctionTemplateDeclaration == nullptr) {
    return false;
  }

  auto *TemplateParameter = FunctionTemplateDeclaration->getTemplateParameters()->getParam(TemplateTypeParamType->getIndex());
  TemplateTypeParameterDeclaration = dyn_cast_or_null<TemplateTypeParmDecl>(TemplateParameter);
  if (!TemplateTypeParameterDeclaration->wasDeclaredWithTypename()) {
    return false;
  }

  return true;
}

Expected<Tweak::Effect> Test::apply(const Selection &Inputs) {
  auto &SourceManager = Inputs.AST->getSourceManager();

  tooling::Replacements Replacements{};

  auto ConceptName = ConceptSpecializationExpression->getNamedConcept()->getQualifiedNameAsString();
  auto SourceRange = TemplateTypeParameterDeclaration->getSourceRange();
  auto Foo = SourceManager.getFileOffset(SourceRange.getEnd()) - SourceManager.getFileOffset(SourceRange.getBegin());
  auto Replacement = tooling::Replacement(SourceManager, SourceRange.getBegin(), Foo, ConceptName + ' ');

  if (auto Err = Replacements.add(Replacement)) {
    return Err;
  }

  return Effect::mainFileEdit(SourceManager, Replacements);
}

} // namespace
} // namespace clangd
} // namespace clang
