//===--- ConvertToAutoParameter.cpp --------------------------------------*- C++-*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#include "AST.h"
#include "FindTarget.h"
#include "SourceCode.h"
#include "refactor/Tweak.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Error.h"

namespace clang {
namespace clangd {
namespace {
/// Replaces type of declaration type with `auto`
/// Before:
///     template <typename T>
///     auto foo(T param) { }
///          ^^^^^^^^^^^^
/// After:
///     auto foo(auto param) { }
class ConvertToAutoParameter : public Tweak {
public:
  const char *id() const final;

  bool prepare(const Selection &Inputs) override;
  Expected<Effect> apply(const Selection &Inputs) override;
  std::string title() const override { return "Convert to auto parameter"; }
  llvm::StringLiteral kind() const override {
    return CodeAction::REFACTOR_KIND;
  }

  template <typename T, typename NodeKind>
  static auto findNode(const SelectionTree::Node &Root) -> const T *;

//  template <typename T>
//  static auto findExpression(const SelectionTree::Node &Root)
//      -> std::tuple<const T *, const SelectionTree::Node *> {
//    return findNode<T, Expr>(Root);
//  }
//
  template <typename T>
  static auto findDeclaration(const SelectionTree::Node &Root) -> const T * {
    return findNode<T, Decl>(Root);
  }

private:
};

REGISTER_TWEAK(ConvertToAutoParameter)

bool ConvertToAutoParameter::prepare(const Selection &Inputs) {

  return true;
}

Expected<Tweak::Effect> ConvertToAutoParameter::apply(const Selection &Inputs) {
  const auto *Root = Inputs.ASTSelection.commonAncestor();
//  if (!Root)
//    return;

  const auto *FunctionTemplateDeclaration = findDeclaration<FunctionTemplateDecl>(*Root);
//  if (!FunctionTemplateDeclaration)
//    return;

  auto path = Inputs.AST->tuPath();
  auto aaaaarg = Inputs.Index->indexedFiles()(path);

  // Get all function template type parameters
  auto *TemplateParameters = FunctionTemplateDeclaration->getTemplateParameters();
//  auto *TemplateParameters = FunctionTemplateDeclaration->getDescribedTemplateParams();
//  auto *TemplateParameters = FunctionTemplateDeclaration

//  FunctionTemplateDeclaration->getDescribedTemplateParams();

  for (auto *TemplateParameter : *TemplateParameters) {
//    auto *Foo = dyn_cast_or_null<TemplateTypeParmDecl>(TemplateParameter);
//    if (!Foo)
//      return false;
//

    findExplicitReferences(TemplateParameter, )

    auto TemplateParameterSymbolID = getSymbolID(FunctionTemplateDeclaration);

    RefsRequest ReferenceRequest{};
    ReferenceRequest.IDs.insert(TemplateParameterSymbolID);
//    ReferenceRequest.Filter = RefKind::Reference;
    ReferenceRequest.Limit = 1;
//    ReferenceRequest.Filter = RefKind::Reference;

    std::vector<Ref> References{};

    bool bar = Inputs.Index->refs(ReferenceRequest, [&](auto Reference) {
      References.push_back(Reference);
    });

    LookupRequest LookupRequest{};
    LookupRequest.IDs.insert(TemplateParameterSymbolID);

    Inputs.Index->lookup(LookupRequest, [&](auto Reference) {
      auto foo = Reference;
    });

    RelationsRequest RelationsRequest{};
    RelationsRequest.Subjects.insert(TemplateParameterSymbolID);

    Inputs.Index->relations(RelationsRequest, [&](auto a, auto b) {
      auto xyz = a;
    });

    // Check if type parameters are only used once
    if (References.size() == 1) {
//      return false;
    }

    // Check if the only usage is a function parameter
    auto Parameters = FunctionTemplateDeclaration->getAsFunction()->parameters();

    auto Found = false;
    for (auto *Parameter : Parameters) {
//      QualType Type = Parameter->getType();
//      if (!Type->isTemplateTypeParmType())
//        return false;

//      Parameter->getType();
////
//      if (getSymbolID(Parameter->getOriginalType()) == TemplateParameterSymbolID) {
//        Found = true;
//        break;
//      }
    }

//    if (!Found)
//      return false;


    // Check if the function parameter is a simple value parameter
  }

  return Effect::showMessage("");
}

template <typename T, typename NodeKind>
auto ConvertToAutoParameter::findNode(const SelectionTree::Node &Root) -> const T * {
  for (const auto *Node = &Root; Node; Node = Node->Parent) {
    if (const T *Result = dyn_cast_or_null<T>(Node->ASTNode.get<NodeKind>()))
      return Result;
  }

  return nullptr;
}

} // namespace
} // namespace clangd
} // namespace clang
