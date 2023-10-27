//===--- ConvertToAutoParameter.cpp --------------------------------------*- C++-*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
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

private:
};

REGISTER_TWEAK(ConvertToAutoParameter)

bool ConvertToAutoParameter::prepare(const Selection &Inputs) {
  return true;
}

Expected<Tweak::Effect> ConvertToAutoParameter::apply(const Selection &Inputs) {
  return Effect::showMessage("");
}

} // namespace
} // namespace clangd
} // namespace clang
