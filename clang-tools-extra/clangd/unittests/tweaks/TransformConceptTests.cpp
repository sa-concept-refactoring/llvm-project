//===-- TransformConceptTests.cpp ---------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "TweakTesting.h"
#include "gmock/gmock-matchers.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace clang {
namespace clangd {
namespace {

TWEAK_TEST(TransformConcept);

TEST_F(TransformConceptTest, Test) {
  Header =
    R"cpp(
      template <typename T>
      concept foo = true;

      template <typename T>
      concept bar = true;

      template <typename T, typename U>
      concept baz = true;
    )cpp";

  ExtraArgs = { "-std=c++20" };

//  EXPECT_EQ(
//      apply("template<typename T> void f(T) requires f^oo<T> {}"),
//      "template<foo T> void f(T) {}");

  EXPECT_AVAILABLE(
    R"cpp(
      template <typename T> void f(T)
        requires ^f^o^o^<^T^> {}
    )cpp"
  );

  EXPECT_AVAILABLE(
      R"cpp(
      template <typename T, typename U> void f(T)
        requires ^f^o^o^<^T^> {}
    )cpp"
  );

  EXPECT_UNAVAILABLE(
    R"cpp(
      template <bar T> void f(T)
        requires ^f^o^o^<^T^> {}
    )cpp"
  );

  EXPECT_UNAVAILABLE(
    R"cpp(
      template <typename T, typename U> void f(T, U)
        requires ^b^a^z^<^T^,^ ^U^> {}
    )cpp"
  );
}

} // namespace
} // namespace clangd
} // namespace clang
