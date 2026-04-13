---
title: "属性と foreign 宣言"
description: "安定した公開属性、予約済みコンパイラ属性、foreign バインディングを使います。"
group: "抽象化"
section: "抽象化"
order: 13
slug: "attributes-and-foreign"
summary: "安定した公開属性、予約済みコンパイラ属性、foreign バインディング。"
---

属性は宣言に付く単純なメタデータです。大半は公開用です。`@known` と `@intrinsic` の 2 つはコンパイラ側が持ちます。

## Foreign バインディング

{{snippet:foreign-puts}}

## 公開属性

宣言に明示的なメタデータが必要なときは次を使います。

- `@link`
- `@when`
- `@repr`
- `@layout`
- `@frozen`
- `@hot`
- `@cold`
- `@deprecated`
- `@since`

{{snippet:attr-link-foreign}}

`@frozen` は export される non-opaque `data` に対する ABI/layout の約束です。不変を意味するわけではありません。

`@hot` と `@cold` は callable 宣言に付く optimizer hint です。意味論は変えません。

## 予約済みコンパイラ属性

予約済み属性は基盤層やコンパイラ管理のモジュールの中でだけ使います。

- `@known(name := "...")`
- `@intrinsic(name := "...")`

`@known` は `Type` や `CString` のようなコンパイラ既知の束縛を示します。

`@intrinsic` は `musi:intrinsics` にあるコンパイラ管理のランタイムフックを示します。一般的な利用者向けメタデータではありません。

## 試す

{{try:attributes-and-foreign}}

## 次の一歩

まず foreign の例を読み、その後 [quote と構文値](/docs/quote-and-syntax) へ進んでください。
