---
title: "基盤ライブラリと標準ライブラリ"
description: "<code>@std</code> を使う場面と、より低い層の基盤名を見ている場面を区別します。"
group: "ツール"
section: "ツール"
order: 15
slug: "foundation-and-standard-library"
summary: "標準ライブラリ群と、より低い層の基盤名前空間。"
---

多くのコードは `@std` から始まります。
`musi:*` は基盤レベルの機能が必要なときに使う、より低い層の名前空間です。

## 基本の分け方

- 日常の作業には `@std`
- 低い層の操作には `musi:*`

ほとんどのコードは `@std` にとどめるべきです。言語ランタイムの境界や、より低い層のコンパイラ向けツールを扱うときに `musi:*` を使います。

{{snippet:stdlib-option-import}}

{{snippet:stdlib-result-import}}

## 比較
{{example:import-stdlib}}

## 試す

{{try:foundation-and-standard-library}}

## 次の一歩

プロジェクトのインポートを 1 つ `@std` に移し、その後 [テストと実行](/docs/testing-and-running) へ進んでください。
