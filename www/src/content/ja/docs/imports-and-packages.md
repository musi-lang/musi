---
title: "インポートとパッケージ"
description: "モジュールをインポートし、主な名前空間を使い分けます。"
group: "言語の基本"
section: "言語の基本"
order: 4
slug: "imports-and-packages"
summary: "インポート式、<code>@std</code>、<code>musi:*</code> の基盤名前空間。"
---

インポートは式です。`let` で束縛し、その後は他の名前と同じように使います。

## 基本ルール

- まず `@std` から始める
- より低い層の基盤モジュールが必要なときだけ `musi:*` を使う

この方針にすると通常のコードは標準ライブラリ側に寄り、コンパイラ寄りの部品は明示的なまま保てます。

## 例

{{example:import-stdlib}}

インポートは式が置ける場所ならどこでも書けますが、ファイルの上の方に寄せるのが今でも最も読みやすい書き方です。

## 試す

{{try:imports-and-packages}}

## 次の一歩

重複したコードをインポートした名前へ置き換え、[式と束縛](/docs/expressions-and-bindings) へ進んでください。
