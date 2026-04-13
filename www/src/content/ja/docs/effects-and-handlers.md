---
title: "エフェクトとハンドラ"
description: "通常の Musi コードの中で <code>effect</code>、<code>perform</code>、<code>handle</code>、<code>resume</code> を使います。"
group: "エフェクト"
section: "エフェクト"
order: 12
slug: "effects-and-handlers"
summary: "Musi の大きな特徴を、実際の構文で示します。"
---

エフェクトは普通の Musi コードの一部です。エフェクトを定義し、操作を `perform` し、境界で `handle` します。

## まず handle する

{{snippet:handle-console}}

ハンドラは操作要求にどう応じるかを決めます。`resume` は値を渡して実行を続けます。

## エフェクトを定義する

{{snippet:effect-console}}

## 操作を perform する

{{snippet:perform-console}}

## 比較

{{example:effect-handle}}

直接呼び出しだと境界のロジックが多くの層ににじむような場面で、エフェクトは役立ちます。

## 試す

{{try:effects-and-handlers}}

## 次の一歩

3 つのコード例を最初から最後まで読み、[属性と foreign 宣言](/docs/attributes-and-foreign) へ進んでください。
