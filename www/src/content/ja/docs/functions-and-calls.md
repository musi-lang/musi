---
title: "関数と呼び出し"
description: "<code>let</code> で関数を定義し、普通に呼び出し、再帰には <code>let rec</code> を使います。"
group: "言語の基本"
section: "言語の基本"
order: 7
slug: "functions-and-calls"
summary: "余計な制御構文なしで書く関数、呼び出し、再帰。"
---

関数は束縛でき、渡せて、呼び出せる値です。

## 基本の呼び出し

{{example:double-function}}

呼び出しは普通に `name(args)` と書きます。関数定義も他の束縛と同じ `let` 構文を使います。

## recursion

{{snippet:recursive-case}}

関数が自分自身を参照する必要があるときは `let rec` を使います。

## 試す

{{try:functions-and-calls}}

## 次の一歩

関数を束縛して呼び出し、再帰の形も足してから [data とパターンマッチ](/docs/data-and-pattern-matching) へ進んでください。
