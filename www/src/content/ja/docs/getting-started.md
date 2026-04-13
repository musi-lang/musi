---
title: "はじめに"
description: "必要なツールを導入し、役割を知り、最初のコマンドから始めます。"
group: "開始"
section: "開始"
order: 1
slug: "getting-started"
summary: "導入、PATH 設定、<code>musi</code> と <code>music</code> の違い。"
---

Musi には 2 つの CLI エントリポイントがあります。まずこの違いを押さえると、その後のツールチェーンが分かりやすくなります。

## 2 つのコマンド

- `musi` はパッケージ単位で動きます。`run`、`check`、`build`、`test` に使います。
- `music` は 1 つのソースのまとまりや、ビルド済みの成果物を直接扱います。

`cargo`、`npm`、`dotnet` のようなツールを知っているなら、`musi` はそれに近い役割です。`music` はファイルを直接動かす実行用コマンドに近い位置づけです。

## 最初のセットアップ

[導入ページ](/install) から始め、次の順に進めます。
- 実行ファイルを導入し PATH を通す
- パッケージを作る
- 最初の式を書く
- `musi check` と `musi run` を実行する

最短で反応を見たいなら、まず 1 ファイルと `music` から始めます。パッケージコマンドや共有のプロジェクト構造が必要になったら `musi` へ移ります。

## 試す

{{try:getting-started}}

## 次の一歩

[導入](/install) を開き、1 つコマンドを動かしてから [最初のプログラム](/docs/first-program) へ進んでください。
