---
title: "ファイル、パッケージ、エントリ"
description: "<code>musi new</code> が何を作り、<code>musi run</code> が何を探すかを知ります。"
group: "開始"
section: "開始"
order: 3
slug: "files-packages-and-entry"
summary: "パッケージ、<code>musi.json</code>、解決されるエントリファイル。"
---

試す段階では素のファイルを使います。繰り返し使うコマンドと安定したプロジェクトルートが必要になったらパッケージを使います。

## パッケージの流れ

{{snippet:package-commands}}

- `musi` はパッケージ設定を読み、エントリファイルを解決し、プロジェクト用コマンドを実行します。
- `music` は 1 つのソースファイルやビルド済みの成果物を直接調べるときにも使えます。

## パッケージが役立つ理由

パッケージを使うとパス処理の重複が減ります。プロジェクト全体で同じコマンド体系を共有しやすくなります。

## 直接実行モード

{{snippet:music-direct}}

パッケージ用の設定がまだ要らない 1 回きりの実験、パーサの確認、小さな例では直接実行モードが向いています。

## 試す

{{try:files-packages-and-entry}}

## 次の一歩

パッケージを作り、どのエントリファイルが使われるかを確認してから [インポートとパッケージ](/docs/imports-and-packages) へ進んでください。
