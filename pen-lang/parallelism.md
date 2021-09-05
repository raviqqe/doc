# Pen 言語における並列計算

---

# 目次

- 並列計算の目的
- 多言語での並列計算の仕組み
- Pen 言語での設計（未実装）

---

## 世はマルチコア時代

- マルチコア使って処理を速くしたい
  - データ並列: 大量の（同種の）データを同時に処理したい <- 今日はこっち
  - タスク並列: 異なるコードを同時に実行したい

---

## Go 言語のチャネル

- いわゆる concurrent queue
- スレッドセーフ
  - 複数の goroutines（グリーンスレッド）から同時にアクセスできる
- 用途：並列な HTTP サーバ、並列バッチ処理

### 例

- チャネル型: `chan int`
- エンキュー: `c <- 42`
- デキュー: `x := <- c`
- チャネルのクローズ: `close(c)`

---

## Go 言語のチャネルの利点・欠点

### 利点

- 高パフォーマンス
- スレッドのような概念と相性がいい

### 欠点

- 関数型言語と相性が悪い
  - ミュータブルで状態を持つ
  - 手続き的な操作

---

## Cloe 言語とは

- https://cloe-lang.org
- インタプリタ型
- Go 言語製
- 今は、もう、メンテされていない
- **遅延リストとプリミティブの関数で並列計算**を実装
  - https://cloe-lang.org/api-reference/builtin#parallelism-and-concurrency

---

## 遅延リストのパフォーマンス

- HTTP のサーバーを実装
  - HTTP リクエストをコンカレントに受け付ける
- Go, Node.js, Cloe で比較

### 結果

- Go >>> Node.js > Cloe
- Cloe が Node.js より 20 ~ 30%遅い
- 実際の HTTP サーバーの実装では、各リクエストの処理でそれなりに時間がかかる。
  - -> 実用に耐える？
- データインテンシブな用途では確実に負けそう

---

## 遅延リストによる並列計算の制限

- サンクの実装にパフォーマンスが律速される
  - https://github.com/composewell/streaming-benchmarks
- 全ての値が帰納的 -> 再帰ができない
  - e.g. タスクがタスクを生む場合、バックプレッシャ
  - 大体のコンカレンシーパターンは実装できる
    - 内部的には concurrent queue を使うので、それはそう
    - "Concurrency in Go" by Katherine Cox-Buday
  - Pen 言語特有の問題
  - ある種のデッドロックを防げる
- [スペースリーク](https://wiki.haskell.org/Space_leak)

---

## Pen 言語での設計

### 必要条件

- ジェネリクス
- 決定性を破壊しない -> コンテキストを渡す

### 例

```pen
import Core'Parallel

foo = \(ctx ConcurrencyContext, xs [number]) [number] {
  concurrency = 200

  Parallel'Map(ctx, concurrency) ~ xs
}
```

```pen
Parallel'Split(foo)
```

---

## ご意見・ご感想お待ちしております
