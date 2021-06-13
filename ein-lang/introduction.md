# 自作言語 Ein の紹介

---

# 自己紹介

- 名前：とやま ようた
- GitHub, Twitter: [raviqqe](https://github.com/raviqqe)
- プログラミング言語が好き
- 普段は Web 系エンジニア

---

# 好きな言語は何ですか？

Rust, Go, TypeScript, Ruby, Python, Haskell, OCaml, C, ...

---

# Go が好き

---

# Go のいいところ

- パフォーマンスと表現力のバランス
- 簡潔さ
  - ジェネリクスが無い
  - マクロが無い

```go
package main

import "fmt"

func main() {
  fmt.Println("Hello, world!")
}
```

<!--
- 引き算でできた言語
-->

---

# Go を関数型にすればもっと簡潔になるのでは？

---

# 自作言語 Ein

- 関数型 Go
- Haskell 風の構文

```haskell
import "github.com/ein-lang/os/Os"

main : Os.Os -> Number
main os =
  let
    result = Os.fdWrite os Os.stdout "Hello, world!\n"
  in
    case _ = result
      Number => 0
      Error => 1
```

<!--
- ジェネリクスが無い関数型言語って他に無い
-->

---

# Go との違い

- 不変な値
- カリー化
- インターフェース -> ユニオン型

---

# ロードマップと進捗状況

- LLVM バックエンド
- 部分評価
- CPS 変換
- 参照カウント GC
- WASM バックエンド (<- 今ここ)
- 完全な OS API
- 実用的な言語？
  - 別言語になるかも？

---

# まとめ

- Go は簡潔でいい言語
- Ein は関数型 Go
  - WASM バックエンド実装中
