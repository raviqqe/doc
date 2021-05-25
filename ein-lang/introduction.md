# Ein 言語の紹介

---

# 自己紹介

- 名前：とやま ようた
- GitHub, Twitter: [raviqqe](https://github.com/raviqqe)
- プログラミング言語が好き
- 普段は Web 系エンジニア

---

# 好きな言語は何ですか？

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

# Ein 言語

- 関数型 Go

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

---

# Go との違い

- 不変な値
- カリー化
- インターフェース -> ユニオン型

---

---

# まとめ

- Go はいい言語
  - 簡潔
- Ein は関数型 Go
