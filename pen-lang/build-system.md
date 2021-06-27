# Ninja で作る自作（？）ビルドシステム

---

# 自己紹介

- 名前：とやま ようた
- GitHub, Twitter: [raviqqe](https://github.com/raviqqe)
- 宗教: Vim

---

# ビルドシステムとは

- ソースコードから効率よくバイナリを生成するためのソフトウェア
- Task-based か Artifact-based へ
  - Make -> Bazel
- 言語ごとに異なる
  - C: Make, CMake, ...
  - Rust: Cargo
  - Go: `go build`

---

## 前に作ったビルドシステム

- Ein では手書きだった
  1. 全てのモジュールをスキャン
  1. 依存関係グラフを生成
     - 相互再帰チェック
- 新しい言語 Pen を作った
  - 外部のビルドシステムを利用したい

---

# Ninja

- https://ninja-build.org/
- シンプルな Make
  - "Where other build systems are high-level languages Ninja aims to be an assembler."
- Make に比べて速い
  - Chrome や LLVM に使われているらしい
  - CMake のバックエンドのひとつ

---

### Ninja (続)

- 元々 C や C++ のプロジェクトのために作られた
  - モジュールの実装 (.c ファイル) とインターフェース (.h ファイル)が別の言語
- 最近の言語だと実装とインターフェースが同じファイルに書かれる
  - Dynamic dependencies という機能が実装された
  - ビルド結果によって依存関係をその場で変えられる

---

# Ninja を使ったビルドシステム

1. 全てのパッケージをダウンロード
   - 最初の一回のみ
1. 全てのパッケージ内の全てのモジュールをスキャン
1. それらをビルドする Ninja スクリプトを生成
1. Ninja を走らせる

---

# 依存関係の解決

- 生成されたスクリプトにはモジュール間の依存関係は書かれていない
- 各モジュールに対して .dd ファイルをコンパイルし動的に依存関係を Ninja に伝える
  - 各モジュールのオジェクトファイルが依存するモジュールのインターフェースファイルのリスト

---

# 実装

- https://github.com/pen-lang/pen/blob/main/lib/infra/src/ninja_module_build_script_compiler.rs
- https://github.com/pen-lang/pen/blob/main/lib/infra/src/ninja_build_script_dependency_compiler.rs

---

#　おまけ

- バグを踏んだ
- https://github.com/ninja-build/ninja/issues/1988
