# Stak Schemeの紹介

Shibuya.lisp, 2024年3月28日

---

# 自己紹介

- raviqqe ([GitHub](https://github.com/raviqqe), [Bluesky](https://bsky.app/profile/raviqqe.bsky.social))
- 普段はRustバックエンドエンジニア
- プログラミング言語が好き

![](https://raviqqe.com/icon.svg)

---

# 概要

- Schemeとは
- Stak Schemeとは
  - Ribbit Schemeとは
  - 実装
- 今後の予定

---

# Stak Schemeとは

- 自分が作っているScheme処理系
- SchemeとRustで書かれている
- R7RS準拠が目標
- Ribbit Schemeが元

---

# Stak Schemeとは

## 実装されている機能

- `(scheme base)`, `(scheme read)`, `(scheme write)`の大体の手続き
- 継続 (`call-with-current-continuation`)
- 例外 (`raise`, `guard`)
- マクロ (`define-syntax`, `syntax-rules`)
- ライブラリシステム (`define-library`, `import`, `export`)

---

# Ribbit Schemeとは

- カナダMontreal大学で作られたScheme処理系
- R4RS準拠
- **小さい**
  - 7KBメモリフットプリント
- VMを色々な言語で実装
  - C, JavaScript, Python, x86アセンブリ等

---

# Stak Schemeの実装

## Ribbit Schemeと同じところ

- Schemeで書かれたバイトコードコンパイラ
- Rustで書かれた仮想マシン
- 全てがリスト
  - オブジェクトに加え、バイトコードや内部スタックも

---

# デモ

> TBD

---

# 今後の予定

- `eval`の実装
- ファイル操作
- コンパイラやVMの性能改善
- Rustとの相互運用
  - RustとScheme間で値を共有

---

# まとめ

- Stak Scheme自作処理系
  - Schemeで書かれたコンパイラ
  - Rustで書かれたVM
  - R7RS準拠が目標
- Scheme処理系作るのは楽しい
