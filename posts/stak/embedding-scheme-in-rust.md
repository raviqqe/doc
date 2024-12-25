# Embedding Scheme in Rust

Rustはコンパイル系言語で動的にプログラムの動作を変更することが難しいです．この記事では[Stak Scheme][stak]というRustで書かれた小さなScheme処理系をRustのプログラムに組み込んで、Rustで書かれたプログラムの動作を動的に（プロセスを止めずに）変更します．

以下のコードはStak Schemeのレポジトリの[`examples/hot-reload`ディレクトリ](https://github.com/raviqqe/stak/tree/main/examples/hot-reload)内にあります．

# Schemeとは

[Scheme](https://www.scheme.org/)はLisp方言の一つで、[一級継続](https://ja.wikipedia.org/wiki/%E7%B6%99%E7%B6%9A)が言語機能として使えることが特徴です．コミュニティベースで仕様の策定が行われており、最新の仕様であるR7RS-smallは約90ページと言語仕様が比較的小さいです．

# Stak Schemeとは

[Stak Scheme][stak]は[Ribbit Scheme](https://github.com/udem-dlteam/ribbit)をフォークして作られた[R7RS標準](https://r7rs.org/)互換のScheme処理系で、以下の特徴があります．

- Rustプログラムの中に組み込めるRustで書かれたScheme処理系
- 小さなメモリフットプリント
- Capability-based security
  - Stak SchemeのインタプリタはデフォルトでOS等外部に対するAPIを扱えません．
  - I/OやファイルのAPIを有効化するには、それらをインタプリタの仮想マシン（VM）の初期化時に有効化する必要があります．
- 儂が書いた

# RustのプログラムにSchemeスクリプトを埋め込む

今回の例では、RustでHTTPサーバのプログラムを書き、その中にSchemeのスクリプトを組み込みます．

## クレートの初期化

初めに、以下のコマンドでHTTPサーバを作るためのバイナリクレートを初期化します．

```sh
cargo init http-server
cd http-server
```

## ライブラリの依存関係追加

Stak SchemeをライブラリとしてRustのクレートに追加するためには、以下のコマンドを実行します．

```sh
cargo add stak
cargo add --build stak-build
```

`stak`クレートはSchemeインタプリタをRustから呼ぶライブラリです．`stak-build`クレートはSchemeのスクリプトをRustのコードに埋め込めるように[`build.rs`ビルドスクリプト](https://doc.rust-lang.org/cargo/reference/build-scripts.html)（後述）の中でコンパイルするライブラリです．

## HTTPサーバの準備

次に、Rustで書かれたHTTPサーバを準備します．今回は非同期ランタイムであるTokio純正のHTTPライブラリ[`axum`](https://github.com/tokio-rs/axum)を使ってHTTPサーバを構築します．まず、以下のコマンドで依存関係を追加します．

```sh
cargo add --features rt-multi-thread tokio
cargo add axum
```

以下のコードを`src/main.rs`に追加します．

```rust
use axum::{routing::post, serve, Router};
use core::error::Error;

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    serve(
        tokio::net::TcpListener::bind("0.0.0.0:3000").await?,
        Router::new().route("/calculate", post("Hello, world!")),
    )
    .await?;

    Ok(())
}
```

`curl`コマンドでHTTPリクエストを送り、動作を確認します．

```sh
cargo run &
curl -f -X POST http://localhost:3000/calculate # -> Hello, world!
kill %1
```

## ビルドスクリプトの追加

Stak SchemeではSchemeスクリプトを`.scm`ファイル拡張子を付けて`src`ディレクトリ内に追加します．このとき、これらのスクリプトファイルがRustのプログラムに直接埋め込まれる訳ではなく、一度これらのファイルを[バイトコード](https://ja.wikipedia.org/wiki/%E3%83%90%E3%82%A4%E3%83%88%E3%82%B3%E3%83%BC%E3%83%89)に変換する必要があります．そのために、先述した`stak-build`クレートを使い、以下のコードを`build.rs`ファイルに追加します．

```rust
use stak_build::{build_r7rs, BuildError};

fn main() -> Result<(), BuildError> {
    build_r7rs()
}
```

これで`cargo build`実行時にSchemeファイルがバイトコードに変換され`target`ディレクトリ内に保存されます．

## Schemeスクリプトによるリクエストハンドラの作成

次に、Schemeのスクリプトを`src`ディレクトリに追加し、それをHTTPリクエストのハンドラとして使います．以下のコードを`src/handler.scm`ファイルに追加します．

```scheme
(import
  (scheme base)
  (scheme read)
  (scheme write))

(write (apply + (read)))
```

`read`はS式を標準入力からパースする関数、`write`は値を標準出力に書き出す関数です．`(apply + xs)`の式でリスト`xs`内の数値の和を計算します．

次に、Rustから上記のスクリプトを参照し実行します．以下のコードを`src/main.rs`ファイルに追加します．

```rust
// 他の`use`ステートメント...
use axum::{http::StatusCode, response};
use stak::{
    device::ReadWriteDevice,
    file::VoidFileSystem,
    include_module,
    module::{Module, UniversalModule},
    process_context::VoidProcessContext,
    r7rs::{SmallError, SmallPrimitiveSet},
    time::VoidClock,
    vm::Vm,
};

// `main`関数など...

// Scheme実行時のヒープサイズ
const HEAP_SIZE: usize = 1 << 16;

// Schemeスクリプトをインポートする．
// 実際には、Rustのプログラム内にバイトコードとして埋め込まれる．
static MODULE: UniversalModule = include_module!("handler.scm");

async fn calculate(input: String) -> response::Result<(StatusCode, String)> {
    // インメモリ標準出力と標準エラーのためのバッファの準備
    let mut output = vec![];
    let mut error = vec![];

    run_scheme(
        &MODULE.bytecode(),
        input.as_bytes(),
        &mut output,
        &mut error,
    )
    .map_err(|error| error.to_string())?;

    let error = decode_buffer(error)?;

    Ok(if error.is_empty() {
        (StatusCode::OK, decode_buffer(output)?)
    } else {
        (StatusCode::BAD_REQUEST, error)
    })
}

/// Schemeのプログラムを実行する．
fn run_scheme(
    bytecodes: &[u8],
    input: &[u8],
    output: &mut Vec<u8>,
    error: &mut Vec<u8>,
) -> Result<(), SmallError> {
    // Schemeのためのヒープメモリの初期化．この場合、Rust側ではスタック上に確保される．
    let mut heap = [Default::default(); HEAP_SIZE];
    // Schemeインタプリタの仮想マシン（VM）の初期化
    let mut vm = Vm::new(
        &mut heap,
        // R7RS標準準拠のプリミティブ関数の初期化
        SmallPrimitiveSet::new(
            ReadWriteDevice::new(input, output, error),
            // 標準入出力以外のプリミティブは必要ないので今回は無効化する．
            VoidFileSystem::new(),
            VoidProcessContext::new(),
            VoidClock::new(),
        ),
    )?;

    // VMをバイトコードで初期化する．
    vm.initialize(bytecodes.iter().copied())?;
    // バイトコードをVM上で実行する．
    vm.run()
}

/// 標準出力や標準エラーのバッファを文字列に変換する．
fn decode_buffer(buffer: Vec<u8>) -> response::Result<String> {
    Ok(String::from_utf8(buffer).map_err(|error| error.to_string())?)
}
```

また、`main`関数を以下のように変更します．

```diff_rust
  #[tokio::main]
  async fn main() -> Result<(), Box<dyn Error>> {
      serve(
          tokio::net::TcpListener::bind("0.0.0.0:3000").await?,
-         Router::new().route("/calculate", post("Hello, world!")),
+         Router::new().route("/calculate", post(calculate)),
      )
      .await?;

      Ok(())
  }
```

`curl`コマンドを使って動作を確認します．

```sh
cargo run &
curl -f -X POST --data '(1 2 3 4 5)' http://localhost:3000/calculate # -> 15
kill %1
```

Rustプログラムの中でSchemeのスクリプトが実行され、渡したリスト内の数値の和が計算されたことが確認できました．

## Hot module reloading

JavaScriptのバンドラ（WebpackやVite等）には、hot module reloadingという機能があります．これは変更されたソースファイルの内容を動作中のHTTPサーバ等に動的に反映させる機能です．

Stak Schemeにも同様の機能があります．それを用いてHTTPサーバの動作を動的に変更します．まず、`Cargo.toml`ファイルの中で`hot-reload`フィーチャを`stak`クレートに対して有効化します．

```toml
[dependencies]
stak = { version = "0.4.1", features = ["hot-reload"] }
```

次に、HTTPサーバを再起動します．

```sh
# 既に起動している場合、サーバのプロセスを止める．
cargo run &
```

試しに、`curl`コマンドを使って現在の動作を確認します．

```sh
curl -f -X POST --data '(1 2 3 4 5)' http://localhost:3000/calculate # -> 15
```

次に、先ほどの`handler.scm`ファイルの中で和を計算していたコードを積を計算するように変更します．

```diff_scheme
+ (write (apply + (read)))
- (write (apply * (read)))
```

**サーバを再起動せず**に、`cargo`コマンドを使ってSchemeスクリプトの再ビルドを行います．

```sh
cargo build
```

再び、`curl`コマンドを使って結果を確認します．

```sh
curl -f -X POST --data '(1 2 3 4 5)' http://localhost:3000/calculate # -> 720
```

先程と異なり、リスト内の値の積が返されたことが確認できました．

```
＿人人人人人人＿
＞　突然の積　＜
￣Y^Y^Y^Y^Y^￣
```

# まとめ

- Stak Schemeを使ってRustプログラムの動作を動的に変更しました
- Schemeはいいぞ

# 今後の展望

- RustとScheme間でのデータ型の相互運用性の改善
  - 現在は標準入出力しかRustとScheme間の通信方法がありません．
- より簡単なhot module reloadingの有効化方法
  - 自分で`cargo build`するの面倒ですね

# 謝辞

[yhara](https://github.com/yhara)さん、[monochrome](https://github.com/sisshiki1969)さん、プログラミング処理系Zulipコミュニティの方々にお世話になりました．ありがとうございました．

# 参考

- あまりメモリフットプリント・標準準拠等を気にしないのであれば、もっとリッチなRust製Scheme処理系もあります．
  - [mattwparas/steel](https://github.com/mattwparas/steel)
  - [volution/vonuvoli-scheme](https://github.com/volution/vonuvoli-scheme)
- Luaやmrubyも同様の用途によく使われます
  - [mlua-rs/mlua](https://github.com/mlua-rs/mlua)
- 少々目的が異なりますが、小さなWASMインタプリタと静的型付言語を含む適当な高級言語のWASMコンパイラを使えば似たようなことができると思います．ただ、自分でグルーコードを書く必要があります．
  - [wasmi-labs/wasmi](https://github.com/wasmi-labs/wasmi)

[stak]: https://github.com/raviqqe/stak
