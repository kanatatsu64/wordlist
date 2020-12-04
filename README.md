# wordlist
[![Build Status](https://travis-ci.com/kanatatsu64/wordlist.svg?branch=master)](https://travis-ci.com/github/kanatatsu64/wordlist)

[test locally](http://localhost:3000)

SQLite3に依存しているため、事前にインストールが必要（後述）。

## プログラムの実行方法

### Frontend

```shell
cd client
yarn run build
```

### Backend + WebFramework

```shell
stack build
stack exec wordlist-exe
```

### Browser

`http://localhost:3000`にアクセスする。

## テストの実行方法

### Frontend
未実装

### Backend

```shell
stack test
```

### Travis script

```shell
cd scripts
python3 test-helper.py
```

## Pluginの追加方法

### Backend

1. `src/Plugins/`下にPluginを作成する。
1. PluginIDを他のPluginと被らないように決め打ちし、pluginidとしてPluginデータに加える。

### Frontend

1. `client/src/Plugins/`下にPluginを作成する。
1. `client/src/Plugin.ts`の`loadModule`関数にPluginIDとPluginのimport文を追加する。

## SQLite3のインストール

### Windowsの場合

1. [公式ページ](https://www.sqlite.org/download.html)から`sqlite-amalgamation`と`sqlite-dll`を`C:\sqlite`等のフォルダにダウンロードする。
1. `sqlite-dll`は`C:\Windows\System32`にも追加する。
1. `stack path --stack-root`を実行して、stackのルートパスを調べる。
1. stackのルートパスにある`config.yaml`に以下を追記する。

```yaml:config.yaml
# Extra directories used by stack for building
extra-include-dirs:
- C:\sqlite
extra-lib-dirs:
- C:\sqlite
```

## 資料

- [URLリファレンス](https://github.com/kanatatsu64/wordlist/blob/master/URL.txt)
