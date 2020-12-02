# wordlist
[![Build Status](https://travis-ci.com/kanatatsu64/wordlist.svg?branch=master)](https://travis-ci.com/github/kanatatsu64/wordlist)

[test locally](http://localhost:3000)

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
2. PluginIDを他のPluginと被らないように決め打ちし、pluginidとしてPluginデータに加える。

### Frontend

1. `client/src/Plugins/`下にPluginを作成する。
2. `client/src/Plugin.ts`の`loadModule`関数にPluginIDとPluginのimport文を追加する。

## 資料

- [URLリファレンス](https://github.com/kanatatsu64/wordlist/blob/master/URL.txt)
