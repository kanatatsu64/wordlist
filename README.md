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

### Frontent
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
