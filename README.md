# Usage

`./trickle announce --tracker 'http://bt2.t-ru.org/ann' --info_hash 4b606b47b2d30d4f57ef3fb4e07906ed64685306 --ip 104.28.212.77 --port 15520`

# Build

```
export http_proxy=http://127.0.0.1:8080
export https_proxy=http://127.0.0.1:8080
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

```
source .ghcup/env
git clone https://github.com/kindaro/trickle
cd trickle
cabal update
cabal build -j1
```
