# How to compile gc2fsa to WASM

```
git clone https://gitlab.haskell.org/ghc/ghc-wasm-meta.git
docker run --rm -ti -v $(pwd)/ghc-wasm-meta:/ghc-wasm-meta -v $(pwd)/chorgram:/chorgram debian:11 bash
apt update && apt install unzip jq curl build-essential xz-utils
cd /ghc-wasm-meta
FLAVOUR=9.6 ./setup.sh
source /root/.ghc-wasm/env
wasm32-wasi-cabal install --lib hxt base text containers filepath array
cd /chorgram/stable_chorgram/
wasm32-wasi-ghc gc2fsa.hs -o gc2fsa.wasm
```

To run the compiled WASM file, simply:

```
wasmtime --dir=. gc2fsa.wasm -- pingpong.gc
```

# Export function

I've also tried (WIP) exporting specific function.

Problem: need to specify FFI interface to use String ([Char]).

```
wasm32-wasi-ghc -v gc2fsa.hs -o gc2fsa2.wasm -no-hs-main -optl-mexec-model=reactor -optl-Wl,--export=hs_init,--export=gc2fsaexportable
```

