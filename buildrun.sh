#!/bin/sh


TIC=../../makes/TIC-80/build/bin/tic80
CC="zig cc --target=wasm32-wasi"
LN=wasm-ld

mkdir -p .build

set -e

$CC -Os main.c -c -o .build/main.o -mbulk-memory -ffast-math -fno-exceptions -fno-rtti -D BUILD_FOR_TIC
$LN .build/main.o -o cart.wasm --no-entry --import-memory --initial-memory=262144 --max-memory=262144 --global-base=98304 #/usr/share/wasi-sysroot/lib/wasm32-wasi/libm.a

$TIC --fs . --cmd 'load cart.tic & import binary cart.wasm & run'
