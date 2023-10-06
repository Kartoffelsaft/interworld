#!/bin/sh


TIC=../../makes/TIC-80/build/bin/tic80
CC=emcc
LN=wasm-ld

mkdir -p .build

set -e

$CC -Os main.c -c -o .build/main.o -sMAIN_MODULE -mbulk-memory -ffast-math -fno-exceptions -fno-rtti
$LN .build/main.o /usr/share/wasi-sysroot/lib/wasm32-wasi/libm.a -o cart.wasm --no-entry --import-memory --initial-memory=262144 --max-memory=262144 --global-base=98304

$TIC --fs . --cmd 'load cart.tic & import binary cart.wasm & run'
