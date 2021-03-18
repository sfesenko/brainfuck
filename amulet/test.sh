#!/usr/bin/bash

SRC="bf.ml"
DST="bf.lua"

if [[ ${SRC} -nt ${DST} ]]; then
  amc compile bf.ml -o bf.lua
fi

luajit ${DST} ../_samples/hello.bf

