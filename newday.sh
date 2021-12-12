#!/bin/sh

mkdir -p "$1"
wget -O "$1/input.txt" "https://adventofcode.com/2021/day/$1/input"
cp template.idr "$1/Main.idr"
cp template.ipkg "$1/solution.ipkg"

