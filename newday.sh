#!/bin/sh

mkdir -p "$1"
touch "$1/input.txt"
cp template.idr "$1/main.idr"

