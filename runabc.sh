#!/bin/bash

exitstatus=0
if [ "$#" -eq 0 ]; then
    exit 1;
else
    if [[ $1 == -* ]]; then
        filepath=$(realpath "$2")
    else
        filepath=$(realpath "$1")
    fi
    filedir=$(dirname "$filepath")
    filenoext="${filepath%.*}"
    opts=""
    if [[ "$*" == *"--lex"* ]]; then
        opts="-l"
    fi
    if [[ "$*" == *"--parse"* ]]; then
        opts="-p"
    fi
    if [[ "$*" == *"--validate"* ]]; then
        opts="-s"
    fi
    if [[ "$*" == *"--tacky"* ]]; then
        opts="-t"
    fi
    if [[ "$*" == *"--codegen"* ]]; then
        opts="-c"
    fi
    if [[ "$*" == *"-S"* ]]; then
        opts="-S"
    fi
    gcc -E -P "$filepath" -o "$filenoext.i" && ~/.cabal/bin/abc "$filenoext.i" $opts
    exitstatus=$?
    rm "$filenoext.i"
    if [ $exitstatus -eq 0 ] && [[ $opts == "" ]]; then
        gcc "$filenoext.s" -o "$filenoext"
        rm "$filenoext.s"
    fi
    exit $exitstatus
fi