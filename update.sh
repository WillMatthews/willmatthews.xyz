#!/bin/bash


# update `site` executable with cabal
cabal update &&
cabal install --overwrite-policy=always &&
site watch -v

