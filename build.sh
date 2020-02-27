#!/bin/bash

# Create build direrctories
mkdir -p build/obj
# Compile files into build directory
ghc -o build/main src/main.hs -odir build/obj -hidir build/obj
