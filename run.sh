#!/bin/bash

# Check if main executable file exists. 
if [ ! -f ./build/main ]; then
    echo "Archivo build/main no encontrado. Compilar con build.sh."
    exit 1
fi

# Run main executable file.
./build/main
