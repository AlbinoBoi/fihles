# fihles - A simple file server written in Haskell

Requirements
------------
- Haskell Stack

Installation
------------
    git clone https://github.com/AlbinoBoi/fihles.git
    cd fihles
    stack build

Usage
-----

    stack exec fihles-exe <port>


The files in the current directory will then be served on Port <port> (or Port 8080 if none is specified).
