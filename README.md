# fihles - A simple file server written in Haskell

Requirements
------------
- Haskell Stack

Installation
------------
    git clone https://github.com/AlbinoBoi/fihles.git
    cd fihles
    stack build --copy-bins


- Add the flag "--copy-bins" to copy the binary to your "~/.local/bin" folder. This is necessary, if you want to run the file server from a different directory than the cloned project directory.

Usage
-----

    stack exec fihles <port>


The files in the current directory will then be served on Port <port> (or Port 8080 if none was specified).
