#!/bin/sh

if [ "$#" != 1 ]; then
    echo "This script checks that the current implementation of nix-bikeshed is"
    echo "an AST-preserving transformation on a whole directory hierarchy of"
    echo ".nix files (eg. nixpkgs), as parsed by hnix."
    echo ""
    echo "Please call it with the path to the directory to check as argument:"
    echo "    $0 /nix/var/nix/profiles/per-user/root/channels/nixos/nixpkgs/"
    exit 1
fi

dir="$1"

echo "Checking the built nix-bikeshed (hope you didn't forget to run"
echo "‘stack --nix build’ before) against all files in ‘$dir’"

find "$1" -type f -name '*.nix' | while read file; do
    if ! stack --nix exec -- nix-bikeshed --check "$file" > /dev/null; then
        echo "Failed to check ‘$file’"
    fi
done
