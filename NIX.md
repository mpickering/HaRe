# Building this repo using nixpkgs

# Install nixpkgs (first time)

As a normal user

    $ bash <(curl https://nixos.org/nix/install)

This will (if you let it) install a startup to your ~/.bashrc

Alternatively activate nix when required by

    source ~/.nix-profile/etc/profile.d/nix.sh

# Prerequisite

Make sure that haskell-token-utils is checked out at the same level as
this one, and on the wip branch.

    cd ..
    git clone https://github.com/alanz/haskell-token-utils.git
    cd haskell-token-utils
    git checkout -b wip

# Developing against a specific compiler

    nix-shell shell742.nix  # GHC 7.4.2
    nix-shell shell63.nix  # GHC 7.6.3
    nix-shell shell.nix  # GHC 7.8.3 (or later)

Each of these will put you in a shell with all the required
dependencies in it.

You can then do the standard

    cabal clean && cabal configure --enable-tests && cabal build

