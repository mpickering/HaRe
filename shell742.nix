let pkgs = import <nixpkgs> {};
    haskellPackages = pkgs.haskellPackages_ghc742.override {
      extension = self: super: {
        HaRe = self.callPackage ./. {};
        haskellTokenUtils = self.callPackage ../haskell-token-utils/. {};
      };
    };
 in pkgs.lib.overrideDerivation haskellPackages.HaRe (attrs: {
   buildInputs = [ haskellPackages.cabalInstall ] ++ attrs.buildInputs;
 })


