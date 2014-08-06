let pkgs = import <nixpkgs> {};
    haskellPackages = pkgs.haskellPackages.override {
      extension = self: super: {
        HaRe = self.callPackage ./. {};
        haskellTokenUtils = self.callPackage ../haskell-token-utils/. {};
      };
    };
 in pkgs.lib.overrideDerivation haskellPackages.HaRe (attrs: {
   buildInputs = [ haskellPackages.cabalInstall ] ++ attrs.buildInputs;
 })


