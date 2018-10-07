let
  fetch   = (import <nixpkgs> {}).fetchFromGitHub;
  nixpkgs = import <nixpkgs> {};
  overrides = self: super: {
    ghcid' = nixpkgs.haskell.lib.overrideSrc super.ghcid {
      src = fetch {
        owner  = "ndmitchell";
        repo   = "ghcid";
        rev    = "9773482fd83bf0bc528bbcb0ad121191a8fd809b";
        sha256 = "18wi0ghxl9kszivyf50aixpbq7srrdncawk7x1fzxhyimm89qqqq";
      };
      version = "0.6.11";
    };
  };
in nixpkgs.mkShell {
  buildInputs = [ ((nixpkgs.haskell.packages.ghc843.override {overrides = overrides;}).ghcWithPackages (p: [ p.ghcid' ])) ];
}
