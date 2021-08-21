{
  pinnedNixpkgsCommit ? "6b940846e99cedd10eafbc301e467972ade1afec", # nixos-21.05
  pinnedNixpkgsUrl ? "https://github.com/NixOS/nixpkgs/archive/${pinnedNixpkgsCommit}.tar.gz",
  pkgs ? import (fetchTarball pinnedNixpkgsUrl) {},
}:
let
  hakyll-agda = pkgs.haskellPackages.callCabal2nix "hakyll-agda" (pkgs.fetchFromGitHub {
    owner = "bitonic";
    repo = "hakyll-agda";
    rev = "4493f84ed6240c5e310325bd0fe223cfd56b538b";
    sha256 = "15bwy5lbkpc1i9byzv9byhkwg952mbrk37wwn160i6xc9fibbbls";
  }) {};
  pandoc = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.dontHaddock (pkgs.haskell.lib.disableLibraryProfiling (pkgs.haskell.lib.disableExecutableProfiling
    (pkgs.haskellPackages.callCabal2nix "pandoc"  (pkgs.fetchFromGitHub {
      owner = "bitonic";
      repo = "pandoc";
      rev = "3b1ae36d1d82ec970a3c34b23c8ceb71c20fab36";
      sha256 = "0kl8flv5f9hyz377kpqajf2czzcbisqgavld71h057qjd563qqz3";
    }) {}))));
  website = pkgs.haskellPackages.callCabal2nix
    "website"
    (builtins.filterSource
      (path: type: builtins.elem
        (builtins.baseNameOf path)
        [ "website.cabal" "Setup.hs" "site.hs" "LICENSE" "KaTeXify.hs" ])
      ./.)
    {
      inherit hakyll-agda;
      inherit pandoc;
    };
  node = import ./node.nix { inherit pkgs; };
  yarn-shell = pkgs.mkShell {
    buildInputs = with pkgs; [
      yarn
    ];
  };
in {
  inherit website;
  website-shell = pkgs.mkShell {
    buildInputs = [ website ];
  };
  inherit yarn-shell;
}
