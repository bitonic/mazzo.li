{
  # pinnedNixpkgsCommit ? "6b940846e99cedd10eafbc301e467972ade1afec", # nixos-21.05
  # pinnedNixpkgsUrl ? "https://github.com/NixOS/nixpkgs/archive/${pinnedNixpkgsCommit}.tar.gz",
  # pkgs ? import (fetchTarball pinnedNixpkgsUrl) {},
  pkgs ? import <nixpkgs> {},
}:
let
  website = pkgs.haskellPackages.callCabal2nix
    "website"
    (builtins.filterSource
      (path: type: builtins.elem
        (builtins.baseNameOf path)
        [ "website.cabal" "Setup.hs" "site.hs" "LICENSE" "KaTeXify.hs" ])
      ./.)
    { };
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
