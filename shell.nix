let
  sources = {
    nixpkgs = fetchTarball https://api.github.com/repos/NixOS/nixpkgs/tarball/nixos-23.05;
  };
  pkgs = import sources.nixpkgs {
    config = { };
    overlays = [ ];
    system = builtins.currentSystem;
  };
  haskellPackages = pkgs.haskellPackages;
in
{
  shell = pkgs.mkShellNoCC {
    buildInputs = with haskellPackages; [
      ghc
      cabal-install
      which
    ];

    shellHook = ''
      export PS1="[\u@\h \W]\$ "
    '';
  };
}
