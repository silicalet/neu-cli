{ pkgs, ... }:

{
  languages.haskell = {
    enable = true;
    package = pkgs.ghc;
    cabal.enable = true;
    lsp.enable = true;
    stack.enable = false;
  };

  packages = [
    pkgs.git
    pkgs.haskellPackages.fourmolu
    pkgs.openssl # RSA interoperability tests
    pkgs.python3 # Offline CLI / HTTP integration tests (standard library only)
  ];

  scripts.neu-fmt.exec = ''
    find app src test -name '*.hs' -type f -exec fourmolu --mode inplace {} +
  '';

  scripts.neu-check.exec = ''
    set -eu
    cabal build all
    cabal run neu -- --help
    cabal run neu -- --version
    cabal test all --test-show-details=direct
    python3 test/cli.py "$(cabal list-bin exe:neu)"
    find app src test -name '*.hs' -type f -exec fourmolu --mode check {} +
  '';

  enterTest = ''
    neu-check
  '';
}
