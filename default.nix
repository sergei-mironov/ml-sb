{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc822"
}:

let

  inherit (nixpkgs) pkgs;
  inherit (pkgs) stdenv;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  pkg = haskellPackages.mkDerivation {
    pname = "ml-sb";
    version = "0.0.0";
    src = ./.;
    isLibrary = false;
    isExecutable = true;
    executableHaskellDepends = with haskellPackages; [
      base parsec pretty-show
      recursion-schemes Earley
     ];
    libraryHaskellDepends = with haskellPackages; [
      cabal-install ghc zlib haskdogs hasktags
    ];
    description = "Minimal ML language to learn the Defunctionalization algorithm";
    license = stdenv.lib.licenses.gpl3;

    shellHook = ''
      cabal() {( `which cabal` --ghc-options=-freverse-errors "$@" ; )}
    '';
  };

in
  if pkgs.lib.inNixShell then pkg.env else pkg
