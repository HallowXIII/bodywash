{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    rhine-chat = {
      url = "github:ners/rhine-chat";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs:
    with builtins;
    let
      inherit (inputs.nixpkgs) lib;
      foreach = xs: f: with lib; foldr recursiveUpdate { } (
        if isList xs then map f xs
        else if isAttrs xs then mapAttrsToList f xs
        else throw "foreach: expected list or attrset but got ${typeOf xs}"
      );
      sourceFilter = root: with lib.fileset; toSource {
        inherit root;
        fileset = fileFilter (file: any file.hasExt [ "cabal" "hs" "hsc" "md" ]) root;
      };
      ghcsFor = pkgs: with lib; foldlAttrs
        (acc: name: hp:
          let
            version = getVersion hp.ghc;
            majorMinor = versions.majorMinor version;
            ghcName = "ghc${replaceStrings ["."] [""] majorMinor}";
            newAcc = acc // { ${ghcName} = hp; };
          in
          if ! hp ? ghc || hp ? ${ghcName} || versionOlder version "9.2"
          then acc
          else newAcc
        )
        { }
        pkgs.haskell.packages;
      hpsFor = pkgs: { default = pkgs.haskellPackages; } // ghcsFor pkgs;
      pname = "bodywhash";
      src = sourceFilter ./.;
      overlay = lib.composeManyExtensions [
        inputs.rhine-chat.overlays.default
        (final: prev: {
          haskell = prev.haskell // {
            packageOverrides = lib.composeManyExtensions [
              prev.haskell.packageOverrides
              (hfinal: hprev: {
                ${pname} = hfinal.callCabal2nix pname src { };
              })
            ];
          };
        })
      ];
    in
    foreach inputs.nixpkgs.legacyPackages (system: pkgs':
      let
        pkgs = pkgs'.extend overlay;
        hps = hpsFor pkgs;
      in
      {
        formatter.${system} = pkgs.nixpkgs-fmt;
        legacyPackages.${system} = pkgs;
        packages.${system} = {
          default = pkgs.haskellPackages.${pname};
        };
        devShells.${system} =
          foreach hps (ghcName: hp: {
            ${ghcName} = hp.shellFor {
              packages = ps: [ ps.${pname} ];
              nativeBuildInputs = with pkgs'; with haskellPackages; [
                cabal-install
                cargo
                fourmolu
                hp.haskell-language-server
              ];
            };
          });
      }
    );
}
