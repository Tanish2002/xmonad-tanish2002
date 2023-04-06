{
  description = "A flake for installing my xmonad build";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.05";

  outputs = { self, nixpkgs, ... }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      my-xmonad =
        pkgs.haskellPackages.callCabal2nix "xmonad-tanish2002" self { };
    in {
      packages.x86_64-linux.default = my-xmonad.overrideAttrs (o: {
        buildInputs = o.buildInputs ++ [ pkgs.makeWrapper ];

        postFixup = ''
          wrapProgram $out/bin/xmonad \
            --prefix PATH : ${nixpkgs.lib.makeBinPath [ pkgs.pstree ]}
        '';
      });
      devShell.x86_64-linux = pkgs.haskellPackages.shellFor {
        packages = p: [ self.packages.x86_64-linux.default ];
        buildInputs = with pkgs; [
          haskell-language-server
          ghcid
          cabal-install
        ];
      };
    };
}
