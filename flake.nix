{
  description = "A flake for installing my xmonad build";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.11";
  
  outputs = { self, nixpkgs, ... }: {
    defaultPackage.x86_64-linux =
      with import nixpkgs { system = "x86_64-linux"; };
      pkgs.haskellPackages.callCabal2nix "xmonad-tanish2002" self { };
  };
}
