{
  description = "Emacs package";

  inputs = {
    eldev.flake = false;
    eldev.url = "github:doublep/eldev/1.3.1";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    std.inputs.nixpkgs.follows = "nixpkgs";
    std.url = "github:divnix/std";

    epkg-abbrev-hook.inputs.eldev.follows = "eldev";
    epkg-abbrev-hook.inputs.emacs-overlay.follows = "emacs-overlay";
    epkg-abbrev-hook.inputs.nixpkgs.follows = "nixpkgs";
    epkg-abbrev-hook.inputs.std.follows = "std";
    epkg-abbrev-hook.url = "github:xFA25E/abbrev-hook";
  };

  outputs = {
    std,
    self,
    ...
  } @ inputs:
    std.growOn {
      inherit inputs;
      cellsFrom = ./nix;
      cellBlocks = with std.blockTypes; [
        (functions "lib")
        (installables "packages")
        (installables "devshells")
      ];
    }
    {
      devShells = std.harvest self ["automation" "devshells"];
      packages = std.harvest self ["main" "packages"];
      checks = std.harvest self [["automation" "packages"] ["main" "packages"]];
    };
}
