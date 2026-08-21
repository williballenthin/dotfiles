{
  description = "virtual environments";

  inputs = {

    nixpkgs = {
      type = "github";
      owner = "nixos";
      repo = "nixpkgs";
      # nixos-25.11
      # ref = "871b9fd269ff6246794583ce4ee1031e1da71895";
      # nixos-26-05
      ref = "8c50a710ddca43d7a530fb805ad55bde8d0141c5";
    };
    devshell = {
      url = "github:numtide/devshell";
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
  };

  outputs = { self, flake-utils, devshell, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system: {
      devShell =
        let
        pkgs = import nixpkgs {
          inherit system;

          overlays = [ devshell.overlays.default ];
        };
        in
        pkgs.devshell.mkShell {
          imports = [ (pkgs.devshell.importTOML ./devshell.toml) ];
          env = [
            {
              name = "LD_LIBRARY_PATH";
              value = "${
                nixpkgs.lib.makeLibraryPath
                # extend library path here
                (with pkgs; [ stdenv.cc.cc openssl ])
              }:$LD_LIBRARY_PATH";
            }
          ];
        };
    });
}
