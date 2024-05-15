{
    description = "williballenthin home-manager flake";

    # references:
    #  - https://discourse.nixos.org/t/home-manager-22-11-homemanagerconfiguration-pkgs-parameter/23948
    #  - https://github.com/nix-community/home-manager/issues/3075#issuecomment-1593969080
    #  - https://blog.nobbz.dev/2022-12-12-getting-inputs-to-modules-in-a-flake/
    #  - https://discourse.nixos.org/t/flakes-impure-error-installing-package-from-local-file-system/22185/9
    #  - https://discourse.nixos.org/t/improving-a-flake-nix-config-that-configures-home-manager/23389/2

    inputs = {
        # nixpkgs.url = "github:nixos/nixpkgs/23.05";
        # have to track nixos-unstable since this is what home-manager dev's against
        nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
        home-manager = {
            url = "github:nix-community/home-manager";
            inputs.nixpkgs.follows = "nixpkgs";
        };
        flake-utils.url = "github:numtide/flake-utils";
    };

  outputs = { nixpkgs, home-manager, flake-utils, ... } @ inputs:
    let
      # mkHomeConfig via: https://github.com/nix-community/home-manager/issues/3075#issuecomment-1593969080
      mkHomeConfig = system: home-manager.lib.homeManagerConfiguration {
        pkgs = import nixpkgs {
          inherit system;
        };

        modules = [
          # Flake forwarding module.
          #
          # Expose flake inputs to our inline module below
          # https://discourse.nixos.org/t/flakes-impure-error-installing-package-from-local-file-system/22185/9
          ({...}: {
              _module.args = {
                  # inherit foo
              };
          })

          # Here's an inline module, receiving the normal set,
          # as well as the extra flake inputs provided via
          # _module.args above.
          ({pkgs, ...}: {
              # home.username = "user";
              # home.homeDirectory = "/home/user";
              #
              # when we're in a flake, need to use --impure to access env vars
              home.username = (builtins.getEnv "USER");
              home.homeDirectory = (builtins.getEnv "HOME");

              home.stateVersion = "23.11";
              programs.home-manager.enable = true;

              home.packages = [
                  # expect underlying system to provide:
                  #   - ssh
                  #   - git
                  # which is reasonable, since these dotfiles come from a github repo.

                  #############################################
                  # shell and PS1
                  pkgs.fish
                  pkgs.starship
                  pkgs.atuin

                  #############################################
                  # basic utilities
                  pkgs.ent
                  pkgs.git-lfs
                  pkgs.gnupg
                  pkgs.htop
                  pkgs.less
                  pkgs.rlwrap
                  pkgs.unzip
                  pkgs.watch
                  pkgs.zstd

                  #############################################
                  # extended utilities
                  pkgs.bat
                  pkgs.broot
                  pkgs.delta
                  pkgs.dua
                  pkgs.eza
                  pkgs.fd
                  pkgs.fzf
                  pkgs.gitui
                  pkgs.gron
                  pkgs.hexyl
                  pkgs.jless
                  pkgs.jujutsu
                  pkgs.jq
                  pkgs.ranger
                  pkgs.ripgrep
                  pkgs.tig
                  pkgs.tmux
                  pkgs.visidata
                  pkgs.zoxide

                  #############################################
                  # development

                  #--------------------------------------------
                  # editors
                  pkgs.helix

                  #--------------------------------------------
                  # rust
                  #
                  # let rust manage itself,
                  # since we'll want to use vs code tools, etc.
                  #
                  #   rustup install stable
                  #   rustup install nightly
                  #   rustup component add rust-analyzer
                  #   rustup component add rustc-codegen-cranelift-preview --toolchain nightly
                  pkgs.rustup
              ];

              programs.direnv.enable = true;
              programs.direnv.nix-direnv.enable = true;

              home.file.".gitconfig".source = ./git/.gitconfig;
              home.file.".tmux.conf".source = ./tmux/.tmux.conf;
              home.file.".config/starship.toml".source = ./.config/starship.toml;
              home.file.".config/helix/config.toml".source = ./.config/helix/config.toml;
              home.file.".config/helix/languages.toml".source = ./.config/helix/languages.toml;
              home.file.".config/fish/config.fish".source = ./.config/fish/config.fish;
              home.file.".config/fish/functions/fzf.fish".source = ./.config/fish/functions/fzf.fish;
              home.file.".config/atuin/config.toml".source = ./.config/atuin/config.toml;
              home.file.".config/bat/config".source = ./.config/bat/config;
          })
        ];
      };
    in {
      homeConfigurations."user@m1" = mkHomeConfig "aarch64-darwin";
      homeConfigurations."user@sb2" = mkHomeConfig "x86_64-linux";
    };
}
