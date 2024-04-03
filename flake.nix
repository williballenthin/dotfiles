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
        nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
        home-manager = {
            url = "github:nix-community/home-manager";
            inputs.nixpkgs.follows = "nixpkgs";
        };
        flake-utils.url = "github:numtide/flake-utils";
        helix.url = "github:helix-editor/helix/24.03";
        jj.url = "github:martinvonz/jj";
        atuin.url = "github:atuinsh/atuin";
    };

    outputs = {nixpkgs, home-manager, flake-utils, helix, jj, atuin, ...}:
        let 
            # TODO: make this configurable for m1
            system = "x86_64-linux";
            pkgs = nixpkgs.legacyPackages.${system};
        in
        {
            defaultPackage.${system} = home-manager.defaultPackage.${system};

            # > The homeConfigurations is a custom flake output, 
            # > which is not recognized by Nix flakes, so when we try to display it,
            # > it shows “unknown” as a description but this will probably be supported in the future.
            #
            # https://gvolpe.com/blog/nix-flakes/
            homeConfigurations.williballenthin = home-manager.lib.homeManagerConfiguration {
                inherit pkgs;

                # Usually this would look like:
                #
                #   modules = [ ./home.nix ];
                #
                # but we're going to keep this inline for simplicity.
                modules = [
                    # Flake forwarding module.
                    #
                    # Expose flake inputs to our inline module below
                    # https://discourse.nixos.org/t/flakes-impure-error-installing-package-from-local-file-system/22185/9
                    ({...}: {
                        _module.args = {
                            inherit helix;
                            inherit jj;
                            inherit atuin;
                        };
                    })

                    # Here's an inline module, receiving the normal set,
                    # as well as the extra flake inputs provided via
                    # _module.args above.
                    ({pkgs, helix, ...}: {
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
                            pkgs.jq
                            pkgs.ranger
                            pkgs.ripgrep
                            pkgs.tig
                            pkgs.tmux
                            pkgs.visidata
                            # include late breaking patch for fish integration
                            # prefer: pkgs.atuin
                            pkgs.atuin
                            # atuin.packages.${pkgs.system}.atuin

                            #############################################
                            # development
                            pkgs.hyperfine
                            pkgs.cmake
                            pkgs.gnumake
                            pkgs.iosevka
                            pkgs.pkg-config
                            # rapid, ongoing development
                            jj.packages.${pkgs.system}.jujutsu

                            #############################################
                            # editors
                            pkgs.neovim
                            pkgs.emacs29
                            # rapid, ongoing development
                            helix.packages.${pkgs.system}.helix

                            #--------------------------------------------
                            # editors->emacs
                            # for building emacs->vterm
                            pkgs.libtool
                            # for emacs->clipboard
                            pkgs.wl-clipboard

                            #--------------------------------------------
                            # editors->LSP
                            pkgs.nodePackages.typescript-language-server
                            pkgs.nodePackages.pyright
                            pkgs.python311Packages.python-lsp-server
                            pkgs.python311Packages.pylsp-mypy
                            pkgs.python311Packages.python-lsp-ruff

                            #############################################
                            # rust
                            #
                            # let rust manage itself,
                            # since we'll want to use vs code tools, etc.
                            pkgs.rustup
                            pkgs.bacon
                        ];

                        # fast, persistent nix integration with direnv
                        # https://github.com/nix-community/nix-direnv
                        programs.direnv.enable = true;
                        programs.direnv.nix-direnv.enable = true;

                        home.file.".gitconfig".source = ./git/.gitconfig;
                        home.file.".tmux.conf".source = ./tmux/.tmux.conf;
                        home.file.".config/starship.toml".source = ./.config/starship.toml;
                        home.file.".config/helix/config.toml".source = ./.config/helix/config.toml;
                        home.file.".config/helix/languages.toml".source = ./.config/helix/languages.toml;
                        home.file.".config/fish/config.fish".source = ./.config/fish/config.fish;
                        home.file.".config/fish/functions/fzf.fish".source = ./.config/fish/functions/fzf.fish;

                        # i think this file is no longer used by fisher,
                        # but it contains the list of plugins i'd like to use.
                        # you may have to do `fisher install foo` for each line in the file.
                        home.file.".config/fish/fish_plugins".source = ./.config/fish/fish_plugins;

                        home.file.".config/fish/completions/fisher.fish".source = pkgs.fetchFromGitHub {
                          owner = "jorgebucaran";
                          repo = "fisher";
                          # 4.4.3
                          rev = "36810b39401536650d7a1018c8f3832f51741950";
                          #hash = "sha256-TR01V4Ol7zAj+3hvBj23PGSNjH+EHTcOQSKtA5uneGE=";
                          hash = "sha256-q9Yi6ZlHNFnPN05RpO+u4B5wNR1O3JGIn2AJ3AEl4xs=";
                        } + "/completions/fisher.fish";
                        home.file.".config/fish/functions/fisher.fish".source = pkgs.fetchFromGitHub {
                          owner = "jorgebucaran";
                          repo = "fisher";
                          # 4.4.3
                          rev = "36810b39401536650d7a1018c8f3832f51741950";
                          #hash = "sha256-TR01V4Ol7zAj+3hvBj23PGSNjH+EHTcOQSKtA5uneGE=";
                          hash = "sha256-q9Yi6ZlHNFnPN05RpO+u4B5wNR1O3JGIn2AJ3AEl4xs=";
                        } + "/functions/fisher.fish";
                    })
                ];
            };
        };
}
