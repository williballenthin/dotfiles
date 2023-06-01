# 1. install nix
# 2. install home-manager
# 3. checkout williballenthin/dotfiles
# 4. symlink home.nix to ~/.config/nixpkgs/home.nix
# 5. home-manager switch
# 6. set shell to: /home/user/.nix-profile/bin/fish
{ config, lib, ... }:

let
  pkgsUnstable = import <nixpkgs-unstable> {};
  pkgs2305 = import <nixos-23.05> {};
  pkgs2211 = import <nixos-22.11> {};
  pkgs2111 = import (builtins.fetchTarball https://github.com/NixOS/nixpkgs/archive/nixos-21.11.tar.gz) { inherit config; };

  # update here for dist-upgrade
  pkgs = pkgs2305;
  inherit (pkgs) stdenv;
  inherit (lib) mkIf;
in                                         
{
  home.username = (builtins.getEnv "USER");
  home.homeDirectory = (builtins.getEnv "HOME");

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.05";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home.packages = [
    pkgs.tmux
    pkgs.openssh
    pkgs.git
    pkgs.git-lfs
    pkgs.rlwrap
    pkgs.jq
    pkgs.gnupg
    pkgs.neovim
    pkgs.emacs
    pkgs.watch
    pkgs.ripgrep
    pkgs.fd
    pkgs.fzf
    pkgs.ranger
    pkgs.broot
    pkgs.visidata
    pkgs.jless
    pkgs.tig
    pkgs.gitui
    pkgs.bat
    pkgs.exa
    pkgs.hexyl
    pkgs.ent
    pkgs.gron
    pkgs.delta
    pkgs.dua
    pkgs.nethogs
    pkgs.fish
    pkgs.less
    pkgs.htop
    pkgs.hyperfine
    pkgs.asciinema
    pkgsUnstable.ov
    # direnv enabled via hm programs below
    #pkgs.direnv
    # let rust manage itself
    # since we'll want to use vs code tools, etc.
    pkgs.rustup
    #pkgs.rust-analyzer
    pkgs.bacon
    # broken right now
    # pkgs.rustracer
    # for spacemacs
    pkgs.source-code-pro
  ]
  ++ lib.optionals stdenv.isDarwin [
    # via https://github.com/NixOS/nixpkgs/issues/160876
    # starship is broken in unstable
    # so we use an old snapshot from 21.11
    pkgs2111.starship
  ]
  ++ lib.optionals stdenv.isLinux [
    pkgs.starship

    pkgs.docker-compose
    pkgs.sanoid

    # for nvim/treesitter compilation
    # darwin already has clang??
    pkgs.gcc11
  ];

  # we don't have vim installed here,
  # but using during development of home.nix.
  # installed via apt
  home.file.".vim/bundle/Vundle.vim".source = pkgs.fetchFromGitHub {
    owner = "VundleVim";
    repo = "Vundle.vim";
    rev = "b255382d6242d7ea3877bf059d2934125e0c4d95";
    hash = "sha256-hufBZwTL3BT1S2iOOb1TXTW2/Vvp52i6afu5gRmddTo=";
  };
  home.file.".vimrc".source = ../vim/.vimrc;

  home.file.".local/share/nvim/site/autoload/plug.vim".source = pkgs.fetchFromGitHub {
    owner = "junegunn";
    repo = "vim-plug";
    rev = "e300178a0e2fb04b56de8957281837f13ecf0b27";
    sha256 = "0bfgadn31n516x0m0kr88jk9x79rl6zllnwij759wpazmw1p0xg8";
  } + "/plug.vim";

  xdg.configFile."nvim/init.vim".source = ../.config/nvim/init.vim;

# spacemacs expects to be able to write to its files,
# so having nix own the files from the repo doesn't work (today).
# so just checkout the directory manually:
#
#     git clone -b develop https://github.com/syl20bnr/spacemacs ~/.emacs.d
#
# otherwise, we'd use this:
#
#  home.file.".emacs.d" = {
#    # don't make the directory read only so that impure melpa can still happen for now.
#    # see: https://discourse.nixos.org/t/home-manager-spacemacs/8033/2
#    recursive = true;
#    source = pkgs.fetchFromGitHub {
#      owner = "syl20bnr";
#      repo = "spacemacs";
#      rev = "5c0650282fe09f852ecd4109bf2a6bc9cb5a950b";
#      sha256 = "sha256-xKSjb88YL5fByieMnaLscLRVfyV22fdzxRyHSNj3J9g=";
#    };
#  };
  home.file.".spacemacs".source = ../emacs/.spacemacs;

  home.file.".gitconfig".source = ../git/.gitconfig;
  home.file.".tmux.conf".source = ../tmux/.tmux.conf;
  home.file.".config/starship.toml".source = ../.config/starship.toml;
  home.file.".config/fish/config.fish".source = ../.config/fish/config.fish;
  home.file.".config/fish/functions/fzf.fish".source = ../.config/fish/functions/fzf.fish;

  # i think this file is no longer used by fisher,
  # but it contains the list of plugins i'd like to use.
  # you may have to do `fisher install foo` for each line in the file.
  home.file.".config/fish/fish_plugins".source = ../.config/fish/fish_plugins;

  home.file.".config/fish/completions/fisher.fish".source = pkgs.fetchFromGitHub {
    owner = "jorgebucaran";
    repo = "fisher";
    # 4.4.3
    rev = "36810b39401536650d7a1018c8f3832f51741950";
    hash = "sha256-TR01V4Ol7zAj+3hvBj23PGSNjH+EHTcOQSKtA5uneGE=";
  } + "/completions/fisher.fish";
  home.file.".config/fish/functions/fisher.fish".source = pkgs.fetchFromGitHub {
    owner = "jorgebucaran";
    repo = "fisher";
    # 4.4.3
    rev = "36810b39401536650d7a1018c8f3832f51741950";
    hash = "sha256-TR01V4Ol7zAj+3hvBj23PGSNjH+EHTcOQSKtA5uneGE=";
  } + "/functions/fisher.fish";

  # fast, persistent nix integration with direnv
  # https://github.com/nix-community/nix-direnv
  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;
}
