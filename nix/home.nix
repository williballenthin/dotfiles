# 1. install nix
# 2. install home-manager
# 3. checkout williballenthin/dotfiles
# 4. symlink home.nix to ~/.config/nixpkgs/home.nix
# 5. home-manager switch
# 6. set shell to: /home/user/.nix-profile/bin/fish
{ config, lib, ... }:

let
  pkgs2311 = import <nixos-23.11> { };
  # you can reference these below if needing to use an old version, pinned version, etc.
  #pkgsUnstable = import <nixpkgs-unstable> {};
  #pkgs2305 = import <nixos-23.05> { };
  #pkgs2211 = import <nixos-22.11> { };
  #pkgs2111 = import (builtins.fetchTarball https://github.com/NixOS/nixpkgs/archive/nixos-21.11.tar.gz) { inherit config; };

  # update here for dist-upgrade
  pkgs = pkgs2311;
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
    # expect underlying system to provide:
    #   - ssh
    #   - git
    # which is reasonable, since these dotfiles come from a github repo.

    pkgs.tmux
    pkgs.unzip
    pkgs.git-lfs
    pkgs.zstd
    pkgs.rlwrap
    pkgs.jq
    pkgs.gnupg
    pkgs.neovim
    pkgs.emacs29
    # for building emacs->vterm
    pkgs.libtool
    # for emacs->clipboard
    pkgs.wl-clipboard
    # for emacs->pyright LSP
    pkgs.pyright
    # for emacs->ts-lsp
    pkgs.nodePackages.typescript-language-server
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
    pkgs.eza
    pkgs.hexyl
    pkgs.ent
    pkgs.gron
    pkgs.delta
    pkgs.dua
    pkgs.fish
    pkgs.less
    pkgs.htop
    pkgs.hyperfine
    pkgs.starship
    # direnv enabled via hm programs below
    #pkgs.direnv
    # let rust manage itself
    # since we'll want to use vs code tools, etc.
    pkgs.rustup
    pkgs.bacon
    # broken right now
    # pkgs.rustracer

    # install jj via: cargo install jj-cli@0.15.1 --locked
    # which requires system packages:
    #   sudo apt-get install libssl-dev openssl pkg-config build-essential
    # nix 23.11 has jj@0.13.0, unstable has jj@0.14.0
  ]
  ++ [
    # for lancelot dev in vscode remote
    pkgs.gnumake
    pkgs.cmake
    pkgs.pkg-config
    pkgs.iosevka
  ]
  ++ lib.optionals stdenv.isDarwin [
  ]
  ++ lib.optionals stdenv.isLinux [
    pkgs.nethogs

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

  home.file.".emacs.d/early-init.el".source = ../emacs/early-init.el;
  home.file.".emacs.d/init.el".source = ../emacs/init.el;

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

  # fast, persistent nix integration with direnv
  # https://github.com/nix-community/nix-direnv
  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;
}
