# 1. install nix
# 2. install home-manager
# 3. checkout williballenthin/dotfiles
# 4. symlink home.nix to ~/.config/nixpkgs/home.nix
# 5. home-manager switch
# 6. set shell to: /home/user/.nix-profile/bin/fish
{ config, lib, pkgs, ... }:

let
  inherit (pkgs) stdenv;
  inherit (lib) mkIf;
  pkgs2111 = import (builtins.fetchTarball https://github.com/NixOS/nixpkgs/archive/nixos-21.11.tar.gz) { inherit config; };
in                                         
{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "user";
  home.homeDirectory = "/home/user";

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
    pkgs.gnupg
    pkgs.neovim
    pkgs.watch
    pkgs.ripgrep
    pkgs.fd
    pkgs.fzf
    pkgs.ranger
    pkgs.broot
    pkgs.visidata
    pkgs.jless
    pkgs.direnv
    pkgs.tig
    pkgs.gitui
    pkgs.bat
    pkgs.exa
    pkgs.hexyl
    pkgs.gron
    pkgs.delta
    pkgs.dua
    pkgs.fish
    pkgs.less
    pkgs.htop
    pkgs.hyperfine
    # let rust manage itself
    # since we'll want to use vs code tools, etc.
    pkgs.rustup
  ]
  ++ lib.optionals stdenv.isDarwin [
    # via https://github.com/NixOS/nixpkgs/issues/160876
    # starship is broken in unstable
    # so we use an old snapshot from 21.11
    pkgs2111.starship
  ]
  ++ lib.optionals stdenv.isLinux [
    pkgs.starship

    # for nvim/treesitter compilation
    # darwin already has clang??
    pkgs.gcc11
    pkgs.libstdcxx5
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

  home.file.".gitconfig".source = ../git/.gitconfig;
  home.file.".tmux.conf".source = ../tmux/.tmux.conf;
  home.file.".config/starship.toml".source = ../.config/starship.toml;
  home.file.".config/fish/config.fish".source = ../.config/fish/config.fish;
  home.file.".config/fish/fish_plugins".source = ../.config/fish/fish_plugins;

  home.file.".config/fish/completions/fisher.fish".source = pkgs.fetchFromGitHub {
    owner = "jorgebucaran";
    repo = "fisher";
    rev = "93dafd242b52a0dc6bea54130d0ea041830c7fd6";
    hash = "sha256-TR01V4Ol7zAj+3hvBj23PGSNjH+EHTcOQSKtA5uneGE=";
  } + "/completions/fisher.fish";
  home.file.".config/fish/functions/fisher.fish".source = pkgs.fetchFromGitHub {
    owner = "jorgebucaran";
    repo = "fisher";
    rev = "93dafd242b52a0dc6bea54130d0ea041830c7fd6";
    hash = "sha256-TR01V4Ol7zAj+3hvBj23PGSNjH+EHTcOQSKtA5uneGE=";
  } + "/functions/fisher.fish";

  # fast, persistent nix integration with direnv
  # https://github.com/nix-community/nix-direnv
  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;
}
