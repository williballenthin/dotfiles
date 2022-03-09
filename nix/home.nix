{ config, pkgs, ... }:

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
    pkgs.git
    pkgs.neovim

    # for nvim/treesitter compilation
    pkgs.gcc11
    pkgs.libstdcxx5
  ];

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
}
