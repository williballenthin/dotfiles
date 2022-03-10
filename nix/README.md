## User configuration via nix and home-manager


Once nix and home-manager are installed, use the following steps.
Alternatively, check out the Dockerfile for an example.

```
git clone git@github.com:williballenthin/dotfiles.git /home/user/.dotfiles

# replace home.nix with the one in this repo
rm ~/.config/nixpkgs/home.nix
ln -s /home/user/.dotfiles/nix/home.nix ~/.config/nixpkgs/home.nix

# deploy the changes
home-manager switch

# initial setup of neovim plugins
nvim, :PlugInstall, :PlugStatus

# set default shell to fish
chsh --shell /home/user/.nix-profile/bin/fish
```
