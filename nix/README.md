## User configuration via nix and home-manager


Once [nix and home-manager](http://ghedam.at/24353/tutorial-getting-started-with-home-manager-for-nix)
are installed, use the following steps.
Alternatively, check out the Dockerfile for an example.

```
nix-channel --add https://nixos.org/channels/nixos-23.05 nixos-23.05
nix-channel --add https://nixos.org/channels/nixos-22.11 nixos-22.11
nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs-unstable

git clone git@github.com:williballenthin/dotfiles.git /home/user/.dotfiles

# replace home.nix with the one in this repo
rm ~/.config/nixpkgs/home.nix
ln -s /home/user/.dotfiles/nix/home.nix ~/.config/nixpkgs/home.nix

# deploy the changes
home-manager switch

# initial setup of neovim plugins
nvim +:PlugInstall +qa
nvim +:TSUpdate +qa

# set default shell to fish
chsh --shell /home/user/.nix-profile/bin/fish
```


## flakes

enable flakes:

```
mkdir ~/.config/nix
echo "experimental-features = nix-command flakes" >> ~/.config/nix/nix.conf
```

create flake with direnv:

```
nix flake new -t github:nix-community/nix-direnv .
```

or with [devshell](https://github.com/numtide/devshell/blob/master/docs/getting_started.md):

```
nix flake new -t "github:numtide/devshell" .
```

or put the devshell in a central repository:

```
nix flake new -t "github:numtide/devshell" ~/projects/foo
```

and make the `.envrc` look like this:

```
#!/usr/bin/env bash
watch_file ~/projects/foo/devshell.toml
use flake ~/projects/foo

# and if you want to use python (need packages.python311 in the devshell.toml):
# layout python
```
