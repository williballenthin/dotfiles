## User configuration via nix and home-manager


Once [nix and home-manager](http://ghedam.at/24353/tutorial-getting-started-with-home-manager-for-nix)
are installed, use the following steps.
Alternatively, check out the Dockerfile for an example.

```
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install

# via: https://nix-community.github.io/home-manager/#sec-install-standalone
nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
nix-channel --update

git clone git@github.com:williballenthin/dotfiles.git /home/user/.dotfiles
cd /home/user/.dotfiles

home-manager switch --flake .#user@sb2 --impure

# set default shell to fish
chsh --shell /home/user/.nix-profile/bin/fish
```

After switching a bunch of times, you can garbage collect via:

```
nix-collect-garbage -d
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
