dotfiles
========

My common configuration for a Linux/macOS environments.
This is a nix flake that uses Home Manager to track things.

To install (after setting up nix with flakes):

```sh
$ nix run . -- switch --flake .#user@m1 --impure
```

Then restart the shell.
(hint: Use `build` instead of `switch` to test without installing.)

I use `--impure` so that `$USER`/`$HOME` can be accessed within the flake.


Complete setup steps for a new system:

```
sudo apt update
sudo apt install git ssh

# create SSH key
mkdir ~/.ssh
cd ~/.ssh
ssh-keygen -t ed25519 -C "user@hostname"
eval "$(ssh-agent -s)"
ssh-add ~/.ssh/id_ed25519

# install nix
cd ~
git clone git@github.com:williballenthin/dotfiles.git .dotfiles
cd .dotfiles/
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | \
  sh -s -- install --nix-build-group-id 4000000 --nix-build-user-id-base 4000000
. /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
mkdir -p ~/.config/nix
echo "experimental-features = nix-command flakes" > ~/.config/nix/nix.conf

home-manager switch --flake .#user@w --impure

cat << EOF >> ~/.bashrc
# use fish (usually)
# https://gist.github.com/voidptr/d77a5a527ed2cc06cd277df9ec366f32
if [[ \$(ps --no-header --pid=\$PPID --format=comm) != "fish" && -z \${BASH_EXECUTION_STRING} ]]
then
  shopt -q login_shell && LOGIN_OPTION='--login' || LOGIN_OPTION=''
  exec fish \$LOGIN_OPTION
fi
EOF

# or for zsh on macOS:
cat << EOF >> ~/.zshrc
# use fish (usually)
# https://gist.github.com/voidptr/d77a5a527ed2cc06cd277df9ec366f32
if [[ \$(ps -p \$PPID -o comm= | tr -d ' ') != "fish" && -z \${ZSH_EXECUTION_STRING} ]]
then
  if [[ -o login ]]; then LOGIN_OPTION='--login'; else LOGIN_OPTION=''; fi
  exec fish \$LOGIN_OPTION
fi
EOF
```

## other setup

  - `atuin import auto` (or `...fish`) to import existing history
  - `atuin login -u williballenthin`
  - `rustup install stable`
  - `rustup install nightly`
  - `rustup component add rust-analyzer`
  - `rustup component add rustc-codegen-cranelift-preview --toolchain nightly`
  - `npm install -g @anthropic-ai/claude-code`

