dotfiles
========

My common configuration for a Linux/macOS environments.
This is a nix flake that uses Home Manager to track things.

To install (after setting up nix with flakes):

```sh
$ nix run . -- switch --flake .#user@m1 --impure  # and/or
$ home-manager switch --flake .#user@m1 --impure
```

Then restart the shell.
(hint: Use `build` instead of `switch` to test without installing.)

I use `--impure` so that `$USER`/`$HOME` can be accessed within the flake.

## other setup

  - `atuin import auto` (or `...fish`) to import existing history
  - `atuin login -u williballenthin`
  - `rustup install stable`
  - `rustup install nightly`
  - `rustup component add rust-analyzer`
  - `rustup component add rustc-codegen-cranelift-preview --toolchain nightly`

