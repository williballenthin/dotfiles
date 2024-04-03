dotfiles
========

Local configuration files for various Linux tools

This is a nix flake that uses Home Manager to track things.

# install

```sh
$ nix run . -- switch --flake .#"williballenthin" --impure
```

Then restart the shell.
(hint: Use `build` instead of `switch` to test without installing.)

Other steps:

  - `fisher update`
  - `atuin import auto` (or `...fish`) to import existing history
  - `atuin login -u williballenthin`

