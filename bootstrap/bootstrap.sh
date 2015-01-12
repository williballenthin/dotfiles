for PKG in "compton" "dmenu" "dunst" "dwbWrapper" "dzen2" \
    "getopt" "git" "htop" "i3lock" "libnotify" "rxvt_unicode_with-plugins" \
    "stow" "trayer" "vim" "vimPlugins.vundle" "wget" "xautolock" \
    "haskellPackages.xmonad" "haskellPackages.xmonadContrib" "haskellPackages.xmonadExtras" \
    "haskellPackages.xmobar" "subversion" "less" "tmux" "iotop" "tree" "man" \
; do
    nix-env -iA nixos.pkgs."$PKG";
done

if [ ! -d ~/.ssh ]; then mkdir ~/.ssh; fi
if [ ! -f ~/.ssh/id_rsa.pub ]; then
    pushd ~/.ssh;
      ssh-keygen -t rsa -C "willi.ballenthin@gmail.com";
      cat ~/.ssh/id_rsa.pub;
    popd;
fi

echo "";
echo "===================================";
echo "Next steps:";
echo "  - You'll probably want to \`stow\` each of the directories in .dotfiles";
echo "  - Then, run \`xmonad --recompile\`";
echo "  - Finally, find the path to the vundle installation, and update .vimrc"
