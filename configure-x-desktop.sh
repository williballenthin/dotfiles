sudo apt-get update
sudo apt-get install xorg ghc libx11-dev cabal-install libxft-dev libxrandr-dev make libxinerama-dev git rvxt-unicode python-software-properties software-properties-common vim zsh openssh-server
chsh "${USER}" -s "/bin/zsh"
wget --no-check-certificate https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O - | sh
sed -i 's/ZSH_THEME="robbyrussell"/ZSH_THEME="gallifrey"/g' ~/.zshrc
echo "PATH=${PATH}:~/.cabal/bin" >> ~/.zshrc
cabal update
cabal install --user xmonad
cabal install --user xmonad-contrib --flags="-use_xft"
cabal install --user xmobar

mkdir ~/software
cd software
wget http://dl.suckless.org/tools/dmenu-4.5.tar.gz
tar xf dmenu-4.5.tar.gz
cd dmenu-4.5
sudo make clean install
cd ~

cd ~/software
git clone https://github.com/robm/dzen.git
cd dzen
sudo make clean install

mkdir ~/.xmonad
cd ~/.xmonad
wget https://raw.github.com/williballenthin/dotfiles/master/warehouse/xmonad.hs-21338 -O xmonad.hs

cd ~
~/.cabal/bin/xmonad --recompile

cd ~
wget https://github.com/williballenthin/dotfiles/blob/master/warehouse/Xdefaults -O .Xdefaults

cd ~
wget https://raw.github.com/williballenthin/dotfiles/master/warehouse/.screenrc-18568 -O .screenrc

cd ~
sudo apt-add-repository ppa:richardgv/compton
sudo apt-get update
sudo apt-get install compton
wget https://raw.github.com/williballenthin/dotfiles/master/warehouse/compton.conf -O .compton.conf

cd ~
wget https://raw.github.com/williballenthin/dotfiles/blob/master/warehouse/.Xmodmap-31980 -O .Xmodmap
echo "xsetroot -cursor_name left_ptr" >> ~/.xinitrc
echo "xmodmap ~/.Xmodmap" >> ~/.xinitrc
echo "compton -b" >> ~/.xinitrc
echo "exec xmonad" >> ~/.xinitrc
