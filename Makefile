

.PHONY: cli_utils
cli_utils:
	sudo apt-get -y install ssh openssh-server sshfs git vim python-software-properties software-properties-common zsh make build-essential tig wget screen ranger cmus dwb weechat-curses htop


.PHONY: remove_cli_utils
remove_cli_utils:
	sudo apt-get -y remove ssh openssh-server sshfs git vim python-software-properties software-properties-common zsh make build-essential tig wget dwb weechat-curses htop


.PHONY: oh_my_zsh
oh_my_zsh: cli_utils
	chsh $(USER) -s "/bin/zsh"
	wget --no-check-certificate https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O - | bash
	sed -i 's/ZSH_THEME="robbyrussell"/ZSH_THEME="gallifrey"/g' ~/.zshrc  # TODO: willitheme


.PHONY: remove_oh_my_zsh
remove_oh_my_zsh:
	uninstall_oh_my_zsh


.PHONY: screen
screen: cli_utils
	if [ ! -h ~/.screenrc ]; then ln -s "$$(pwd)"/warehouse/.screenrc-18568 ~/.screenrc; fi


.PHONY: remove_screen
remove_screen:
	if [ -h ~/.screenrc ]; then unlink ~/.screenrc; fi


.PHONY: vim
vim: cli_utils
	if [ ! -h ~/.vimrc ]; then ln -s "$$(pwd)"/warehouse/.vimrc ~/.vimrc; fi
	mkdir -p ~/.vim/autoload
	mkdir -p ~/.vim/bundle
	wget --no-check-certificate -O ~/.vim/autoload/pathogen.vim https://raw.github.com/tpope/vim-pathogen/master/autoload/pathogen.vim

.PHONY: remove_vim
remove_vim:
	if [ -h ~/.vimrc ]; then unlink ~/.vimrc; fi
	rm -rf ~/.vim/autoload
	rm -rf ~/.vim/bundle


.PHONY: python
python:
	sudo apt-get -y install python-dev python-pip
	sudo pip install virtualenv


.PHONY: remove_python
remove_python:
	-sudo pip uninstall virtualenv
	sudo apt-get -y remove python-dev python-pip


.PHONY: pycheckers
pycheckers: python
	sudo pip install pep8 pyflakes
	sudo cp "$$(pwd)"/warehouse/pycheckers.py-113 /opt/pycheckers.py
	if [ ! -h /usr/local/bin/pycheckers ]; then sudo ln -s /opt/pycheckers.py /usr/local/bin/pycheckers; fi


.PHONY: remove_pycheckers
remove_pycheckers:
	if [ -h /usr/local/bin/pycheckers ]; then sudo unlink /usr/local/bin/pycheckers; fi
	sudo rm -f /opt/pycheckers.py
	-sudo pip uninstall pep8 pyflakes


# note, this doesn't have a cleanup recipe
# TODO(wb): make the default ~/.software, but also read from env variable
.PHONY: software_dir
software_dir:
	if [ ! -d ~/.software ]; then mkdir ~/.software; fi


# TODO(wb): where should the local PATH be?
.PHONY: pyp
pyp: software_dir
	wget http://pyp.googlecode.com/files/pyp_beta -O ~/.software/pyp
	sudo ln -s ~/.software/pyp /usr/local/bin/pyp


.PHONY: remove_pyp
remove_pyp:
	if [ -h /usr/local/bin/pyp ]; then unlink /usr/local/bin/pyp; fi
	rm -f ~/.software/pyp


.PHONY: x11
x11:
	sudo apt-get -y install xorg libx11-dev libxft-dev libxrandr-dev libxinerama-dev


.PHONY: remove_x11
remove_x11:
	sudo apt-get -y remove xorg libx11-dev libxft-dev libxrandr-dev libxinerama-dev


.PHONY: dmenu
dmenu: software_dir cli_utils x11
	if [ ! -d ~/.software/dmenu-4.5 ]; then                   \
    cd ~/.software                                        &&\
      wget http://dl.suckless.org/tools/dmenu-4.5.tar.gz &&\
      tar xf dmenu-4.5.tar.gz                            &&\
      cd dmenu-4.5                                       &&\
      sudo make clean install; fi


.PHONY: remove_demu
remove_dmenu:
	if [ -d ~/.software/dmenu-4.5 ]; then \
    cd ~/.software/dmenu-4.5          &&\
      sudo make uninstall clean      &&\
      cd ~/.software                  &&\
      rm -r dmenu-4.5; fi
  # TODO(wb): technically we need `make` here


.PHONY: dzen
dzen: software_dir cli_utils x11
	if [ ! -d ~/.software/dzen ]; then                \
    cd ~/.software                                &&\
      git clone https://github.com/robm/dzen.git &&\
      cd dzen                                    &&\
      sudo make clean install; fi


.PHONY: remove_dzen
remove_dzen:
	if [ -d ~/.software/dzen ]; then \
    cd ~/.software/dzen          &&\
      sudo make uninstall clean &&\
      cd ~/.software             &&\
      sudo rm -r dzen; fi            # need sudo for some git files
  # TODO(wb): technically we need `make` here


.PHONY: xmonad
xmonad: cli_utils x11
	sudo apt-get -y install ghc cabal-install
	if ! grep "__CABAL_PATH__" ~/.zshrc; then                            \
    echo "PATH=${PATH}:~/.cabal/bin  # __CABAL_PATH__" >> ~/.zshrc; fi
	cabal update
	cabal install --user xmonad
	cabal install --user xmonad-contrib --flags="-use_xft"
	cabal install --user xmobar
	if [ ! -d ~/.xmonad ]; then mkdir ~/.xmonad; fi
	if [ ! -h ~/.xmonad/xmonad.hs ]; then ln -s "$$(pwd)"/warehouse/xmonad.hs ~/.xmonad/xmonad.hs; fi
	~/.cabal/bin/xmonad --recompile
  # TODO(wb): need to modify terminal if on ARM


.PHONY: remove_xmonad
remove_xmonad:
	if [ -h ~/.xmonad/xmonad.hs ]; then unlink ~/.xmonad/xmonad.hs; fi
	if [ -d ~/.xmonad ]; then rm -r ~/.xmonad; fi
#	cabal remove --user xmonad xmonad-contrib xmobar  # cabal remove isn't a command.
	sed -i -e "s/.*__CABAL_PATH__.*//g" ~/.zshrc
	sudo apt-get -y remove ghc cabal-install


.PHONY: xmodmap
xmodmap:
	if [ ! -h ~/.Xmodmap ]; then ln -s $$(pwd)/warehouse/.Xmodmap-31980 ~/.Xmodmap; fi


.PHONY: remove_xmodmap
remove_xmodmap:
	if [ -h ~/.Xmodmap ]; then unlink ~/.Xmodmap; fi


.PHONY: xresources
xresources:
	if [ ! -h ~/.Xresources ]; then ln -s $$(pwd)/warehouse/.Xresources ~/.Xresources; fi


.PHONY: remove_xresources
remove_xresources:
	if [ -h ~/.Xresources ]; then unlink ~/.Xresources; fi


.PHONY: compton
compton: cli_utils
	sudo apt-add-repository ppa:richardgv/compton
	sudo apt-get update
	sudo apt-get -y install compton
	if [ ! -h ~/.compton.conf ]; then ln -s "$$(pwd)"/warehouse/.compton.conf ~/.compton.conf; fi


.PHONY: remove_compton
remove_compton:
	if [ -h ~/.compton.conf ]; then unlink ~/.compton.conf; fi
	sudo apt-get -y remove compton
	sudo apt-add-repository --remove ppa:richardgv/compton


.PHONY: fetch_wallpaper
fetch_wallpaper:
	if [ ! -d ~/.wallpaper ]; then mkdir ~/.wallpaper; fi
	wget http://s.imgur.com/a/7883i/zip -O ~/.wallpaper/archive.zip
	cd ~/.wallpaper && unzip archive.zip


.PHONY: remove_fetch_wallpaper
remove_fetch_wallpaper:
	rm -f ~/.wallpaper


.PHONY: wallpaper
wallpaper: fetch_wallpaper
	sudo apt-get -y install feh
	if [ ! -h ~/.wallpaper/wallpaper.sh ]; then ln -s $$(pwd)/warehouse/wallpaper.sh ~/.wallpaper/wallpaper.sh; fi


.PHONY: remove_wallpaper
remove_wallpaper:
	if [ -h ~/.wallpaper/wallpaper.sh ]; then unlink ~/.wallpaper/wallpaper.sh; fi
	sudo apt-get -y remove feh


.PHONY: gtk_appearance
gtk_appearance:
	if [ ! -h ~/.themes ]; then ln -s $$(pwd)/warehouse/.themes ~/.themes; fi
	if [ ! -h ~/.gtkrc-2.0 ]; then ln -s $$(pwd)/warehouse/.gtkrc-2.0 ~/.gtkrc-2.0; fi


.PHONY: remove_gtk_appearance
remove_gtk_appearance:
	if [ -h ~/.themes ]; then unlink ~/.themes; fi
	if [ -h ~/.gtkrc-2.0 ]; then unlink ~/.gtkrc-2.0; fi


.PHONY: xinitrc
xinitrc:
	if [ ! -h ~/.xmonad/start-xmonad.sh ]; then ln -s $$(pwd)/warehouse/start-xmonad.sh ~/.xmonad/start-xmonad.sh; fi
	echo "bash /.xmonad/start-xmonad.sh" > ~/.xinitrc


.PHONY: remove_xinitrc
remove_xinitrc:
	rm -f ~/.xinitrc
	rm -f ~/.xmonad/start-xmonad.sh


.PHONY: xmonad_desktop
xmonad_desktop:
	if [ ! -h ~/.xmonad/start-xmonad.sh ]; then ln -s $$(pwd)/warehouse/start-xmonad.sh ~/.xmonad/start-xmonad.sh; fi
	sudo ln -s $$(pwd)/warehouse/xmonad.desktop /usr/share/xsessions/xmonad.desktop


.PHONY: remove_xmonad_desktop
remove_xmonad_desktop:
	rm -f ~/.xmonad/start-xmonad.sh
	if [ -h /usr/share/xsessions/xmonad.desktop ]; then sudo unlink /usr/share/xsessions/xmonad.desktop; fi


.PHONY: intel_desktop_tools
intel_desktop_tools:
	sudo apt-get -y install rxvt-unicode


.PHONY: remove_intel_desktop_tools
remove_intel_desktop_tools:
	sudo apt-get -y remove rxvt-unicode


.PHONY: all_intel_software
all_intel_software: cli_utils oh_my_zsh screen pycheckers software_dir x11 dmenu dzen xmonad xmodmap xresources compton python intel_desktop_tools


.PHONY: install_ubuntu_server_intel
install_ubuntu_server_intel: all_intel_software xinitrc


.PHONY: all_arm_software  # Ubuntu ARM doesn't have a Compton PPA package
all_arm_software: cli_utils oh_my_zsh screen pycheckers software_dir x11 dmenu dzen xmonad xmodmap xresources python


.PHONY: remove_all
remove_all: remove_screen remove_pycheckers remove_x11 remove_dmenu remove_dzen remove_xmonad remove_xmodmap remove_xresources remove_compton remove_python remove_intel_desktop_tools remove_cli_utils
	sudo apt-get -y autoremove
