

.PHONY: cli_utils
cli_utils:
	sudo apt-get -y install ssh openssh-server sshfs git vim python-software-properties software-properties-common zsh make build-essential tig wget screen ranger cmus weechat-curses htop subversion jq iotop nethogs ack-grep
	git config --global alias.lg "log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%C(bold blue)<%an>%Creset' --abbrev-commit"
	sudo dpkg-divert --local --divert /usr/bin/ack --rename --add /usr/bin/ack-grep 


.PHONY: oh_my_zsh
oh_my_zsh: cli_utils
	chsh $(USER) -s "/bin/zsh"
	wget --no-check-certificate https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O - | bash
	sed -i 's/ZSH_THEME="robbyrussell"/ZSH_THEME="gallifrey"/g' ~/.zshrc  # TODO: willitheme


.PHONY: screen
screen: cli_utils
	if [ ! -h ~/.screenrc ]; then ln -s "$$(pwd)"/warehouse/.screenrc-18568 ~/.screenrc; fi


.PHONY: vim
vim: cli_utils
	if [ ! -h ~/.vimrc ]; then ln -s "$$(pwd)"/warehouse/.vimrc ~/.vimrc; fi
	mkdir -p ~/.vim/autoload
	mkdir -p ~/.vim/bundle
	git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
	vim +BundleInstall +qall
	sudo apt-get -y install cmake libclang1-3.4
	cd ~/.vim/bundle/YouCompleteMe && \
		./install.sh


.PHONY: python
python:
	sudo apt-get -y install python-dev python-pip
	sudo pip install virtualenv flake8



.PHONY: pycheckers
pycheckers: python
	sudo pip install pep8 pyflakes
	sudo cp "$$(pwd)"/warehouse/pycheckers.py-113 /opt/pycheckers.py
	if [ ! -h /usr/local/bin/pycheckers ]; then sudo ln -s /opt/pycheckers.py /usr/local/bin/pycheckers; fi


.PHONY: ruby
ruby:
	sudo apt-get -y install rubygems


.PHONY: timetrap
timetrap: ruby
	sudo apt-get -y install libsqlite3-dev
	sudo gem install timetrap


.PHONY: work_stuff
work_stuff: timetrap
	sudo apt-get -y install libreoffice-impress libreoffice-writer


.PHONY: lua
lua: vim
	sudo apt-get install lua5.1 luarocks
	sudo luarocks install luafilesystem
	sudo luarocks install moonscript
	sudo luarocks install LuaSocket
	cd ~/.vim/bundle && git clone git://github.com/leafo/moonscript-vim.git


# note, this doesn't have a cleanup recipe
# TODO(wb): make the default ~/.software, but also read from env variable
.PHONY: software_dir
software_dir:
	if [ ! -d ~/.software ]; then mkdir ~/.software; fi


# TODO(wb): where should the local PATH be?
.PHONY: pyp
pyp: software_dir
	wget http://pyp.googlecode.com/files/pyp_beta -O ~/.software/pyp
	chmod +x ~/.software/pyp
	sudo ln -s ~/.software/pyp /usr/local/bin/pyp


.PHONY: golang
golang: software_dir
	sudo apt-get -y install mercurial
	cd ~/.software && \
		hg clone -u release https://code.google.com/p/go && \
		cd go/src && \
		./all.bash && \
		if ! grep "__PATH_GOLANG__" ~/.zshrc; then \
			echo 'PATH=$$PATH:~/.software/go/bin/  # __PATH_GOLANG__' >> ~/.zshrc; fi
	# GOPATH NOT SET
	# echo 'export GOPATH=/data/data/go  # __GOPATH__' >> ~/.zshrc
	# go get -u github.com/nsf/gocode
	# echo 'PATH=$$PATH:$$GOPATH/bin/  # __GOLANG_PATH_BIN__' >> ~/.zshrc;


.PHONY: truecrypt
truecrypt: cli_utils software_dir
	wget http://www.truecrypt.org/download/truecrypt-7.1a-linux-x64.tar.gz ~/.software/truecrypt-7.1a-linux-x64.tar.gz
	cd ~/.software && \
		tar xf truecrypt-7.1a-linux-x64.tar.gz && \
		sudo ./truecrypt-7.1a-setup-x64


.PHONY: x11
x11:
	sudo apt-get -y install xorg libx11-dev libxft-dev libxrandr-dev libxinerama-dev


.PHONY: dmenu
dmenu: software_dir cli_utils x11
	if [ ! -d ~/.software/dmenu-4.5 ]; then                   \
    cd ~/.software                                        &&\
      wget http://dl.suckless.org/tools/dmenu-4.5.tar.gz &&\
      tar xf dmenu-4.5.tar.gz                            &&\
      cd dmenu-4.5                                       &&\
      sudo make clean install; fi


.PHONY: dzen
dzen: software_dir cli_utils x11
	if [ ! -d ~/.software/dzen ]; then                \
    cd ~/.software                                &&\
      git clone https://github.com/robm/dzen.git &&\
      cd dzen                                    &&\
      sudo make clean install; fi


.PHONY: trayer
trayer:
	sudo apt-get -y install trayer


.PHONY: dunst
dunst: software_dir cli_utils x11
	sudo apt-get install libdbus-1-dev libxinerama-dev libxft-dev libxss-dev libxdg-basedir-dev libglib2.0-dev libpango1.0-dev
	cd ~/.software && \
      git clone git://github.com/knopwob/dunst && \
      cd dunst && \
      make && \
      sudo make install
	if [ ! -h ~/.dunstrc ]; then \
      ln -s $$(pwd)/warehouse/dunstrc ~/.dunstrc; fi


.PHONY: localpath
localpath:
	if ! grep "__LOCAL_PATH__" ~/.zshrc; then \
		echo 'PATH=$$PATH:~/.path  # __LOCAL_PATH__' >> ~/.zshrc; fi


.PHONY: xmonad
xmonad: cli_utils x11
	sudo apt-get -y install ghc cabal-install
	if ! grep "__CABAL_PATH__" ~/.zshrc; then                            \
    echo 'PATH=$$PATH:~/.cabal/bin  # __CABAL_PATH__' >> ~/.zshrc; fi
	cabal update
	cabal install --user xmonad
	cabal install --user xmonad-contrib --flags="-use_xft"
	cabal install --user xmobar
	if [ ! -d ~/.xmonad ]; then mkdir ~/.xmonad; fi
	if [ ! -h ~/.xmonad/xmonad.hs ]; then ln -s "$$(pwd)"/warehouse/xmonad.hs ~/.xmonad/xmonad.hs; fi
	~/.cabal/bin/xmonad --recompile
  # TODO(wb): need to modify terminal if on ARM


.PHONY: xmodmap
xmodmap:
	if [ ! -h ~/.Xmodmap ]; then ln -s $$(pwd)/warehouse/.Xmodmap-31980 ~/.Xmodmap; fi


.PHONY: xresources
xresources:
	if [ ! -h ~/.Xresources ]; then ln -s $$(pwd)/warehouse/.Xresources ~/.Xresources; fi


.PHONY: compton
compton: cli_utils
	sudo apt-add-repository ppa:richardgv/compton
	sudo apt-get update
	sudo apt-get -y install compton
	if [ ! -h ~/.compton.conf ]; then ln -s "$$(pwd)"/warehouse/.compton.conf ~/.compton.conf; fi


.PHONY: fetch_wallpaper
fetch_wallpaper:
	if [ ! -d ~/.wallpaper ]; then mkdir ~/.wallpaper; fi
	wget http://s.imgur.com/a/7883i/zip -O ~/.wallpaper/archive.zip
	cd ~/.wallpaper && unzip archive.zip
	cd ~/.wallpaper && for i in $$(seq 1 158); do I=$$(printf "%%03d" "$$i"); echo "$$I"; wget "http://www.squidfingers.com/_patterns/files/pattern_"$$I".zip"; unzip "pattern_"$$I".zip"; rm "pattern_"$$I".zip"; done && rm -r __MACOSX;


.PHONY: wallpaper
wallpaper:
	sudo apt-get -y install feh
	if [ ! -h ~/.wallpaper/wallpaper.sh ]; then ln -s $$(pwd)/warehouse/wallpaper.sh ~/.wallpaper/wallpaper.sh; fi


.PHONY: gtk_appearance
gtk_appearance:
	if [ ! -h ~/.themes ]; then ln -s $$(pwd)/warehouse/.themes ~/.themes; fi
	if [ ! -h ~/.gtkrc-2.0 ]; then ln -s $$(pwd)/warehouse/.gtkrc-2.0 ~/.gtkrc-2.0; fi


.PHONY: dwb
dwb:
	sudo apt-get -y install dwb


.PHONY: xinitrc
xinitrc:
	if [ ! -h ~/.xmonad/start-xmonad.sh ]; then ln -s $$(pwd)/warehouse/start-xmonad.sh ~/.xmonad/start-xmonad.sh; fi
	echo "bash /.xmonad/start-xmonad.sh" > ~/.xinitrc


.PHONY: xmonad_desktop
xmonad_desktop:
	if [ ! -h ~/.xmonad/start-xmonad.sh ]; then ln -s $$(pwd)/warehouse/start-xmonad.sh ~/.xmonad/start-xmonad.sh; fi
	sudo ln -s $$(pwd)/warehouse/xmonad.desktop /usr/share/xsessions/xmonad.desktop
	if [ ! -h ~/.xmonad/bar.sh ]; then ln -s $$(pwd)/warehouse/bar.sh ~/.xmonad/bar.sh; fi


.PHONY: intel_desktop_tools
intel_desktop_tools:
	sudo apt-get -y install rxvt-unicode


.PHONY: all_intel_software
all_intel_software: cli_utils oh_my_zsh screen pycheckers software_dir x11 dmenu dzen xmonad xmodmap xresources compton python intel_desktop_tools trayer


.PHONY: install_ubuntu_server_gui_intel
install_ubuntu_server_gui_intel: all_intel_software xinitrc


.PHONY: all_arm_software  # Ubuntu ARM doesn't have a Compton PPA package
all_arm_software: cli_utils oh_my_zsh screen pycheckers software_dir x11 dmenu dzen xmonad xmodmap xresources python trayer


.PHONY: install_ubuntu_server_cli
install_ubuntu_server_cli: cli_utils oh_my_zsh screen pycheckers python software_dir pyp localpath
	echo "ok"

