FROM ubuntu:xenial

# add repo for vim8
RUN apt-get update && \
    apt-get install -y software-properties-common && \
    add-apt-repository -y ppa:jonathonf/vim && \
    apt-get update
RUN apt-get install -y \
    build-essential python-virtualenv \
    python3-dev python3-pip libffi-dev libssl-dev \
    wget tmux git vim ack-grep tmuxinator rlwrap
RUN mkdir /env && virtualenv -p python3 /env
RUN git clone https://github.com/VundleVim/Vundle.vim.git /root/.vim/bundle/Vundle.vim && \
    wget https://raw.githubusercontent.com/williballenthin/dotfiles/master/vim/.vimrc -O /root/.vimrc && \
    vim +PluginInstall +qall && \
    wget https://raw.githubusercontent.com/williballenthin/dotfiles/master/tmux/.tmux.conf -O /root/.tmux.conf && \
    pip3 install pylint mypy ipython
