# basic Ubuntu image with user `user`
FROM ubuntu:22.04

RUN apt-get -y update
RUN apt-get -y upgrade
RUN apt-get install -y sudo

RUN groupadd -r user
RUN useradd -r -g user user
RUN usermod -aG sudo user
RUN echo "user ALL=(ALL:ALL) NOPASSWD: ALL" | tee -a /etc/sudoers.d/local

RUN mkdir /home/user
RUN chown user:user /home/user
RUN chmod -R 770 /home/user

USER user
WORKDIR /home/user
ENV USER=user

# now install nix and home-manager

RUN sudo apt-get install -y curl xz-utils
RUN curl -L https://nixos.org/nix/install | sh
RUN echo ". /home/user/.nix-profile/etc/profile.d/nix.sh" >> /home/user/.profile
RUN . /home/user/.profile && \
    nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager && \
    nix-channel --add https://nixos.org/channels/nixos-22.11 nixos-22.11 && \
    nix-channel --add https://nixos.org/channels/nixos-23.05 nixos-23.05 && \
    nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs-unstable && \
    nix-channel --update
RUN echo "export NIX_PATH=$HOME/.nix-defexpr/channels:/nix/var/nix/profiles/per-user/root/channels${NIX_PATH:+:$NIX_PATH}" >> /home/user/.profile
RUN . /home/user/.profile && \
    nix-shell '<home-manager>' -A install
RUN echo ". /home/user/.nix-profile/etc/profile.d/hm-session-vars.sh" >> /home/user/.profile

# git clone git@github.com:williballenthin/dotfiles.git /home/user/.dotfiles
# source /home/user/.profile
# rm ~/.config/home-manager/home.nix
# ln -s /home/user/.dotfiles/nix/home.nix ~/.config/home-manager/home.nix
# home-manager switch
# nvim +:PlugInstall +qa
# nvim +:TSUpdate +qa
# chsh --shell /home/user/.nix-profile/bin/fish
