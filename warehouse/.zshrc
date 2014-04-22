# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="gallifrey"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(gdoc git cpanm debian django extract github gnu-utils history-substring-search perl pip python svn terminator colored-man colorize encode64 git-extras)

source $ZSH/oh-my-zsh.sh

ZSH_THEME=evan

# Customize to your needs...
export PATH=/usr/sbin:/sbin:/usr/local/bin:/usr/bin:/bin:/usr/local/games:/usr/games



## options from: http://askubuntu.com/questions/1577/moving-from-bash-to-zsh
setopt completeinword
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*:killall:*' command 'ps -u $USER -o cmd'

# superglobs
setopt extendedglob
unsetopt caseglob

getopt interactivecomments # pound sign in interactive prompt
REPORTTIME=10
function apt-list-packages {
      dpkg-query -W --showformat='${Installed-Size} ${Package} ${Status}\n' | grep -v deinstall | sort -n | awk '{print $1" "$2}'
}




###########   BASH / ZSH COMMON ##################

EMACSCLIENT="emacsclient --alternate-editor=\"\" -c"
alias top='htop'
alias play='mpg123'
alias up='cd ..'
alias e="emacsclient -c"
alias l="ls -laht"
alias o="xdg-open"
alias dv="dirs -v"

j() {
    pushd +"$1";
}

google() {
        qstring=""

        for arg in $*
        do
                  qstring="$qstring+$arg"
        done

        w3m -no-cookie "http://www.google.com/search?q=$qstring";
}


s() {
        grep -r "$1" .
}

f() {
        find . -iname "*$1*"
}

# A shortcut function that simplifies usage of xclip.
# - Accepts input from either stdin (pipe), or params.
# - If the input is a filename that exists, then it
#   uses the contents of that file.
# from: http://madebynathan.com/2011/10/04/a-nicer-way-to-use-xclip/
# ------------------------------------------------
cb() {
  local _scs_col="\e[0;32m"; local _wrn_col='\e[1;31m'; local _trn_col='\e[0;33m'
  # Check that xclip is installed.
  if ! which xclip | grep xclip -q; then
    echo -e "$_wrn_col""You must have the 'xclip' program installed.\e[0m"
  # Check user is not root (root doesn't have access to user xorg server)
  elif [[ $(whoami) == root ]]; then
    echo -e "$_wrn_col""Must be regular user (not root) to copy a file to the clipboard.\e[0m"
  else
    # If no tty, data should be available on stdin
    if [[ "$( tty )" == 'not a tty' ]]; then
      input="$(< /dev/stdin)"
    # Else, fetch input from params
    else
      input="$*"
    fi
    if [ -z "$input" ]; then  # If no input, print usage message.
      echo "Copies a string or the contents of a file to the clipboard."
      echo "Usage: cb <string or file>"
      echo "       echo <string or file> | cb"
    else
      # If the input is a filename that exists, then use the contents of that file.
      if [ -e "$input" ]; then input="$(cat $input)"; fi
      # Copy input to clipboard
      echo -n "$input" | xclip -selection c
      # Truncate text for status
      if [ ${#input} -gt 80 ]; then input="$(echo $input | cut -c1-80)$_trn_col...\e[0m"; fi
      # Print status.
      # echo -e "$_scs_col""Copied to clipboard:\e[0m $input"
    fi
  fi
}

function pw() {
    echo -n ">";
    read -s string;
    echo "";
    if (( $# == 0 ))
    then
        echo -n "$string" | md5sum - | cut -d " " -f 1 | cb;
    else
        echo -n "$string" | md5sum - | cut -c 1-$1 | cb;
    fi
    sleep 10;
    cb "empty";
    clear
}


function willi-git() {
    REMOTE_DIR="/home/ubuntu/Git/$(basename  $(pwd))"".git";

    ssh -i ~/.ssh/aws.pem ubuntu@54.243.153.154 "mkdir \"$REMOTE_DIR\"; git init --bare \"$REMOTE_DIR\"";
    git remote add willi-git ubuntu@54.243.153.154:"$REMOTE_DIR"; 
    git push --all willi-git;
}
alias wg willi-git;

function git-add-all() {
    if [ ! -d ".git" ]; then
        echo "You must be in the root of a Git repository!";
        return -1;
    fi

    CONFIG=".git/config";


    if [[ $(cat .git/config| grep "\[remote" | grep "all" | wc -l) -ne 0 ]]; then
        echo "Git remote \"all\" already configured."
        return -2;
    fi

    echo "[remote \"all\"]" >> "$CONFIG";
    git remote -v | grep "fetch" | grep "\(master\|willi\)" | awk '{print "\turl = "$2}' >> "$CONFIG"; 
}
alias gaa git-add-all;


ssh-add ~/.ssh/aws.pem >/dev/null 2>&1;


# from: http://jeroenjanssens.com/2013/08/16/quickly-navigate-your-filesystem-from-the-command-line.html
export MARKPATH=$HOME/.marks
function jump { 
        cd -P "$MARKPATH/$1" 2>/dev/null || echo "No such mark: $1"
}
function mark { 
        mkdir -p "$MARKPATH"; ln -s "$(pwd)" "$MARKPATH/$1"
}
function unmark { 
        rm -i "$MARKPATH/$1"
}
function marks {
        ls -l "$MARKPATH" | sed 's/  / /g' | cut -d' ' -f9- | sed 's/ -/\t-/g' && echo
}
function _completemarks {
      reply=($(ls $MARKPATH))
}
compctl -K _completemarks jump
compctl -K _completemarks unmark


PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
PATH=$PATH:/usr/local/go/bin

if [[ -f "/home/willi/.zshrc.local" ]]; then
    source "/home/willi/.zshrc.local";
fi

PATH=$PATH:/opt/010editor;export PATH; # ADDED BY INSTALLER - DO NOT EDIT OR DELETE THIS COMMENT - 87FF8EFC-483D-BCAA-D67D-735CF60410D1 8C13F59B-E126-3EA5-C25D-2435E15962EF

PATH=$PATH:/opt/010editor;export PATH; # ADDED BY INSTALLER - DO NOT EDIT OR DELETE THIS COMMENT - 87FF8EFC-483D-BCAA-D67D-735CF60410D1 56BE20D5-FA0F-D226-687C-435EEA6841D9

[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm" 

PATH=$PATH:/opt/Sweetscape;export PATH; # ADDED BY INSTALLER - DO NOT EDIT OR DELETE THIS COMMENT - 87FF8EFC-483D-BCAA-D67D-735CF60410D1 94E6EA6B-7B3E-7A2C-61FE-A781E326C7C0
PATH=/usr/local/jdk1.8.0/bin:"$PATH"; export PATH


PATH="$PATH":/data/data/Git/python-registry/samples; export PATH
PATH="$PATH":/data/data/Git/LfLe/; export PATH
PATH=$PATH:~/.path  # __LOCAL_PATH__
PATH=$PATH:~/.cabal/bin  # __CABAL_PATH__
