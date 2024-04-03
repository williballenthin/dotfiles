if test -f ~/.config/fish/local.fish
    source ~/.config/fish/local.fish
end

set -gx EDITOR hx

direnv hook fish | source
# disable direnv logging
# via: https://github.com/direnv/direnv/issues/68#issuecomment-519030360
set -gx DIRENV_LOG_FORMAT ""

starship init fish | source

if status --is-interactive
    abbr --add --global ll eza --long --git
    abbr --add --global lt eza --tree --level=2 --long --git
    abbr --add --global gs tig status
    abbr --add --global cat bat
    # via: https://stackoverflow.com/a/59069793/87207
    abbr --add --global ipytest pytest --pdb --pdbcls=IPython.terminal.debugger:TerminalPdb
    abbr --add --global ec emacsclient -nw --create-frame --alternate-editor=nvim
    abbr --add --global em nohup emacs --user="" --maximized . >/dev/null 2>&1 &
    atuin init fish | source
end

# fzf.fish config
set fzf_preview_dir_cmd eza --all --color=always
set fzf_fd_opts --hidden --exclude=.git
