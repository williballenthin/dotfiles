source ~/.config/fish/local.fish

set -gx EDITOR nvim

direnv hook fish | source
starship init fish | source

if status --is-interactive
    abbr --add --global ll exa --long --git
    abbr --add --global lt exa --tree --level=2 --long --git
    abbr --add --global gs tig status
    abbr --add --global cat bat
    # via: https://stackoverflow.com/a/59069793/87207
    abbr --add --global ipytest pytest --pdb --pdbcls=IPython.terminal.debugger:TerminalPdb
end

# fzf.fish config
set fzf_preview_dir_cmd exa --all --color=always
set fzf_fd_opts --hidden --exclude=.git
bind \ct _fzf_search_directory
