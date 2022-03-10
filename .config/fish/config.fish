set -gx EDITOR nvim

direnv hook fish | source
starship init fish | source

if status --is-interactive
    abbr --add --global ll exa --long --git
    abbr --add --global lt exa --tree --level=2 --long --git
    abbr --add --global gs tig status
    abbr --add --global cat bat
end

fish_ssh_agent
