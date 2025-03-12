if test -f ~/.config/fish/local.fish
    source ~/.config/fish/local.fish
end

set fish_greeting # disable welcome prompt
set -gx EDITOR hx
set -gx COLORTERM truecolor # for hx color detection -> themes

direnv hook fish | source
# disable direnv logging
# via: https://github.com/direnv/direnv/issues/68#issuecomment-519030360
set -gx DIRENV_LOG_FORMAT ""

starship init fish | source

if status --is-interactive
    abbr --add --global ll eza --long --git
    abbr --add --global lt eza --tree --level=2 --long --git
    abbr --add --global gs tig status
    abbr --add --global lg lazygit
    abbr --add --global cat bat
    # via: https://stackoverflow.com/a/59069793/87207
    abbr --add --global ipytest pytest --pdb --pdbcls=IPython.terminal.debugger:TerminalPdb
    # requires `uv pip install llm llm-gemini rich-cli`
    abbr --add --global llmx --position command --set-cursor=! 'llm "!" | rich --markdown --line-numbers --hyperlinks --panel square -'

    if type -q atuin
        atuin init fish | source
    end
    zoxide init fish | source

    function ,init-python-project
        if test -d ".env"
            echo ".env already exists" >&2
            return
        end

        if test -d ".envrc"
            echo ".envrc already exists" >&2
            return
        end

        cp -r ~/.dotfiles/nix/profiles/python/ ".env"
        pushd ".env"
        if ! test -f ../.justfile && ! test -f ../justfile
            mv justfile ../.justfile
        end
        if ! test -f ../pyproject.toml
            mv pyproject.toml ../pyproject.toml
        end
        git init .
        git add *
        popd
        echo "created .env" >>&2

        cp ".env/example-.envrc" ".envrc"
        echo "created .envrc" >>&2
    end

    function ,init-rust-project
        if test -d ".env"
            echo ".env already exists" >&2
            return
        end

        if test -d ".envrc"
            echo ".envrc already exists" >&2
            return
        end

        cp -r ~/.dotfiles/nix/profiles/rust/ ".env"
        pushd ".env"
        git init .
        git add *
        popd
        echo "created .env" >>&2

        cp ".env/example-.envrc" ".envrc"
        echo "created .envrc" >>&2
    end

    function ,init-go-project
        if test -d ".env"
            echo ".env already exists" >&2
            return
        end

        if test -d ".envrc"
            echo ".envrc already exists" >&2
            return
        end

        cp -r ~/.dotfiles/nix/profiles/go/ ".env"
        pushd ".env"
        git init .
        git add *
        popd
        echo "created .env" >>&2

        cp ".env/example-.envrc" ".envrc"
        echo "created .envrc" >>&2
    end

    function ,init-js-project
        if test -d ".env"
            echo ".env already exists" >&2
            return
        end

        if test -d ".envrc"
            echo ".envrc already exists" >&2
            return
        end

        cp -r ~/.dotfiles/nix/profiles/js/ ".env"
        pushd ".env"
        git init .
        git add *
        popd
        echo "created .env" >>&2

        cp ".env/example-.envrc" ".envrc"
        echo "created .envrc" >>&2
    end
end

# fzf.fish config
set fzf_preview_dir_cmd eza --all --color=always
set fzf_fd_opts --hidden --exclude=.git
