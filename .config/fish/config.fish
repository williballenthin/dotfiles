if test -f /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.fish
    source /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.fish
end
if test -f /nix/var/nix/profiles/default/share/fish/vendor_completions.d/nix.fish
    source /nix/var/nix/profiles/default/share/fish/vendor_completions.d/nix.fish
end

if test -f ~/.config/fish/local.fish
    source ~/.config/fish/local.fish
end

set fish_greeting # disable welcome prompt
set -gx EDITOR hx
set -gx COLORTERM truecolor # for hx color detection -> themes

# Configure less:
# -R: handle ANSI escape codes
# -J: show search locations in side column
# -S: truncate lines (and use left/right to horizontally scroll), to fix quirk with -J
# -i: Use smart-case for searching (case-insensitive unless uppercase used)
# -c: show text at top for small files
# -x4: 4-space tabs
# -jN: Specifies a line on the screen where the "target" line is to be positioned
# P...: prompt containing [filename/STDIN] and [N%] for percentage through file.
# NOTE: --use-color and -Dd+r/-Du+b removed; they require less 590+ (macOS ships 581)
#
# via: https://lobste.rs/s/lrx8vc/assorted_less_1_tips#c_3ubyqu
set -gx LESS '-RJSic -x4 -j4 -P?f[%f]:[STDIN].?pB - [%pB\\%]'
set -gx MANROFFOPT -c

direnv hook fish | source
# disable direnv logging
# via: https://github.com/direnv/direnv/issues/68#issuecomment-519030360
set -gx DIRENV_LOG_FORMAT ""

# via: https://news.ycombinator.com/item?id=45841794
set -gx CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC 1
set -gx DISABLE_ERROR_REPORTING 1
set -gx DISABLE_TELEMETRY 1

# via: https://code.claude.com/docs/en/model-config#extended-context
set -gx CLAUDE_CODE_DISABLE_1M_CONTEXT 1

# see: https://github.com/modem-dev/hunk/issues/337
set -gx HUNK_TEXT_PAGER cat

starship init fish | source

# add global NPM binaries, like claude code
fish_add_path ~/.local/state/npm/bin/

# add uv tool binaries, like ty
fish_add_path ~/.local/bin/

if status --is-interactive
    abbr --add --global ll eza --long
    abbr --add --global lt eza --tree --level=2 --long
    abbr --add --global gs tig status
    abbr --add --global lg lazygit
    abbr --add --global cat bat
    # via: https://stackoverflow.com/a/59069793/87207
    abbr --add --global ipytest pytest --pdb --pdbcls=IPython.terminal.debugger:TerminalPdb
    # requires `uv pip install llm llm-gemini rich-cli`
    abbr --add --global llmx --position command --set-cursor=! 'llm "!" --system "be concise." | rich --markdown --line-numbers --hyperlinks --panel square --force-terminal - | less -FIRX'
    abbr --add --global md 'rich --markdown --line-numbers --hyperlinks --panel square --force-terminal - | less -FIRX'
    abbr --add --global opus 'claude --dangerously-skip-permissions --model claude-opus-4-6'
    abbr --add --global sonnet 'claude --dangerously-skip-permissions --model claude-sonnet-4-6'
    abbr --add --global haiku 'claude --dangerously-skip-permissions --model claude-haiku-4-5'

    if type -q atuin
        atuin init fish | source
    end
    zoxide init fish | source

    bind \cT tv_smart_autocomplete

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

    function ,init-minimal-project
        if test -d ".env"
            echo ".env already exists" >&2
            return
        end

        if test -d ".envrc"
            echo ".envrc already exists" >&2
            return
        end

        cp -r ~/.dotfiles/nix/profiles/minimal/ ".env"
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
