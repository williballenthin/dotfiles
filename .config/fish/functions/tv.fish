function tv_smart_autocomplete
    set -l current_prompt (commandline -cp)

    set -l output (tv --autocomplete-prompt "$current_prompt")

    if test -n "$output"
        # add a space if the prompt does not end with one (unless the prompt is an implicit cd, e.g. '\.')
        string match -r '.*( |./)$' -- "$current_prompt" || set current_prompt "$current_prompt "
        commandline -r "$current_prompt$output"
        commandline -f repaint
    end
end
