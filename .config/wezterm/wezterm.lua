local wezterm = require 'wezterm'
local config = {}

config.color_scheme = 'Batman'
config.keys = {
    {
        key = 'F11',
        action = wezterm.action.ToggleFullScreen,
    },
}
config.hide_tab_bar_if_only_one_tab = true

return config
