" >> load plugins
call plug#begin(stdpath('data') . 'vimplug')
    Plug 'nvim-lua/plenary.nvim'
    Plug 'nvim-lua/popup.nvim'
    Plug 'nvim-telescope/telescope.nvim'
    Plug 'neovim/nvim-lspconfig'
    Plug 'kabouzeid/nvim-lspinstall'
    Plug 'glepnir/lspsaga.nvim'
    Plug 'hrsh7th/nvim-compe'
    Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
    Plug 'nvim-treesitter/nvim-treesitter-textobjects'

    Plug 'glepnir/galaxyline.nvim', { 'branch': 'main' }
    Plug 'kyazdani42/nvim-web-devicons'  " needed for galaxyline icons

    Plug 'NLKNguyen/papercolor-theme'
    Plug 'nikvdp/neomux'

    " comment line/word/etc
    Plug 'tomtom/tcomment_vim'

    " wb
    Plug 'simrat39/rust-tools.nvim'
    Plug 'preservim/nerdtree'
    Plug 'airblade/vim-gitgutter'
    Plug 'folke/which-key.nvim'
call plug#end()


" basic settings
syntax on
set number
set relativenumber
set ignorecase      " ignore case
set smartcase     " but don't ignore it, when search string contains uppercase letters
set nocompatible
set incsearch        " do incremental searching
set visualbell
set expandtab
set tabstop=4
set ruler
set smartindent
set shiftwidth=4
set hlsearch
set virtualedit=all
set backspace=indent,eol,start " allow backspacing over everything in insert mode
set autoindent
set mouse=a  " mouse support

colorscheme PaperColor

" wb: treesitter config
lua <<EOF
require'nvim-treesitter.configs'.setup {
  -- One of "all", "maintained" (parsers with maintainers), or a list of languages
  ensure_installed = {
      "bash",
      "c",
      "c_sharp",
      "clojure", 
      "cmake", 
      "cpp", 
      "css", 
      "dockerfile", 
      "fish", 
      "go",
      "html",
      "java", 
      "javascript",
      "json",
      "lua",
      "make",
      "nix",
      "perl",
      "php",
      "python",
      "rust",
      "tsx",
      "typescript",
      "vim",
      "yaml",
      "zig",
  },

  -- Install languages synchronously (only applied to `ensure_installed`)
  sync_install = false,

  -- List of parsers to ignore installing
  ignore_install = { },

  highlight = {
    -- `false` will disable the whole extension
    enable = true,

    -- list of language that will be disabled
    disable = { "rust" },

    -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
    -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
    -- Using this option may slow down your editor, and you may see some duplicate highlights.
    -- Instead of true it can also be a list of languages
    additional_vim_regex_highlighting = false,
  },

  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = "gnn",
      node_incremental = "grn",
      scope_incremental = "grc",
      node_decremental = "grm",
    },
  },

  indent = {
    enable = true
  },
}
EOF

" wb: enable rust-tools
lua <<EOF
require('rust-tools').setup({})
require('rust-tools.inlay_hints').set_inlay_hints()
EOF

" wb: delay before swap file and git gutter updated
set updatetime=100
set timeoutlen=300

" wb: highlight the line number too when there's a git diff
augroup git_gutter_config
    autocmd!
    au VimEnter * GitGutterLineNrHighlightsEnable
augroup END

" wb: set leader key to space
let g:mapleader=" "

" Start NERDTree when Vim is started without file arguments.
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists('s:std_in') | NERDTree | endif

" register keybinding help
" which-key will show popup of completions after `timeoutlen`
lua << EOF
require("which-key").setup({})
local wk = require("which-key")
wk.register({
    -- telescope shows a dialog with autocomplete and preview
    ["<leader>'"] = { "<cmd>lua require'telescope.builtin'.oldfiles{}<cr>", "find recently used files" },
    ["<leader>;"] = { "<cmd>lua require'telescope.builtin'.buffers{}<cr>", "find buffers" },
    ["<leader>/"] = { "<cmd>lua require'telescope.builtin'.current_buffer_fuzzy_find{}<cr>", "find in current buffer" },
    ["<leader>m"] = { "<cmd>lua require'telescope.builtin'.marks{}<cr>", "find marks" },
    ["<leader>p"] = { "<cmd>lua require'telescope.builtin'.find_files{}<cr>", "find files" },
    ["<leader>rg"] = { "<cmd>lua require'telescope.builtin'.live_grep{}<cr>", "ripgrep" },

    ["<leader>f"] = {"<cmd>NERDTreeToggle<cr>", "file browser"},

    ["<leader>g"] = {
        name = "+lsp",

        d = {"<cmd>lua vim.lsp.buf.definition()<CR>", "lsp: go to definition"},
        r = {"<cmd>lua require'telescope.builtin'.lsp_references{}<cr>",
             -- alt: "<cmd>lua vim.lsp.buf.references()<CR>
             "lsp: go to references" },
        D = {"<cmd>lua vim.lsp.buf.declaration()<CR>", "lsp: go to declaration"},
        i = {"<cmd>lua vim.lsp.buf.implementation()<CR>", "lsp: go to implementation"},

        h = {"<cmd>Lspsaga hover_doc<CR>", "lsp: help"},
        n = {"<cmd>lua vim.lsp.buf.rename()<CR>", "lsp: rename"},
        c = {":TComment<CR>", "toggle comment"},
    }
})

wk.setup({})
EOF

" wb: To use `ALT+{h,j,k,l}` to navigate windows from any mode:
" ref: https://neovim.io/doc/user/nvim_terminal_emulator.html
:tnoremap <A-h> <C-\><C-N><C-w>h
:tnoremap <A-j> <C-\><C-N><C-w>j
:tnoremap <A-k> <C-\><C-N><C-w>k
:tnoremap <A-l> <C-\><C-N><C-w>l
:inoremap <A-h> <C-\><C-N><C-w>h
:inoremap <A-j> <C-\><C-N><C-w>j
:inoremap <A-k> <C-\><C-N><C-w>k
:inoremap <A-l> <C-\><C-N><C-w>l
:nnoremap <A-h> <C-w>h
:nnoremap <A-j> <C-w>j
:nnoremap <A-k> <C-w>k
:nnoremap <A-l> <C-w>l
