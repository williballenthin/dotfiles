" pathogen, from https://github.com/tpope/vim-pathogen
execute pathogen#infect()

"set encoding=utf8

set smartindent
set tabstop=4
set shiftwidth=4
set expandtab
set smarttab
let indent_guides_enable_on_vim_startup = 1


syntax on
set t_co=256
" set background=dark
" colorscheme solarized

set number

filetype plugin indent on

" NERDTree, from https://github.com/scrooloose/nerdtree
"
" open NERDTree if no file specified
autocmd vimenter * if !argc() | NERDTree | endif
" close NERDTree if last buffer left
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif


set so=14 " keep cursor away from sceen edges

set cursorline
:hi CursorLine   cterm=NONE ctermbg=DarkGray guibg=DarkGray
:hi CursorColumn cterm=NONE ctermbg=DarkGray guibg=DarkGray

set mouse=n
set ttymouse=xterm2  " allow resizing of splits with mouse
"
" Automatically leave insert mode after 'updatetime' (4s by default).
au CursorHoldI * stopinsert

set list listchars=tab:⇥⇥,

" trailing whitespace highlighted
highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/
autocmd BufWinLeave * call clearmatches()


nnoremap <F2> :set invpaste paste?<CR>
set pastetoggle=<F2>
set showmode
