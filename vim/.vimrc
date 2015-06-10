:let mapleader = ","


"  ----- BEGIN VUNDLE -----

set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim/
call vundle#begin()

" Brief help
" :BundleList          - list configured bundles
" :BundleInstall(!)    - install (update) bundles
" :BundleSearch(!) foo - search (or refresh cache first) for foo
" :BundleClean(!)      - confirm (or auto-approve) removal of unused bundles

" let Vundle manage Vundle, required
Plugin 'gmarik/vundle'

" ------------ NerdTree -------------------------------------------------------
Plugin 'scrooloose/nerdtree'
" KEYMAP===============================
:nmap <leader>e :NERDTreeToggle<CR>

" ------------ CtrlP ----------------------------------------------------------
Plugin 'https://github.com/kien/ctrlp.vim'

" KEYMAP===============================
" default: control-p --> :CtrlP
:nmap ; :CtrlPBuffer<CR>

" SETTINGS==============================
"
" from Things of Variable interest blog
" http://statico.github.io/vim.html
:let g:ctrlp_match_window_bottom = 1
:let g:ctrlp_match_window_reversed = 0
:let g:ctrlp_custom_ignore = '\v\~$|\.(o|swp|pyc|wav|mp3|ogg|blend)$|(^|[/\\])\.(hg|git|bzr)($|[/\\])|__init__\.py'
:let g:ctrlp_working_path_mode = 0
:let g:ctrlp_dotfiles = 0
:let g:ctrlp_switch_buffer = 0

" ------------ Ack -----------------------------------------------------------
Plugin 'mileszs/ack.vim'
" KEYMAP===============================
" don't know yet, thinking "\a :Ack [query goes here, how2prompt?]"

" ------------ TagBar --------------------------------------------------------
Plugin 'majutsushi/tagbar'
" KEYMAP===============================
:nmap <leader>t :TagbarToggle<CR>

" ------------ vim-gitgutter -------------------------------------------------
" SETTINGS===============================
Plugin 'airblade/vim-gitgutter'
" Gutter should be black
highlight SignColumn ctermbg=0


" ------------ go-vim --------------------------------------------------------
Plugin 'fatih/vim-go'
" SETTINGS===============================
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1
" KEYMAP===============================
au FileType go nmap <leader>r <Plug>(go-run)
au FileType go nmap <leader>b <Plug>(go-build)
au FileType go nmap <Leader>gd <Plug>(go-doc)
au FileType go nmap <Leader>gv <Plug>(go-doc-vertical)
au FileType go nmap <Leader>gb <Plug>(go-doc-browser)
au FileType go nmap <Leader>s <Plug>(go-implements)
au FileType go nmap <Leader>i <Plug>(go-info)
au FileType go nmap <Leader>e <Plug>(go-rename)

" ------------ Others ---------------------------------------------------------
Plugin 'nvie/vim-flake8'
"Plugin 'Valloric/YouCompleteMe'  " requires Python 2 support in VIM
Plugin 'leafo/moonscript-vim'
Plugin 'bling/vim-airline'
Plugin 'git://github.com/rainux/vim-vala.git'
" To ignore plugin indent changes, instead use:
"filetype plugin on
filetype plugin indent on     " required

" see :h vundle for more details or wiki for FAQ
" NOTE: comments after Plugin commands are not allowed.
" Put your stuff after this line

call vundle#end()
"  ----- END VUNDLE -----



" golang support
"
" Some Linux distributions set filetype in /etc/vimrc.
" Clear filetype flags before changing runtimepath to force Vim to reload them.
filetype off
filetype plugin indent off
set runtimepath+=$GOROOT/misc/vim
filetype plugin indent on
syntax on
au BufNewFile,BufRead *.gotemplate set filetype=go



set encoding=utf8
set smartindent
set tabstop=4
set shiftwidth=4
set expandtab
set smarttab
let indent_guides_enable_on_vim_startup = 1

" from Things of Variable interest blog
" http://statico.github.io/vim.html
:set incsearch
:set ignorecase
:set smartcase
:set hlsearch
:nmap \q :nohlsearch<CR>


set number
syntax on
"set t_co=256

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

nmap J :bn<CR>
nmap K :bp<CR>
nmap <F7> :CtrlP<CR>
nmap <F8> :TagbarToggle<CR>


" Python should not use tabs, but spaces
autocmd BufEnter *.py set ai sw=4 ts=4 sta et fo=croql

