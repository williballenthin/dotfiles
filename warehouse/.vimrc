set smartindent
set tabstop=4
set shiftwidth=4
set expandtab
set number
syntax on
filetype plugin indent on

" pathogen, from https://github.com/tpope/vim-pathogen
execute pathogen#infect()

" NERDTree, from https://github.com/scrooloose/nerdtree
"
" open NERDTree if no file specified
autocmd vimenter * if !argc() | NERDTree | endif
" close NERDTree if last buffer left
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif
