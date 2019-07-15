""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" VUNDLE
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set nocompatible
filetype off
" Vundle setup
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let vundle manage vundle
Plugin 'gmarik/Vundle.vim'

Plugin 'arcticicestudio/nord-vim'
Plugin 'mileszs/ack.vim'
Plugin 'kien/ctrlp.vim'
Plugin 'Yggdroot/indentLine'
Plugin 'scrooloose/nerdtree'
" Plugin 'scrooloose/syntastic'
Plugin 'godlygeek/tabular'
Plugin 'tpope/vim-bundler'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-dispatch'
Plugin 'tpope/vim-endwise'
Plugin 'tpope/vim-eunuch'
Plugin 'tpope/vim-fugitive'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'xolox/vim-misc'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-surround'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'tpope/vim-unimpaired'
Plugin 'stephpy/vim-yaml'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
