" Vim filetype plugin file
" Language:    Egison
" Maintainer:  Satoshi Egi <egisatoshi@gmail.com>

if exists("b:did_ftplugin")
  finish
endif
let b:did_ftplugin = 1

let s:save_cpo = &cpo
set cpo&vim

" Comment settings
setlocal commentstring=--\ %s
setlocal comments=s1:{-,mb:\ ,ex:-},b:--

" Indentation settings
setlocal expandtab
setlocal shiftwidth=2
setlocal softtabstop=2
setlocal tabstop=2

" Keyword characters (allow ' in identifiers like Haskell)
setlocal iskeyword+=39

" File format
setlocal formatoptions-=t
setlocal formatoptions+=croql

" Undo settings when switching filetypes
let b:undo_ftplugin = "setlocal commentstring< comments< expandtab< shiftwidth< softtabstop< tabstop< iskeyword< formatoptions<"

let &cpo = s:save_cpo
unlet s:save_cpo
