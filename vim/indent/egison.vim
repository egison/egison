" Vim indent file
" Language:    Egison
" Maintainer:  Satoshi Egi <egisatoshi@gmail.com>

if exists("b:did_indent")
  finish
endif
let b:did_indent = 1

setlocal indentexpr=GetEgisonIndent()
setlocal indentkeys=!^F,o,O,0|
setlocal autoindent

let b:undo_indent = "setlocal indentexpr< indentkeys< autoindent<"

if exists("*GetEgisonIndent")
  finish
endif

function! GetEgisonIndent()
  let lnum = prevnonblank(v:lnum - 1)
  if lnum == 0
    return 0
  endif

  let prevline = getline(lnum)
  let curline = getline(v:lnum)
  let ind = indent(lnum)

  " Increase indent after keywords that start blocks
  if prevline =~# '\<\(def\|class\|instance\|inductive\|where\|do\|let\|matcher\)\>.*$'
        \ && prevline !~# ':=.*[^ \t]'
    let ind = ind + &shiftwidth
  endif

  " Increase indent after 'with' at end of line
  if prevline =~# '\<with\>\s*$'
    let ind = ind + &shiftwidth
  endif

  " Increase indent after ':=' at end of line
  if prevline =~# ':=\s*$'
    let ind = ind + &shiftwidth
  endif

  " Align '|' bars with previous '|' bar
  if curline =~# '^\s*|'
    let barline = search('^\s*|', 'bnW')
    if barline > 0
      let ind = indent(barline)
    endif
  endif

  " Top-level definitions start at column 0
  if curline =~# '^\(def\|class\|instance\|inductive\|load\|declare\|infixl\|infixr\|infix\)\>'
    let ind = 0
  endif

  return ind
endfunction
