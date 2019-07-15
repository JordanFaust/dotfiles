""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" CUSTOM KEY MAPS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Set leader to ,
let mapleader=","

" Operator pending mapping that effects the content of the surround
" parentheses. Think of this as 'parentheses'
" Example dp => 'delete parameters'
onoremap p i(

" Operator pending mapping that effects the content of the next set of
" surrounding parentheses. Think of this as 'inside next parentheses'.
" Example cin( => 'change inside next parentheses'
onoremap inp :<c-u>normal! f(vi(<cr>

" Operator pending mapping that effects the content of the last set of
" surrounding parentheses. Think of this as 'inside last parentheses'.
" Example cil( => 'change inside last parentheses'
onoremap ilp :<c-u>normal! F)vi(<cr>

" Jump to next/previous screen row in the editor instead of the line when wrapping.
nnoremap j gj
nnoremap k gk

" Use Cmd + N to change tabs
noremap <D-1> 1gt
noremap <D-2> 2gt
noremap <D-3> 3gt
noremap <D-4> 4gt
noremap <D-5> 5gt
noremap <D-6> 6gt
noremap <D-7> 7gt
noremap <D-8> 8gt
noremap <D-9> 9gt
noremap <D-0> :tablast<CR>

" don't use Ex mode, use Q for formatting
noremap Q gq

" clear the search buffer when hitting return
nnoremap <CR> :nohlsearch<cr>

" toggle the current fold
nnoremap <Space> za

" In command-line mode, C-a jumps to beginning (to match C-e)
cnoremap <C-a> <Home>

" Map CtrlPBuffer to <C-b>
noremap <C-b> :CtrlPBuffer<CR>

noremap <C-n> :NERDTreeToggle<CR>

" double percentage sign in command mode is expanded
" to directory of current file - http://vimcasts.org/e/14
cnoremap %% <C-R>=expand('%:h').'/'<cr>

" toggle between last open buffers
nnoremap <leader><leader> <c-^>

" Open vimrc for editing
nnoremap <leader>ev :vsplit $MYVIMRC<cr>

" Load new changes made in vimrc
nnoremap <leader>sv :source $MYVIMRC<cr>

" copy to clipboard
noremap <leader>y "+y

" compile a maven project so that its parsable for the quickfix list
nnoremap <leader>m :Dispatch mvn compile -q -f pom.xml<CR>

" paste lines from unnamed register and fix indentation
nnoremap <leader>p pV`]=
nnoremap <leader>P PV`]=

" Close all other windows, open a vertical split, and open this file's test
" alternate in it.
nnoremap <leader>s :call FocusOnFile()<cr>

" Open the current file's test alternate in a vertical split.
nnoremap <leader>. :call OpenTestAlternate()<cr>

" Run dispatch
nnoremap <leader>t :Dispatch<CR>

" Rename the current buffers file and prompt for the new file name.
nnoremap <leader>n :call RenameFile()<cr>

" Strip the whitespace from the current buffer.
map <silent> <leader>w :call StripTrailingWhitespaces()<CR>

" Run ctags in a dispatch window
nnoremap <leader>c :Start! .git/hooks/ctags >/dev/null<CR>

nnoremap <leader>k :OnlineThesaurusCurrentWord<CR>

" Open Tagbar
nnoremap <leader>b :Tagbar<CR>

" Repurpose arrow keys to move lines
" Inspired by http://jeetworks.com/node/89
" Arrow key remapping:
" Up/Dn = move line up/dn
" Left/Right = indent/unindent

" Normal mode
nnoremap <silent> <Left>   <<
nnoremap <silent> <Right>  >>
nnoremap <silent> <Up>     <Esc>:call <SID>MoveLineUp()<CR>
nnoremap <silent> <Down>   <Esc>:call <SID>MoveLineDown()<CR>

" Visual mode
vnoremap <silent> <Left>   <gv
vnoremap <silent> <Right>  >gv
vnoremap <silent> <Up>     <Esc>:call <SID>MoveVisualUp()<CR>
vnoremap <silent> <Down>   <Esc>:call <SID>MoveVisualDown()<CR>

" Insert mode
inoremap <silent> <Left>   <C-D>
inoremap <silent> <Right>  <C-T>
inoremap <silent> <Up>     <C-O>:call <SID>MoveLineUp()<CR>
inoremap <silent> <Down>   <C-O>:call <SID>MoveLineDown()<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" PRESERVE HISTORY
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Preserve history and cursor position while executing the given command
function! Preserve(command)
  " Preparation: save last search, and cursor position.
  let _s=@/
  let l = line(".")
  let c = col(".")
  " Do the business:
  execute a:command
  " Clean up: restore previous search history, and cursor position
  let @/=_s
  call cursor(l, c)
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" KILL WHITE SPACE
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! StripTrailingWhitespaces()
  call Preserve("%s/\\s\\+$//e")
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" FOCUS ON CURRENT FILE
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! FocusOnFile()
  tabnew %
  normal! v
  normal! l
  call OpenTestAlternate()
  normal! h
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" RENAME CURRENT FILE
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! RenameFile()
  let old_name = expand('%')
  let new_name = input('New file name: ', expand('%'), 'file')
  if new_name != '' && new_name != old_name
    exec ':saveas ' . new_name
    exec ':silent !rm ' . old_name
    redraw!
  endif
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" SWITCH BETWEEN TEST AND PRODUCTION CODE
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! OpenTestAlternate()
  let new_file = AlternateForCurrentFile()
  exec ':vsp ' . new_file
endfunction

function! AlternateForCurrentFile()
  let current_file = expand("%")
  let new_file = current_file
  if match(current_file, '\.rb$') != -1
    let new_file = RubyAlternateForCurrentFile(current_file)
  elseif match(current_file, '\.java$') != -1
    let new_file = JavaAlternateForCurrentFile(current_file)
  elseif match(current_file, '\.groovy$') != -1
    let new_file = GroovyAlternateForCurrentFile(current_file)
  endif
  return new_file
endfunction

function! RubyAlternateForCurrentFile(current_file)
  let new_file = a:current_file
  let in_spec = match(a:current_file, '^spec/') != -1
  let going_to_spec = !in_spec
  let in_app = match(a:current_file, '\<controllers\>') != -1 || match(a:current_file, '\<models\>') != -1 || match(a:current_file, '\<views\>') != -1 || match(a:current_file, '\<helpers\>') != -1 || match(a:current_file, '\<presenters\>') != -1
  if going_to_spec
    if in_app
      let new_file = substitute(new_file, '^app/', '', '')
    end
    let new_file = substitute(new_file, '\.e\?rb$', '_spec.rb', '')
    let new_file = 'spec/' . new_file
  else
    let new_file = substitute(new_file, '_spec\.rb$', '.rb', '')
    let new_file = substitute(new_file, '^spec/', '', '')
    if in_app
      let new_file = 'app/' . new_file
    end
  endif
  return new_file
endfunction

function! JavaAlternateForCurrentFile(current_file)
  let new_file = a:current_file
  let in_test = match(a:current_file, 'Test\.java$') != -1
  let going_to_test = !in_test
  if going_to_test
    let new_file = substitute(new_file, '\.java$', 'Test\.java', '')
    let new_file = substitute(new_file, '\/main\/', '\/test\/', '')
  else
    let new_file = substitute(new_file, 'Test\.java$', '\.java', '')
    let new_file = substitute(new_file, '\/test\/', '\/main\/', '')
  endif
  return new_file
endfunction

function! GroovyAlternateForCurrentFile(current_file)
  let new_file = a:current_file
  let in_test = match(a:current_file, 'Test\.groovy$') != -1
  let going_to_test = !in_test
  if going_to_test
    let new_file = substitute(new_file, '\.groovy$', 'Test\.groovy', '')
    let new_file = substitute(new_file, '\/main\/', '\/test\/', '')
  else
    let new_file = substitute(new_file, 'Test\.groovy$', '\.groovy', '')
    let new_file = substitute(new_file, '\/test\/', '\/main\/', '')
  endif
  return new_file
endfunction

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Repurpose Arrow Keys
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

function! s:MoveLineUp()
  call <SID>MoveLineOrVisualUp(".", "")
endfunction

function! s:MoveLineDown()
  call <SID>MoveLineOrVisualDown(".", "")
endfunction

function! s:MoveVisualUp()
  call <SID>MoveLineOrVisualUp("'<", "'<,'>")
  normal gv
endfunction

function! s:MoveVisualDown()
  call <SID>MoveLineOrVisualDown("'>", "'<,'>")
  normal gv
endfunction

function! s:MoveLineOrVisualUp(line_getter, range)
  let l_num = line(a:line_getter)
  if l_num - v:count1 - 1 < 0
    let move_arg = "0"
  else
    let move_arg = a:line_getter." -".(v:count1 + 1)
  endif
  call <SID>MoveLineOrVisualUpOrDown(a:range."move ".move_arg)
endfunction

function! s:MoveLineOrVisualDown(line_getter, range)
  let l_num = line(a:line_getter)
  if l_num + v:count1 > line("$")
    let move_arg = "$"
  else
    let move_arg = a:line_getter." +".v:count1
  endif
  call <SID>MoveLineOrVisualUpOrDown(a:range."move ".move_arg)
endfunction

function! s:MoveLineOrVisualUpOrDown(move_arg)
  let col_num = virtcol(".")
  execute "silent! ".a:move_arg
  execute "normal! ".col_num."|"
endfunction
