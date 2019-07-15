""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" CUSTOM AUTOCMDS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if has("autocmd")
  " Autocmd group responsible for file type specific autocmd's
  augroup file_types
    " Clear out the file_types autocommand group
    autocmd!

    " In Makefiles, use real tabs, not tabs expanded to spaces
    autocmd FileType make setlocal noexpandtab

    " In gitconfig, use real tabs, not tabs expanded to spaces
    autocmd FileType gitconfig setlocal noexpandtab

    " Make groovy play nice. And :( groovy
    " Set 'formatoptions' to break comment lines but not other lines,
    " and insert the comment leader when hitting <CR> or using "o".
    " Set 'comments' to format dashed lists in comments. Behaves just like C.
    autocmd FileType groovy setlocal formatoptions-=t formatoptions+=croql
                              \ smartindent autoindent
                              \ comments& comments^=sO:*\ -,mO:*\ \ ,exO:*/
                              \ commentstring=//%s

    " Setup dispatch to default to running the current test file
    autocmd FileType groovy let b:dispatch = 'mvn test -Dtest=' . expand("%:t:r")
    autocmd FileType java let b:dispatch = 'mvn test -Dtest=' . expand("%:t:r")

    " Setup indent rules for Java
    autocmd Filetype java setlocal softtabstop=4 tabstop=4 shiftwidth=4

    " Setup tab spacing and syntax for avro models.
    autocmd Filetype avdl setlocal shiftwidth=2 tabstop=2 expandtab=2 expandtab=4

    " make Python follow PEP8 ( http://www.python.org/dev/peps/pep-0008/ )
    autocmd FileType python setlocal softtabstop=4 tabstop=4 shiftwidth=4 textwidth=79

    " Setup dispatch to default to running the current test file
    autocmd FileType ruby let b:dispatch = 'bundle exec rspec %'

    " Setup tab spacing for Rust
    autocmd FileType rust setlocal softtabstop=4 tabstop=4 shiftwidth=4 textwidth=79
    autocmd FileType rust let b:dispatch = 'rustc %'
  augroup END

  " Autocmd group responsible for buffer opening and closing specific
  " autocmd's
  augroup event_group
    " Clear out the event_group autocommand group
    autocmd!

    " Make sure all markdown files have the correct filetype set and setup wrapping
    autocmd BufRead,BufNewFile *.{md,markdown,mdown,mkd,mkdn,txt} setf markdown | setlocal wrap | setlocal wrapmargin=2

    " Treat JSON files like JavaScript
    autocmd BufRead,BufNewFile *.json set ft=javascript

    " Handle rails asset pipeline wierdness for less
    autocmd BufRead,BufNewFile *.less set ft=less

    " https://github.com/sstephenson/bats
    autocmd BufRead,BufNewFile *.bats set ft=sh

    " Set the filetype to rust when opening a rust file
    autocmd BufRead,BufNewFile *.rs set ft=rust

    " Set the filetype to scala when opening a scala file
    autocmd BufRead,BufNewFile *.scala set ft=scala

    " Set the filetype to C when opening a avro file
    autocmd BufRead,BufNewFile *.avdl set ft=c

    " Remember last location in file, but not for commit messages.
    " see :help last-position-jump
    autocmd BufReadPost * if &filetype !~ '^git\c' && line("'\"") > 0 && line("'\"") <= line("$")
      \| exe "normal! g`\"" | endif

    " mark Jekyll YAML frontmatter as comment
    autocmd BufRead,BufNewFile *.{md,markdown,html,xml} sy match Comment /\%^---\_.\{-}---$/

    " magic markers: enable using `H/S/J/C to jump back to
    " last HTML, stylesheet, JS or Ruby code buffer
    autocmd BufLeave *.{erb,html}      exe "normal! mH"
    autocmd BufLeave *.{css,scss,sass} exe "normal! mS"
    autocmd BufLeave *.{js,coffee}     exe "normal! mJ"
    autocmd BufLeave *.{rb}            exe "normal! mC"
  augroup END

  " Autcomd group responsible for leaving and entering insert mode specific
  " autocmd's
  augroup insert_mode_group
    " Clear out the insert_mode_group autocommand group
    autocmd!

    " Avoid showing trailing whitespace when in insert mode
    autocmd InsertEnter * :set listchars-=trail:*
    autocmd InsertLeave * :set listchars+=trail:*
  augroup END
endif
