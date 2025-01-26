
set nocompatible
" filetype plugin on

set backupdir=%HOMEPATH%/.vim/tmp//,.
set directory=%HOMEPATH%/.vim/tmp//,.
set undodir=%HOMEPATH%/.vim/tmp//,.

set tabstop=4
set shiftwidth=4

set number

set hlsearch
set incsearch
set ignorecase
set shortmess-=S
set clipboard=unnamed


if has("gui_running")
	set guioptions -=m 
	set guioptions -=T
	set guioptions -=r
	set guioptions -=L
	colorscheme slate
	set guifont=Courier_New:h10
endif

" https://vim.fandom.com/wiki/Displaying_status_line_always
set laststatus=2
" set ruler

set encoding=utf-8

" "https://stackoverflow.com/questions/9065941/how-can-i-change-vim-status-line-color
" function! InsertStatuslineColor(mode)
"   if a:mode == 'i'
" 	hi statusline guibg=Cyan ctermfg=6 guifg=Black ctermbg=0
"   elseif a:mode == 'r'
" 	hi statusline guibg=Purple ctermfg=5 guifg=Black ctermbg=0
"   else
" 	hi statusline guibg=DarkRed ctermfg=1 guifg=Black ctermbg=0
"   endif
" endfunction

" au InsertEnter * call InsertStatuslineColor(v:insertmode)
" au InsertLeave * hi statusline guibg=DarkGrey ctermfg=8 guifg=White ctermbg=15

" " default the statusline to green when entering Vim
" hi statusline guibg=DarkGrey ctermfg=8 guifg=White ctermbg=15

" " Formats the statusline
" set statusline=%f                           " file name
" set statusline+=[%{strlen(&fenc)?&fenc:'none'}, "file encoding
" set statusline+=%{&ff}] "file format
" set statusline+=%y      "filetype
" set statusline+=%h      "help file flag
" set statusline+=%m      "modified flag
" set statusline+=%r      "read only flag


" set statusline+=\ %=                        " align left
" set statusline+=Line:%l/%L[%p%%]            " line X of Y [percent of file]
" set statusline+=\ Col:%c                    " current column
" set statusline+=\ Buf:%n                    " Buffer number

" https://stackoverflow.com/questions/25745169/to-get-lnoremap-with-l-to-work
set iminsert=1
set imcmdline

lnoremap Q <nop>
cnoremap Q <nop>
lnoremap W <nop>
cnoremap W <nop>
lnoremap E {
cnoremap E {
lnoremap R }
cnoremap R }
lnoremap T <nop>
cnoremap T <nop>
lnoremap Y <C-o><C-r>
cnoremap Y <C-o><C-r>
lnoremap U (
cnoremap U (
lnoremap I )
cnoremap I )
lnoremap O +
cnoremap O +
lnoremap P -
cnoremap P -
lnoremap [ _
cnoremap [ _
lnoremap { /
cnoremap { /

lnoremap A <end>
cnoremap A <end>
lnoremap S 1
cnoremap S 1
lnoremap D 0
cnoremap D 0
lnoremap F =
cnoremap F =
lnoremap G &
cnoremap G &
lnoremap H <Left>
cnoremap H <Left>
lnoremap J <Down>
cnoremap J <Down>
lnoremap K <Up>
cnoremap K <Up>
lnoremap L <Right>
cnoremap L <Right>

lnoremap ' <BS>
cnoremap ' <BS>
lnoremap " <BS>
cnoremap " <BS>

" https://vi.stackexchange.com/questions/4556/undo-in-insert-mode
lnoremap Z <C-o>u
cnoremap Z <C-o>u
lnoremap X <C-g>u<C-r>*
cnoremap X <C-g>u<C-r>*
lnoremap C 2
cnoremap C 2
lnoremap V \
cnoremap V \
lnoremap B <Bar>
cnoremap B <Bar>
lnoremap N [
cnoremap N [
lnoremap M ]
cnoremap M ]

lnoremap / ;
cnoremap / ;
lnoremap ? *
cnoremap ? *

lnoremap ; <nop>
cnoremap ; <nop>
lnoremap : <nop>
cnoremap : <nop>
lnoremap ;q @
cnoremap ;q @
lnoremap :q @
cnoremap :q @
lnoremap ;w ?
cnoremap ;w ?
lnoremap :w ?
cnoremap :w ?
lnoremap ;e %
cnoremap ;e %
lnoremap :e %
cnoremap :e %
lnoremap ;r `
cnoremap ;r `
lnoremap :r `
cnoremap :r `

lnoremap ;a #
cnoremap ;a #
lnoremap :a #
cnoremap :a #
lnoremap ;s :
cnoremap ;s :
lnoremap :s :
cnoremap :s :
lnoremap ;d "
cnoremap ;d "
lnoremap :d "
cnoremap :d "
lnoremap ;f '
cnoremap ;f '
lnoremap :f '
cnoremap :f '

lnoremap ;z ~
cnoremap ;z ~
lnoremap :z ~
cnoremap :z ~
lnoremap ;x ^
cnoremap ;x ^
lnoremap :x ^
cnoremap :x ^
lnoremap ;c !
cnoremap ;c !
lnoremap :c !
cnoremap :c !
lnoremap ;v $
cnoremap ;v $
lnoremap :v $
cnoremap :v $

lnoremap ;k <cr>
cnoremap ;k <cr>
lnoremap :k <cr>
cnoremap :k <cr>
lnoremap ;l <esc>
cnoremap ;l <C-c><esc>
lnoremap :l <esc>
cnoremap :l <C-c><esc>

lnoremap <C-a> A
cnoremap <C-a> A
lnoremap <C-b> B
cnoremap <C-b> B
lnoremap <C-c> C
cnoremap <C-c> C
lnoremap <C-d> D
cnoremap <C-d> D
lnoremap <C-e> E
cnoremap <C-e> E
lnoremap <C-f> F
cnoremap <C-f> F
lnoremap <C-g> G
cnoremap <C-g> G
lnoremap <C-h> H
cnoremap <C-h> H
lnoremap <C-i> I
cnoremap <C-i> I
lnoremap <C-j> J
cnoremap <C-j> J
lnoremap <C-k> K
cnoremap <C-k> K
lnoremap <C-l> L
cnoremap <C-l> L
lnoremap <C-m> M
cnoremap <C-m> M
lnoremap <C-n> N
cnoremap <C-n> N
lnoremap <C-o> O
cnoremap <C-o> O
lnoremap <C-p> P
cnoremap <C-p> P
lnoremap <C-q> Q
cnoremap <C-q> Q
lnoremap <C-r> R
cnoremap <C-r> R
lnoremap <C-s> S
cnoremap <C-s> S
lnoremap <C-t> T
cnoremap <C-t> T
lnoremap <C-u> U
cnoremap <C-u> U
lnoremap <C-v> V
cnoremap <C-v> V
lnoremap <C-w> W
cnoremap <C-w> W
lnoremap <C-x> X
cnoremap <C-x> X
lnoremap <C-y> Y
cnoremap <C-y> Y
lnoremap <C-z> Z
cnoremap <C-z> Z

lnoremap <M-a> 1
cnoremap <M-a> 1
lnoremap <M-s> 2
cnoremap <M-s> 2
lnoremap <M-d> 3
cnoremap <M-d> 3
lnoremap <M-f> 4
cnoremap <M-f> 4
lnoremap <M-g> 5
cnoremap <M-g> 5
lnoremap <M-h> 6
cnoremap <M-h> 6
lnoremap <M-j> 7
cnoremap <M-j> 7
lnoremap <M-k> 8
cnoremap <M-k> 8
lnoremap <M-l> 9
cnoremap <M-l> 9
lnoremap <M-;> 0
cnoremap <M-;> 0



noremap ; <nop>
noremap : <nop>
noremap ;l <esc>
noremap :l <esc>


noremap s :
noremap f /
noremap w <C-w>

noremap j gj
noremap k gk

noremap H b
noremap L e
noremap J }
noremap K {

noremap <C-h> ^
noremap <C-l> $
noremap <C-k> <C-o>
noremap <C-j> <C-i>

nnoremap U <C-r>


noremap e f
noremap E F

" https://stackoverflow.com/questions/23695727/vim-highlight-a-word-with-without-moving-cursor
noremap S *``

noremap ' "_x
nnoremap " "_dd
vnoremap " "_D

" https://vi.stackexchange.com/questions/2365/how-can-i-get-n-to-go-forward-even-if-i-started-searching-with-or
nnoremap <expr> n (v:searchforward ? 'n' : 'N')
nnoremap <expr> N (v:searchforward ? 'N' : 'n')

" https://vi.stackexchange.com/questions/19804/how-to-make-comma-and-semi-colon-direction-insensitive-of-the-latest-f-f-t-t-com
nnoremap <expr> m getcharsearch().forward ? ';':','
nnoremap <expr> M getcharsearch().forward ? ',':';'


" https://vi.stackexchange.com/questions/184/how-can-i-clear-word-highlighting-in-the-current-document-e-g-such-as-after-se
noremap <silent> ;n :noh<cr>

noremap ;v gv
noremap :v gv


noremap <M-a> 1
noremap <M-s> 2
noremap <M-d> 3
noremap <M-f> 4
noremap <M-g> 5
noremap <M-h> 6
noremap <M-j> 7
noremap <M-k> 8
noremap <M-l> 9
noremap <M-;> 0



" https://stackoverflow.com/questions/290465/how-to-paste-over-without-overwriting-register
xnoremap p pgvy
