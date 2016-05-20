set encoding=utf-8
set fileencoding=utf-8
set fileencodings=utf-8,euc-kr
set nu
set hls
set si
syntax on
set ts=8
set sts=4
set sw=4
" set et

set bg=dark

" mapping to make movements operate on 1 screen line in wrap mode 
function! ScreenMovement(movement) 
  if &wrap 
	 return "g" . a:movement 
  else 
	 return a:movement 
  endif 
endfunction 
onoremap <silent> <expr> j ScreenMovement("j") 
onoremap <silent> <expr> k ScreenMovement("k") 
onoremap <silent> <expr> 0 ScreenMovement("0") 
onoremap <silent> <expr> ^ ScreenMovement("^") 
onoremap <silent> <expr> $ ScreenMovement("$") 
nnoremap <silent> <expr> j ScreenMovement("j") 
nnoremap <silent> <expr> k ScreenMovement("k") 
nnoremap <silent> <expr> 0 ScreenMovement("0") 
nnoremap <silent> <expr> ^ ScreenMovement("^") 
nnoremap <silent> <expr> $ ScreenMovement("$") 

set nobackup

" For making backspace works
" set bs=2

" set mouse=nc
" set textwidth=70
