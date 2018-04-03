"Setup
set noeol
set t_co=256

let g:pathogen_disabled= []

set guifont=Knack\ Nerd\ Font:h14
"pathogen and plugins
filetype plugin indent on
execute pathogen#infect()
syntax on

"Basic color setup & numbering
set relativenumber
set number
let NERDTreeShowLineNumbers=1
colorscheme slate
set noeb
set backspace=2
let mapleader  = ";"
let maplocalleader = ","

" Airline
let g:airline#extensions#ale#enabled = 1

"Bracketing and quotation help
"inoremap { {<cr><cr>}<esc>ki<tab>
inoremap ( ()<Esc>i
inoremap [ []<Esc>i
inoremap " ""<Esc>i
iabbrev /* /**/<Esc>hi<space>
set showmatch

"Folds
set foldmethod=indent
set foldlevel=99
nnoremap <space> za
set linebreak

"Default formatting
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
set autoindent
set fileformat=unix
set tw=100

"easy split nav
set belloff=all
set splitbelow
set splitright

"Shortcuts
nnoremap <leader>ev :15split $MYVIMRC<cr> " Edit vimrc
nnoremap <leader>lv :source $MYVIMRC<cr>  " Load vimrc
noremap! <expr> ,t strftime("%H:%M")
inoremap jj <esc>l:w<cr>
inoremap kj <esc>l:w<cr>
inoremap <esc> <esc>l
nnoremap <leader>w <C-W><C-W>
nnoremap <C-J> <C-W><C-J> " Move vertical splits
nnoremap <C-K> <C-W><C-K>
nnoremap <C-H> <C-W><C-h> " Move Horizontal splits
nnoremap <C-L> <C-W><C-l>
nnoremap <leader>hc :set listchars=eol:$,tab:>-,trail:~,extends:>,precedes:< <cr> :set list <cr>
nnoremap <leader>hh :%!xxd<cr>
nnoremap <leader>hb ::%!xxd -r<cr>
nnoremap <leader>p :setlocal spell! spelllang=en_us<CR>
nnoremap <leader>c :close<CR>
nnoremap <leader>dw :%s/\s\+$//e<cr><esc>

"Window sizing
nnoremap <silent> <Leader>+ :exe "resize " . (winheight(0) * 3/2)<CR>
nnoremap <silent> <Leader>- :exe "resize " . (winheight(0) * 2/3)<CR>

"Remember line
if has("autocmd")
    au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif

"UTF-8 Characters
iabbrev -- <c-v>u2022
iabbrev -_ <c-v>u25E6
iabbrev -> <c-v>u2192
iabbrev <- <c-v>u2190
iabbrev -v <c-v>u2193
iabbrev -^ <c-v>u2191
iabbrev ^d <c-v>u0394
iabbrev bh **<space><esc>hhi  **

" General Prose editing
iabbrev teh the
iabbrev THe The
nmap ]s ]sz=
nmap [s [sz=
set pastetoggle=<leader>v

"airline-line
let g:airline#extensions#tabline#enabled = 1
""set t_Co=256
set laststatus=2
let g:bufferline_echo = 0
set noshowmode
let g:airline_powerline_fonts = 1

"Undotree
nnoremap <leader>6 :UndotreeToggle<cr>

"Nerd tree keybinding
nnoremap <leader>5 :NERDTreeToggle<cr>

"Buffregator
nnoremap <leader>4 :BuffergatorToggle<cr>

"YCM
let g:ycm_global_ycm_extra_conf = "~/.vim/.ycm_extra_conf.py"
let g:ycm_python_binary_path = '/usr/bin/python3.5'
nnoremap <leader>fi YcmCompleter FixIt <cr>
let g:ycm_auto_trigger = 1
let g:ycm_autoclose_preview_window_after_insertion = 1
let g:ycm_seed_identifiers_with_syntax = 1

"AUTO COMMAND GROUPS"
" System wide
autocmd InsertLeave * set relativenumber
autocmd InsertEnter * set norelativenumber

augroup filetype_java
    nnoremap <leader>je :Java % <cr>
    nnoremap <leader>jp :JavaImport <cr>
augroup end

"Org-mode
augroup filetype_org
    autocmd!
    autocmd FileType org set spell
    autocmd FileType org let g:ycm_seed_identifiers_with_syntax = 0
    autocmd FileType org let g:ycm_auto_trigger = 4
augroup END

"Text"
augroup filetype_text
    autocmd!
    autocmd FileType txt let g:ycm_seed_identifiers_with_syntax = 0
augroup END

"Ruby"
augroup filetype_ruby
    autocmd!
    autocmd FileType ruby setlocal ts=2 sts=2 sw=2 expandtab
    autocmd FileType ruby let g:rubycomplete_buffer_loading =1
    autocmd FileType ruby let g:rubycomplete_rails = 1
augroup END

"C"
augroup filetype_c
    autocmd!
    autocmd FileType c inoremap { {<cr><cr>}<esc>ki<tab>
augroup END
"
"Kotlin"
augroup filetype_kotlin
    autocmd!
    autocmd FileType kotlin imap " "
augroup END

let g:rainbow_conf = {
	\	'guifgs': ['royalblue3', 'darkorange3', 'seagreen3', 'firebrick'],
	\	'ctermfgs': ['lightblue', 'lightyellow', 'lightcyan', 'lightmagenta'],
	\	'operators': '_,_',
	\	'parentheses': ['start=/(/ end=/)/ fold', 'start=/\[/ end=/\]/ fold', 'start=/{/ end=/}/ fold'],
	\	'separately': {
	\		'*': {},
	\		'tex': {
	\			'parentheses': ['start=/(/ end=/)/', 'start=/\[/ end=/\]/'],
	\		},
	\		'lisp': {
	\			'guifgs': ['royalblue3', 'darkorange3', 'seagreen3', 'firebrick', 'darkorchid3'],
	\		},
	\		'vim': {
	\			'parentheses': ['start=/(/ end=/)/', 'start=/\[/ end=/\]/', 'start=/{/ end=/}/ fold', 'start=/(/ end=/)/ containedin=vimFuncBody', 'start=/\[/ end=/\]/ containedin=vimFuncBody', 'start=/{/ end=/}/ fold containedin=vimFuncBody'],
	\		},
	\		'html': {
	\			'parentheses': ['start=/\v\<((area|base|br|col|embed|hr|img|input|keygen|link|menuitem|meta|param|source|track|wbr)[ >])@!\z([-_:a-zA-Z0-9]+)(\s+[-_:a-zA-Z0-9]+(\=("[^"]*"|'."'".'[^'."'".']*'."'".'|[^ '."'".'"><=`]*))?)*\>/ end=#</\z1># fold'],
	\		},
	\		'css': 0,
	\	}
	\}
