"Setup
set noeol
set t_co=256

let g:pathogen_disabled= ["syntastic"]

"pathogen and plugins
filetype plugin indent on
execute pathogen#infect()
syntax on
"syntastic formatting
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_aggregate_errors = 1
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

"Specifying syntax checkers
let g:syntastic_python_checkers = ['python', 'flake8']
let g:syntastic_c_checkers = ['gcc']
let g:syntastic_sql_checkers = ['sqlint']

"Basic color setup & numbering
set number
colorscheme slate
set noeb
set backspace=2
let mapleader  = ";"
let maplocalleader = ","

"Bracketing and quotation help
inoremap ( ()<Esc>i
inoremap { {<cr><cr>}<esc>ki<tab>
inoremap [ []<Esc>i
inoremap " ""<Esc>i
iabbrev /* /**/<Esc>hi<space>
autocmd BufNewFile,BufRead MatchParen cterm=none ctermbg=green ctermfg=blue

"Folds
set foldmethod=indent
set foldlevel=99
nnoremap <space> za
set linebreak

"Pep8 guide formatting
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
set autoindent
set fileformat=unix

"easy split nav
set noerrorbells
set splitbelow
set splitright

"Shortcuts
nnoremap <leader>ev :15split $MYVIMRC<cr> " Edit vimrc
nnoremap <leader>lv :source $MYVIMRC<cr>  " Load vimrc
vnoremap <leader>wt '<i"''>"<esc> 
inoremap jj <esc>l:w<cr>
inoremap kj <esc>l
nnoremap <leader>fp :echo expand('%:p')<cr>  " show file path
nnoremap <leader>w <C-W><C-W>
nnoremap <C-J> <C-W><C-J> " Move vertical splits
nnoremap <C-K> <C-W><C-K>
nnoremap <C-H> <C-W><C-h> " Move Horizontal splits
nnoremap <C-L> <C-W><C-l>
nnoremap <leader>hc :set listchars=eol:$,tab:>-,trail:~,extends:>,precedes:< <cr> :set list <cr>
nnoremap <leader>hh :%!xxd<cr>
nnoremap <leader>hb ::%!xxd -r<cr>
nnoremap <leader>4 :buffers<CR>:buffer<Space>
nnoremap <leader>p :setlocal spell! spelllang=en_us<CR>
nnoremap <leader>c :close<CR>
nnoremap <leader>b :b
nnoremap <leader>dw :%s/\s\+$//e<cr>
nnoremap bn :bn<cr>
nnoremap bN :bp<cr>

"Spelling
iabbrev teh the
iabbrev THe The

"Window sizing
nnoremap <silent> <Leader>+ :exe "resize " . (winheight(0) * 3/2)<CR>
nnoremap <silent> <Leader>- :exe "resize " . (winheight(0) * 2/3)<CR>

"Remember line
if has("autocmd")
    au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif

"Nerd tree keybinding
nnoremap <leader>6 :NERDTreeToggle<cr>

"Disabling shitty keys to be better
nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>

"UTF-8 shortcuts
iabbrev sigma <c-v>u03c3
iabbrev mpi <c-v>u03A0
iabbrev -- <c-v>u2022
iabbrev -_ <c-v>u25E6
iabbrev -> <c-v>u2192
iabbrev <- <c-v>u2190
iabbrev -v <c-v>u2193
iabbrev -^ <c-v>u2191
iabbrev ^d <c-v>u0394
iabbrev bh **<space><esc>hhi  ** 

"airline-line
let g:airline#extensions#tabline#enabled = 1
set t_Co=256
set laststatus=2
let g:airline_theme='simple'
let g:bufferline_echo = 0
set noshowmode
let g:airline_powerline_fonts = 1

"Undotree
nnoremap <leader>5 :UndotreeToggle<cr>

"Pencil
let g:pencil#cursorwrap = 1
nnoremap <leader>np :NoPencil<cr>
nnoremap <leader>pe :SoftPencil<cr>
"Tagbar
nnoremap <leader>7 :TagbarToggle<cr>

"Org-mode
autocmd BufNewFile,BufRead *.org :set spell

"YCM
let g:ycm_global_ycm_extra_conf = "~/.vim/.ycm_extra_conf.py"
let g:ycm_python_binary_path = '/usr/bin/python3.5'
let g:ycm_auto_trigger = 1
"let g:ycm_add_preview_to_completeopt = 1 
let g:ycm_autoclose_preview_window_after_insertion = 1
let g:ycm_seed_identifiers_with_syntax = 1
autocmd BufNewFile,BufRead *.org :let g:ycm_seed_identifiers_with_syntax = 0 
autocmd BufNewFile,BufRead *.txt :let g:ycm_seed_identifiers_with_syntax = 0

"Eclim
let g:EclimCompletionMethod = 'omnifunc'
nnoremap <leader>je :Java % <cr>
nnoremap <leader>jp :JavaImport <cr>
