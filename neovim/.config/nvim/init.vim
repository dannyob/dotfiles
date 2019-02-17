"shhh! no bell or visualbell
set noerrorbells novisualbell t_vb=
let mapleader = " "
let localleader = "_"

" read/write a shared file, don't store items that are bigger than 100K.
set shada='20,s100

" Show status lines on all windows
set laststatus=2

" automatically put cuts into the clipboard
set clipboard+=unnamedplus
 
" Try to pick colors that look good on dark background
set background=dark

" Fancy listchars
set listchars=tab:»┄,eol:↲,trail:·,precedes:«,extends:»

" Minimize time waiting for leader and esc keys
set timeoutlen=1000 ttimeoutlen=0

" Super backspace!
set backspace=indent,eol,start

" Super indent!
set copyindent cindent smartindent

" ~ is an operator to me
set tildeop

" No two spaces after joining '.' etc.
set nojoinspaces

" Ignore some binary files
set wildignore=*.zip,*.gz,*.bz,*.tar
set wildignore+=*.jpg,*.png,*.gif,*.avi,*.wmv,*.ogg,*.mp3,*.mov,*.mp4

" Preferred tab settings
set tabstop=4 shiftwidth=4
set expandtab shiftround

" Searches
set incsearch   " Do incremental searching
set showmatch   " Make curson jump between brackets
set hlsearch    " Highlight searches
set ignorecase  " make searches ignore case, unless
set smartcase   " you actually put uppercase in the search

"  Switching buffers
set switchbuf=useopen,usetab,split

" See those s/replaces live
set inccommand=nosplit

set display=lastline,uhex " stop showing @@@@@ instead of last line
let html_use_css = 1  " use stylesheets when generating html

" Don't want tmp and undo files cluttering up directories
set directory=~/tmp,.,/var/tmp,/tmp
set undodir=~/tmp,.

set undofile

" Typos etc
abbrev ogr org
abbrev ahve have

" o  - open url under curson
map <Leader>o <Esc>:Utl<CR>
" d - insert todays date
map <Leader>d <Esc>i<C-R>=strftime("%Y-%m-%dT%H:%M%z")<CR>
" w
map <Leader>w <Esc>i<url:/home/danny/Private/wiki><Esc>h

" macro to turn multiple lines into one graf
map <Leader>A vapJ<ESC>o<ESC>j

" # = toggle comments on selected
map <Leader># <Esc>:'<,'>s/^/# /<CR>:'<,'>s/^# # //<CR>

" > = quote text
map <Leader>> <Esc>:'<,'>s/^/> /<CR>

" Copy to temp file
map <Leader>C <Esc>:'<,'>w! ~/tmp/temp<CR>
" Paste from temp file
"
map <Leader>P <Esc>:r ~/tmp/temp<CR>

" Turn on spelling
map <Leader>S <Esc>:setlocal spell spelllang=en_us<CR>

" ,F finds a "From" field in an email reply
"    and sets the To: to that line. Good for fixing
"    crummy mail forwards.
"
map <Leader>F ggj/From:<CR>wwy$0ggjwwD<Esc>jA<Esc>p<Esc>k"0p<Esc>}

" Mice!
"
set mouse=a
" Block/Line selection with mouse
noremap <C-LeftMouse> <4-LeftMouse>
inoremap <C-LeftMouse> <4-LeftMouse>
onoremap <C-LeftMouse> <C-C><4-LeftMouse>
noremap <C-LeftDrag> <LeftDrag>
inoremap <C-LeftDrag> <LeftDrag>
onoremap <C-LeftDrag> <C-C><LeftDrag>

noremap <S-LeftMouse> <3-LeftMouse>
inoremap <S-LeftMouse> <3-LeftMouse>
onoremap <S-LeftMouse> <C-C><3-LeftMouse>
noremap <S-LeftDrag> <LeftDrag>
inoremap <S-LeftDrag> <LeftDrag>
onoremap <S-LeftDrag> <C-C><LeftDrag>


" sudo make me a sandwich
cmap w!! %!sudo tee > /dev/null %

" turn me from markdown into docx
command! Wdoc !pandoc -i % -o %:r.docx -f markdown -t docx
command! Whtml !pandoc -i % -o %:r.html -f markdown -t html5
command! Wrtf !pandoc -i % -o %:r.rtf -s -f markdown -t rtf

" gpg options
let g:GPGPreferArmor=1
let g:GPGPreferSign=1
let g:GPGDefaultRecipients=["danny@spesh.com"]

" Some NeoVim stuff
if has('nvim')
    autocmd BufWinEnter,WinEnter term://* startinsert
    autocmd BufLeave term://* stopinsert
    tnoremap <Esc><Esc> <c-\><c-n>
    tnoremap <c-w> <c-\><c-n>
endif


set completeopt=menu,menuone,preview,longest
set number
set relativenumber
set cursorline
highlight LineNr ctermbg=black guibg=#000
highlight Normal ctermbg=black guibg=#000
highlight SignColumn ctermbg=black guibg=#000

set termguicolors
set guicursor=n-c:block,i-ci-ve:ver40,r-cr-v:hor20,o:hor50,a:blinkwait700-blinkoff400-blinkon250-Cursor/lCursor,sm:block-blinkwait175-blinkoff150-blinkon175

call plug#begin()
Plug 'eraserhd/parinfer-rust'
Plug 'jpalardy/vim-slime'
Plug 'tomtom/quickfixsigns_vim'
" gc == comment/uncomment
Plug 'tpope/vim-commentary' 
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
" <c-a> and <c-x>, d<c-x>, d<c-a> on dates
Plug 'tpope/vim-speeddating'
" cs, ys
Plug 'tpope/vim-surround'
" ]b. [b and friends
Plug 'tpope/vim-unimpaired'
Plug 'vimoutliner/vimoutliner'
Plug 'vim-scripts/utl.vim'
Plug 'gioele/vim-autoswap'
Plug 'unblevable/quick-scope'

" Lisp 'n' Guile 'n' Guix
" For all this Guix Info goodness
Plug 'hiphish/info.vim'
Plug 'guns/vim-sexp'
"(Really a fork of TobBrandt/vim-scripts )
Plug 'dannyob/vim-scripts' 
Plug 'junegunn/rainbow_parentheses.vim'
Plug 'tpope/vim-sexp-mappings-for-regular-people'
call plug#end()

" Plugin Setup
" jpalardy/vim-slime
" see slime-kitty
let g:slime_target = "neovim"

augroup rainbow_lisp
  autocmd!
  autocmd FileType lisp,clojure,scheme RainbowParentheses
augroup END

" dannyobrien/vim-scripts
" (Really a fork of TobBrandt/vim-scripts )

let g:guile_highlight_naming_conventions=1
let g:guile_highlight_base_library=1
let g:guile_highlight_std_libs=1
let g:guile_highlight_reader_extensions=1
let g:guile_highlight_goops=1
let g:guile_highlight_api=1
let g:guile_highlight_modules=1
