colorscheme molokai
execute pathogen#infect()
map <C-n> :NERDTreeToggle<CR>
set number
set smartindent
set tabstop=4
set shiftwidth=4
set expandtab
set cursorline
set listchars=nbsp:¶,eol:¬,tab:>-,extends:»,precedes:«,trail:•
set list
set laststatus=2
set colorcolumn=80
set shortmess=a
set ruler
set t_Co=256
set noshowmode
set cmdheight=1

let g:airline_theme="dark"
highlight LineNr ctermfg=white

let g:EasyMotion_do_mapping = 0
nmap s <Plug>(easymotion-s)
nmap s <Plug>(easymotion-s2)

let g:EasyMotion_smartcase = 1
let g:airline_powerline_fonts = 1

map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)

hi CursorLine cterm=NONE ctermbg=black ctermfg=NONE guibg=darkgray guifg=white
highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+\%#\@<!$/

nmap j <up>
nmap k <down>

nmap ö :
