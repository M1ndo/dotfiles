set nocompatible              " be iMproved, required
filetype off                  " required

call plug#begin("~/.config/nvim/plugged")
Plug 'preservim/nerdcommenter'                     " Line Commenter
Plug 'nvim-treesitter/nvim-treesitter' 
Plug 'itchyny/lightline.vim'                       " Lightline statusbar
Plug 'frazrepo/vim-rainbow'
Plug 'johnstef99/vim-nerdtree-syntax-highlight'    " Highlighting NerdTree (Fixing Bug)  
" Plug 'tiagofumo/vim-nerdtree-syntax-highlight'     " Highlighting Nerdtree
Plug 'ryanoasis/vim-devicons'                      " Icons for Nerdtree
Plug 'jreybert/vimagit'                            " Magit-like plugin for vim
Plug 'tpope/vim-surround'                          " Change surrounding marks
Plug 'kovetskiy/sxhkd-vim'                         " sxhkd highlighting
Plug 'vim-python/python-syntax'                    " Python highlighting
Plug 'ap/vim-css-color'                            " Color previews for CSS
Plug 'junegunn/goyo.vim'                           " Distraction-free viewing
Plug 'junegunn/limelight.vim'                      " hyperfocus Writing ( With Or Without Goyo )
Plug 'junegunn/vim-emoji'                          " Vim needs emojis!
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'Yggdroot/indentLine' " Indent Line
Plug 'nvim-tree/nvim-web-devicons' " Icons
Plug 'romgrk/barbar.nvim' " Tabs
Plug 'cohama/lexima.vim' " Auto Complete Brackets Quotes .. 
" Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' } " Auto Completion For Lsp
Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }
Plug 'junegunn/fzf' 
Plug 'junegunn/vim-easy-align' " Easy Aligning
Plug 'jceb/vim-orgmode' " Org Mode Yaa Top G!
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim', { 'tag': '0.1.0' } " Search
Plug 'terryma/vim-multiple-cursors' " Multi Cursor OFC
Plug 'SirVer/ultisnips' " Snippets For Sure
" Plug 'honza/vim-snippets'
Plug 'M1ndo/vim-snippets', { 'branch' : 'Org-Add' } " My Snippets Patch
Plug 'majutsushi/tagbar' " Tag Bar *Why Not
Plug 'wellle/targets.vim' " Expanded Text Object Deletion (Not Sure abt the term?)
Plug 'ervandew/supertab' " Tab Sucks in insert mode
" Plug 'deoplete-plugins/deoplete-jedi' " Python Deoplete Jedi Plugin (LanguagClient Already Has One But Testing This iF it works?) (Works Good!, But LanguageClient Pylsp Is better)
" Plug 'liuchengxu/vim-which-key', { 'on': ['WhichKey', 'WhichKey!'] } " Which key
" Plug 'lukas-reineke/indent-blankline.nvim' " Indent Line
" Plug 'bluz71/vim-moonfly-colors', { 'branch': 'cterm-compat' } " Theme
" Plug 'dracula/vim', { 'as': 'dracula' } " Theme
call plug#end()

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Text, tab and indent related
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set path+=**					" Searches current directory recursively.
set wildmenu					" Display all matches when tab complete.
set incsearch                   " Incremental search
set hidden                      " Needed to keep multiple buffers open
set nobackup                    " No auto backups
set noswapfile                  " No swap
set t_Co=256                    " Set if term supports 256 colors.
set number relativenumber       " Display line numbers
set clipboard=unnamedplus       " Copy/paste between vim and other programs.
syntax enable
let g:rehash256 = 1
set laststatus=2
set noshowmode
set showmatch
set ai                          " set auto-indenting on for programming
" set smarttab                    " Be smart using tabs ;)
set expandtab                   " Use spaces instead of tabs.
set tabstop=2                   " One tab == four spaces.
set shiftwidth=2                " One tab == four spaces.
set softtabstop=2
"set cursorline                  "hilight the line of the cursor
"set cursorcolumn                "hilight the column of the cursor
set nowrap                      "no line wrapping
set foldenable
set foldmethod=indent           "folding by indent
set foldlevel=99

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => NERDTree
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Uncomment to autostart the NERDTree
" autocmd vimenter * NERDTree
nnoremap <C-n> :NERDTreeToggle<CR>
let g:NERDTreeDirArrowExpandable = '►'
let g:NERDTreeDirArrowCollapsible = '▼'
let g:NERDTreeShowLineNumbers=1
let g:NERDTreeShowHidden=1
let g:NERDTreeMinimalUI = 1
let g:NERDTreeWinSize=38

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Commenter
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:NERDSpaceDelims = 1

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Completion 
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Fix Lexima Comfliction With deoplete
let g:lexima_no_default_rules = 1
call lexima#set_default_rules()
call lexima#insmode#map_hook('before', '<CR>', '')

"function! s:my_cr_function() abort
"  return deoplete#close_popup() . lexima#expand('<CR>', 'i')
"endfunction

"inoremap <expr> <cr> <sid>my_cr_function()
" deoplate

"let g:deoplete#enable_at_startup = 1
" inoremap <silent><expr> <TAB>
" \ pumvisible() ? "\<C-n>" :
" \ <SID>check_back_space() ? "\<TAB>" :
" \ deoplete#manual_complete()
" function! s:check_back_space() abort "{{{
  " let col = col('.') - 1
  " return !col || getline('.')[col - 1]  =~ '\s'
" endfunction"}}}

let g:SuperTabMappingForward = '<s-tab>'
let g:SuperTabMappingBackward = '<tab>'

let g:LanguageClient_serverCommands = {
    \ 'sh': ['bash-language-server', 'start'],
    \ 'python': ['/home/alienx/.local/bin/pylsp'],
    \ }
nnoremap <Space>is :LanguageClientStart<CR>
nnoremap <Space>iS :LanguageClientStop<CR>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Colors and Theming
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
colorscheme My_Theme 
" colorscheme moonfly
" colorscheme dracula

" Transparency 
" let g:neovide_transparency=0.95
" let g:transparency = 0.8
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Open terminal inside Vim
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
map <A-t>t :term<CR>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Comment / Uncomment
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:NERDSpaceDelims = 1
map <Space>c[ <Plug>NERDCommenterToggle
map <Space>c] <Plug>NERDCommenterToggle
map <Space>c\ <Plug>NERDCommenterSexy

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Easy Align 
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
xmap ga <Plug>(LiveEasyAlign)
nmap ga <Plug>(LiveEasyAlign)

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Mouse Scrolling
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set mouse=nicr
set mouse=a

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Fix Sizing Bug With Alacritty Terminal
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
autocmd VimEnter * :silent exec "!kill -s SIGWINCH $PPID"

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Splits and Tabbed Files
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set splitbelow splitright

" Remap splits navigation to just CTRL + hjkl
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Custom KeyMaps 
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Map Space + L/U/D/R
map <Space><left> <C-w>h
map <Space><up> <C-w>k
map <Space><down> <C-w>j
map <Space><right> <C-w>l
map <A-q> <C-w>q

" Save And quit
nmap <Space>wq :wq<Enter>
nmap <Space>w :w<Enter>
nmap <Space>fs :w<Enter>
nmap <Space>q :q<Enter>
nmap <Space>qq :q!<Enter>

" Make adjusing split sizes a bit more friendly
noremap <silent> <C-Left> :vertical resize +3<CR>
noremap <silent> <C-Right> :vertical resize -3<CR>
noremap <silent> <C-Up> :resize +3<CR>
noremap <silent> <C-Down> :resize -3<CR>

" Change 2 split windows from vert to horiz or horiz to vert
map<Leader>th <C-w>t<C-w>H
map <Leader>tk <C-w>t<C-w>K

inoremap <C-Del> <C-o>dw 
" inoremap <C-BS> <C-w>
inoremap 1<BS> <C-w> 

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Goyo & LimeLight 
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nmap <Space>zz :Goyo<Enter>
nmap <Space>zt :Limelight<Enter>
nmap <Space>zT :Limelight!<Enter>
let g:goyo_width = "120x50"
let g:limelight_conceal_ctermfg = 'gray'
let g:limelight_conceal_ctermfg = 240
let g:limelight_default_coefficient = 0.7
" let g:limelight_paragraph_span = 0
" let g:limelight_bop = '^\s'
" let g:limelight_eop = '\ze\n^\s'

autocmd! User GoyoEnter Limelight
autocmd! User GoyoLeave Limelight!

" Removes pipes | that act as seperators on splits
set fillchars+=vert:\ 

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Python  
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:python_highlight_all = 1
let g:python_recommended_style = 0

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Org Mode  
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" No Settings Yet

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Telesope any better ??? 
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Mutli Cursor 
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:multi_cursor_use_default_mapping=0

" let g:multi_cursor_start_word_key      = '<C-n>'
" let g:multi_cursor_select_all_word_key = '<A-n>'
let g:multi_cursor_start_key           = 'gt'
let g:multi_cursor_select_all_key      = 'gty'
let g:multi_cursor_next_key            = '<tab>'
let g:multi_cursor_prev_key            = 'b'
let g:multi_cursor_skip_key            = 'x'
let g:multi_cursor_quit_key            = 'q'
nnoremap zm :MultipleCursorsFind<space>
xnoremap zm :MultipleCursorsFind<space>
xmap zI zm^<cr>I
xmap zA zm$<cr>A


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Indent Line 
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:indentLine_color_gui = "#504945"

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => status line / tabs
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" The lightline.vim theme
let g:lightline={ 'enable': {'statusline': 1, 'tabline': 0}, 'colorscheme': 'powerline' }
" Hide tabline
nnoremap <silent> <Space>bb <Cmd>set showtabline=0 <bar> BarbarDisable<CR>
nnoremap <silent> <Space>bB <Cmd>set showtabline=2 <bar> BarbarEnable<CR>
" Move to previous/next
nnoremap <silent>    <A-;> <Cmd>BufferPrevious<CR>
nnoremap <silent>    <A-'> <Cmd>BufferNext<CR>
" Re-order to previous/next
nnoremap <silent>    <Space>; <Cmd>BufferMovePrevious<CR>
nnoremap <silent>    <Space>' <Cmd>BufferMoveNext<CR>
" Goto buffer in position...
" Set Me When Using Tmux --
nnoremap <silent>    <Space>1 <Cmd>BufferGoto 1<CR>
nnoremap <silent>    <Space>2 <Cmd>BufferGoto 2<CR>
nnoremap <silent>    <Space>3 <Cmd>BufferGoto 3<CR>
nnoremap <silent>    <Space>4 <Cmd>BufferGoto 4<CR>
nnoremap <silent>    <Space>5 <Cmd>BufferGoto 5<CR>
nnoremap <silent>    <Space>6 <Cmd>BufferGoto 6<CR>
nnoremap <silent>    <Space>7 <Cmd>BufferGoto 7<CR>
nnoremap <silent>    <Space>8 <Cmd>BufferGoto 8<CR>
nnoremap <silent>    <Space>9 <Cmd>BufferGoto 9<CR>
nnoremap <silent>    <Space>0 <Cmd>BufferLast<CR>
" Pin/unpin buffer
nnoremap <silent>    <Space>p <Cmd>BufferPin<CR>
" Close buffer
nnoremap <silent>    <Space><Tab>d <Cmd>BufferClose<CR>
nnoremap <silent>    <Space>bd <Cmd>BufferClose<CR>
nnoremap <silent>    <Space>bc <Cmd>BufferCloseAllButCurrentOrPinned<CR>
nnoremap <silent>    <Space>bl <Cmd>BufferWipeout<CR>
" Magic buffer-picking mode
nnoremap <silent> <C-p>    <Cmd>BufferPick<CR>
" New Buffer
nnoremap <silent>    <Space><Tab><Tab> <Cmd>tabnew<CR>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Snippets 
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" <tab> => expand the snippets
let g:UltiSnipsExpandTrigger = '<tab>'
" <ctrl-d> => list available snippets start with the chars before the cursor
let g:UltiSnipsListSnippets = '<c-d>'
" <enter> => go to the next placeholder
let g:UltiSnipsJumpForwardTrigger = '<enter>'
" <shift-enter> => go to the previous placeholder
if exists('g:nyaovim_version')
  let g:UltiSnipsJumpBackwardTrigger = '<s-enter>'
else "as <shift-enter> can't be handled in terminal, use <ctrl-k> instead
  let g:UltiSnipsJumpBackwardTrigger = '<c-k>'
endif

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => TagBar 
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" tb => open the tagbar
nmap tb :TagbarToggle<CR>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Treesitter 
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
lua <<EOF
require'nvim-treesitter.configs'.setup {
  ensure_installed = { "bash", "python", "lua", "cpp", "c", "vim"},
  auto_install = true,
  highlight = {
    enable = true,              
},
}
EOF

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Highlight TODO/FIXME/BUG ...
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
hi Todolevel1 ctermbg=NONE guibg=red ctermfg=red guifg=white gui=bold cterm=bold
hi Todolevel2 ctermbg=NONE guibg=yellow ctermfg=yellow guifg=black gui=bold cterm=bold
hi Todolevel3 ctermbg=NONE guibg=green ctermfg=green guifg=white gui=bold cterm=bold
autocmd Syntax * call matchadd('Todolevel1',  '\W\zs\(FIXME\|XXX\|BUG\|HACK\|REPAIR\)')
autocmd Syntax * call matchadd('Todolevel2',  '\W\zs\(WARNING\|WARN\|CHANGED\|CHANGE\)')
autocmd Syntax * call matchadd('Todolevel3',  '\W\zs\(TODO\|NOTE\|DONE\)')

" => Which key
" nnoremap <silent> <leader> :WhichKey '<Space>'<CR>
"

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Neovide Configuration 
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if exists("g:neovide")
  set guifont=Cascadia\ Code:h12
  let g:neovide_refresh_rate = 220
  let g:neovide_scroll_animation_length = 0.3
  let g:neovide_transparency = 0.8
endif

"let g:neovide_transparency=0.95
" Remap ESC to ii
":imap ii <Esc>


