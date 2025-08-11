vim.opt.guicursor = ""

vim.opt.clipboard = "unnamed,unnamedplus"

-- get rid of annoying tildas
vim.opt.fillchars = { eob = ' ' }

vim.opt.nu = true
vim.opt.relativenumber = true

vim.defer_fn(function()
   vim.o.guicursor = "n-v-c-i:block"
   vim.cmd("set guicursor+=a:blinkon30000-blinkoff3000-blinkwait500")
end, 100)

vim.opt.tabstop = 3
vim.opt.softtabstop = 3
vim.opt.shiftwidth = 3
vim.opt.expandtab = true

vim.opt.smartindent = true

vim.opt.wrap = false

vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undodir = os.getenv("HOME") .. "/.vim/undodir"
vim.opt.undofile = true

vim.opt.hlsearch = false
vim.opt.incsearch = true

vim.opt.termguicolors = true

vim.opt.scrolloff = 5
vim.opt.signcolumn = "yes"
vim.opt.isfname:append("@-@")

vim.opt.updatetime = 50

vim.opt.colorcolumn = "75"
