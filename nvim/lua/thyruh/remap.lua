vim.g.mapleader = " "  -- Set the leader key to space

-- Open file explorer with <leader>pv
vim.keymap.set("n", "<leader>pv", function()
    vim.cmd("Ex")
end)

vim.keymap.set("n", "<leader><leader>", function()
    vim.cmd("so")
end)

vim.keymap.set("n", "<C-f>", "<cmd>silent !tmux neww tmux-sessionizer<CR>")

vim.keymap.set("n", "<C-u>", "<C-d>zz")
vim.keymap.set("n", "<C-i>", "<C-u>zz")

vim.keymap.set({"n", "v"}, "<leader>y", [["+y]])  -- Yanks selected text to system clipboard in normal and visual mode
vim.keymap.set("n", "<leader>Y", [["+Y]])  -- Yanks the whole line to system clipboard in normal mode

vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")

vim.keymap.set("n", "Q", "0")
vim.keymap.set("n", "E", "$")

