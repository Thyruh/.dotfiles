local builtin = require('telescope.builtin')

vim.keymap.set('n', '<leader>pf', builtin.find_files, { noremap = true, silent = true })
vim.keymap.set('n', '<C-p>', builtin.git_files, { noremap = true, silent = true })
vim.keymap.set('n', '<leader>hf', builtin.commands, { noremap = true, silent = true })
vim.keymap.set('n', '<leader>ps', function()
    builtin.live_grep()
end, { noremap = true, silent = true })

vim.keymap.set("n", "<leader>f", builtin.diagnostics, { desc = "Show workspace diagnostics" })
