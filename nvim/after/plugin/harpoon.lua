vim.cmd([[packadd harpoon]])

-- Safely load 'harpoon.mark' and 'harpoon.ui'
local mark_ok, mark = pcall(require, "harpoon.mark")
if not mark_ok then
  print("Error loading harpoon.mark")
  return
end

local ui_ok, ui = pcall(require, "harpoon.ui")
if not ui_ok then
  print("Error loading harpoon.ui")
  return
end

-- Set up the key mappings
vim.keymap.set("n", "<leader>a", mark.add_file)
vim.keymap.set("n", "<C-e>", ui.toggle_quick_menu)

vim.keymap.set("n", "<C-h>", function() ui.nav_file(1) end)
vim.keymap.set("n", "<C-j>", function() ui.nav_file(2) end)
vim.keymap.set("n", "<C-k>", function() ui.nav_file(3) end)
vim.keymap.set("n", "<C-l>", function() ui.nav_file(4) end)

