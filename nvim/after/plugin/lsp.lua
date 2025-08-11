-- Ensure cmp and lspconfig are available
local cmp = require('cmp')
local lspconfig = require('lspconfig')

-- Setup nvim-cmp for autocompletion
local cmp_select = { behavior = cmp.SelectBehavior.Select }
local cmp_mappings = {
  ['<M-p>'] = cmp.mapping.select_prev_item(cmp_select),
  ['<M-n>'] = cmp.mapping.select_next_item(cmp_select),
  ['<C-y>'] = cmp.mapping.confirm({ select = true }),
  ['<M-Space>'] = cmp.mapping.complete(),  -- Correct the <C-Space> mapping
}



cmp.setup({
  mapping = cmp_mappings,
  sources = {
    { name = 'nvim_lsp' },
  },
})

-- LSP setup function
local function on_attach(client, bufnr)
  local opts = { buffer = bufnr, remap = false }

  -- Setup keybindings
  vim.keymap.set("n", "gd", vim.lsp.buf.definition, opts)
  vim.keymap.set("n", "K", vim.lsp.buf.hover, opts)
  vim.keymap.set("n", "<leader>vws", vim.lsp.buf.workspace_symbol, opts)
  vim.keymap.set("n", "<leader>vd", vim.diagnostic.open_float, opts)
  vim.keymap.set("n", "[d", vim.diagnostic.goto_next, opts)
  vim.keymap.set("n", "]d", vim.diagnostic.goto_prev, opts)
  vim.keymap.set("n", "<leader>vca", vim.lsp.buf.code_action, opts)
  vim.keymap.set("n", "<leader>vrr", vim.lsp.buf.references, opts)
  vim.keymap.set("n", "<leader>vrn", vim.lsp.buf.rename, opts)
  vim.keymap.set("i", "<C-h>", vim.lsp.buf.signature_help, opts)
end

-- Setup LSP servers
lspconfig.pyright.setup({ on_attach = on_attach })
lspconfig.hls.setup({ on_attach = on_attach })
lspconfig.gopls.setup({ on_attach = on_attach })

require('lspconfig').clangd.setup({on_attach = on_attach,})


