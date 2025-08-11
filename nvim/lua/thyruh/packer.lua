--remap This file can be loaded by calling `lua require('pugins')` from your init.vim

-- Only required if you have packer configured as `opt`
vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function(use)
   -- Packer can manage itself
   use 'wbthomason/packer.nvim'

   use {
      'nvim-telescope/telescope.nvim', tag = '0.1.8',
      -- or                           , branch = '0.1.x',
      requires = { {'nvim-lua/plenary.nvim'} }
   }
   use {
      "rmagatti/auto-session",
      config = function()
         require("auto-session").setup {
            suppressed_dirs = { "~/", "~/Projects", "~/Downloads", "/" },
         }
      end,
   }

   use{ 
      'rose-pine/neovim',
      as = 'rose-pine',
      config = function()
         vim.cmd('colorscheme rose-pine')
      end
   }
   use('nvim-treesitter/nvim-treesitter', {run = 'TSUpdate'})	
   use('nvim-treesitter/playground')	
   use('thePrimeagen/harpoon')
   use('mbbill/undotree')
   use('tpope/vim-fugitive')

   use('nvim-lualine/lualine.nvim')

   use('folke/tokyonight.nvim')

   use {
      'VonHeikemen/lsp-zero.nvim',
      branch = 'v1.x',
      requires = {
         -- LSP Support
         {'neovim/nvim-lspconfig'},
         {'williamboman/mason.nvim'},
         {'williamboman/mason-lspconfig.nvim'},

         -- Autocompletion
         {'hrsh7th/nvim-cmp'},
         {'hrsh7th/cmp-buffer'},
         {'hrsh7th/cmp-path'},
         {'saadparwaiz1/cmp_luasnip'},
         {'hrsh7th/cmp-nvim-lsp'},
         {'hrsh7th/cmp-nvim-lua'},

         -- Snippets
         {'L3MON4D3/LuaSnip'},
         {'rafamadriz/friendly-snippets'},
      }
   }


   use({'ThePrimeagen/vim-be-good'})

   use({
      "stevearc/conform.nvim",
      "williamboman/mason.nvim",
      "williamboman/mason-lspconfig.nvim",
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-path",
      "hrsh7th/cmp-cmdline",
      "L3MON4D3/LuaSnip",
      "saadparwaiz1/cmp_luasnip",
      "j-hui/fidget.nvim",
   })
end)

