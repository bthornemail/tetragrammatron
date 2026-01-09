#!/bin/bash
emacs --batch \
  --eval "(require 'org)" \
  --eval "(require 'ob-tangle)" \
  --eval "(setq org-confirm-babel-evaluate nil)" \
  --eval "(org-babel-tangle-file "genesis.org")"