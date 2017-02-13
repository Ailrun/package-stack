#!/usr/bin/env bash

curl -fsSkL https://raw.github.com/rejeep/evm/master/go | bash

export PATH="$HOME/.evm/bin:$PATH"
export PATH="$HOME/.cask/bin:$PATH"

evm config path /tmp
