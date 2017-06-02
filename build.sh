#!/bin/sh

buildapp \
  --output schlep \
  --asdf-tree . \
  --load-system schlep \
  --entry schlep:main
