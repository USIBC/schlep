#!/bin/sh

buildapp \
  --logfile buildlog \
  --output schlep \
  --asdf-tree . \
  --load-system schlep \
  --entry schlep:main
