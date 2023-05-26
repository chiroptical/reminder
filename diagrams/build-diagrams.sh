#!/usr/bin/env bash

for f in $(ls diagrams/*.dot); do
  dot -Tsvg -O $f
done
