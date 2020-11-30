#!/bin/bash
set -ev

stack --no-terminal test --haddock --no-haddock-deps
