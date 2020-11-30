#!/bin/bash
set -ev

echo $TRAVIS_COMMIT_RANGE
exit 1
stack --no-terminal test --haddock --no-haddock-deps
