#!/bin/python3
import os
import helper
import gitDiff

# config.yml
# {
#   name(string): {
#     test: regexp(/string/),
#     script: command(string)
#   }
# }

ConfigFile = "scripts/config.yml"

Env = object()
Env.commit = os.environ['TRAVIS_COMMIT']
Env.commitRange = os.environ['TRAVIS_COMMIT_RANGE']
Env.branch = os.environ['TRAVIS_BRANCH']
Env.pullRequest = os.environ['TRAVIS_PULL_REQUEST']
Env.pullRequestBranch = os.environ['TRAVIS_PULL_REQUEST_BRANCH']
Env.tag = os.environ['TRAVIS_TAG']
Env.stage = os.environ['TRAVIS_BUILD_STAGE_NAME']

if __name__ == "main":
    config = helper.load(ConfigFile)
    files = gitDiff.getFiles(Env)
    scripts = []
    if files == None:
        scripts = helper.getAllScripts(config)
    else:
        scripts = helper.getScripts(config, files)
    helper.execScripts(scripts)
