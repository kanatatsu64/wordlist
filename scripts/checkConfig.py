#!/bin/python3

from runTest import ConfigFile
import helper

if __name__ == "main":
    config = helper.loadFile(ConfigFile)
    helper.validate(config)
