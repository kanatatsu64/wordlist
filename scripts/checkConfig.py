#!/bin/python3

from runTest import ConfigFile
import helper

if __name__ == "__main__":
    config = helper.loadFile(ConfigFile)
    if helper.validate(config):
        print("no problem is found")
    else:
        print("config file is invalid")
