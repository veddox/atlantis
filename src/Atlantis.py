#!/usr/bin/python3
# 
# Atlantis is a framework for creating multi-user dungeon worlds.
#
# Licensed under the terms of the GPLv3
# author: Daniel Vedder
# date: 02/05/2015
#

VERSION = (0, 0, 1) #release, major revision, minor revision

import sys


def print_version():
    version_string = str(VERSION[0])+"."+str(VERSION[1])+"."+str(VERSION[2])
    print("Atlantis "+version_string)
    print("Licensed under the terms of the GNU General Public License v3.")

def print_help():
    print_version()
    print("\nHelp text not yet available.")

def main():
    if "--version" in sys.argv or "-v" in sys.argv: print_version()
    elif "--help" in sys.argv or "-h" in sys.argv: print_help()

if __name__ == "__main__":
    main()
