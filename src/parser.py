#!/usr/bin/python3
# 
# Atlantis is a framework for creating multi-user dungeon worlds.
# This module reads in a world file and parses it.
#
# Licensed under the terms of the GPLv3
# author: Daniel Vedder
# date: 02/05/2015
#

class DefineCommand(object):
    '''
    This is the super class for all define commands. It should be extended
    for each individual command, like define-place, define-monster, etc.
    '''

    def __init__(self):
        pass


class Parser(object):
    '''
    The actual parser class. It reads in a world file and transforms it into
    a series of DefineCommands.
    '''

    def __init__(self, world_file):
        print("Loading world file '"+world_file+"'...")
