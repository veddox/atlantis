#!/usr/bin/python3
# 
# Atlantis is a framework for creating multi-user dungeon worlds.
# This module reads in a world file and parses it. This is where the
# structure of the Atlantis language is defined.
#
# Licensed under the terms of the GPLv3
# author: Daniel Vedder
# date: 02/05/2015
#

from world import World


class OptionCommand(object):
    '''
    This is the super class for all option commands. (A define command
    includes one or more option commands.)
    '''

    def __init__(self, name, doc_string):
        self.name = name
        self.doc_string = doc_string

    def act(self, arg):
        '''
        Carry out this option commands action with argument arg.
        Has to be extended by subclasses!
        '''
        raise NotImplementedError


class DefineCommand(object):
    '''
    This is the super class for all define commands. It should be extended
    for each individual command, like define-place, define-monster, etc.
    '''

    def __init__(self, name, doc_string):
        self.name = name
        self.doc_string = doc_string
        self.option_registry = []

    def add_option(self, option_command):
        self.option_registry.append(option_command)


class Parser(object):
    '''
    The actual parser class. It reads in a world file and transforms it into
    a series of DefineCommands.
    '''

    def __init__(self, world_file):
        self.world_file = world_file

    def generate_world(self):
        return World()
