#!/usr/bin/python3
# 
# Atlantis is a framework for creating multi-user dungeon worlds.
# This module houses the code for the define blocks of the Atlantis
# ATL language.
#
# Licensed under the terms of the GPLv3
# author: Daniel Vedder
# date: 04/05/2015
#


class DefineCommand(object):
    '''
    This is the super class for all define commands. It should be extended
    for each individual command, like define-place, define-monster, etc.

    Each define command works by creating the relevant object, then fleshing
    it out as more options are passed to it. Finally, the finished object is
    returned.
    '''

    def __init__(self, name, doc_string):
        self.name = name
        self.doc_string = doc_string
        self.option_registry = dict()

    def init_object(self, object_name):
        '''
        Initialize the object this command creates
        '''
        raise NotImplementedError

    def return_object(self):
        '''
        Return the type of the game object ('place', 'npc', 'item' or
        'monster') and the object itself
        '''
        raise NotImplementedError

    def pass_option(self, option_name, option_argument):
        '''
        Pass this define command one of its options and the relevant argument
        '''
        self.option_registry[option_name]["function"](option_argument)

    def add_option(self, option_name, option_docstring, option_function):
        '''
        Add an option for this define command. Arguments:
        option_name: The name of this option as it would appear in
                     an atl source file
        option_docstring: A description of this option
        option_function: The function that this option calls
                     (this function should take exactly one string argument)
        '''
        option_dict = dict(docstring=option_docstring, function=option_function)
        self.option_registry[option_name] = option_dict


define_command_registry = dict()

def register_define_command(def_com):
    define_command_registry[def_com.name] = def_com

def get_define_command(command_name):
    return define_command_registry[command_name]
