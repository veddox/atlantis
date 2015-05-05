#!/usr/bin/python3
# 
# Atlantis is a framework for creating multi-user dungeon worlds.
# This module reads in a world file and parses it. The structure of 
# the Atlantis language ATL is defined here.
#
# Licensed under the terms of the GPLv3
# author: Daniel Vedder
# date: 02/05/2015
#

from world import World
from define import DefineCommand, define_command_registry


class Parser(object):
    '''
    The actual parser class. It reads in a world file and transforms it into
    a series of DefineCommands, which are then executed
    '''

    def __init__(self, world_file_name):
        self.line_command_registry = dict()
        self.world = World()
        self.add_line_commands()

    def load(self, world_file_name):
        '''
        Load a world file and pass it on to the interpreter
        '''
        try:
            world_file = open(world_file_name, 'r')
            atl_text = world_file.readlines()
            world_file.close()
            print("Loaded "+world_file_name)
        except IOError:
            print("Failed to load world file '"+world_file_name+"'!")
        self.interpret_atl(atl_text)
        return self.world

    def add_line_commands(self):
        '''
        Line commands are ATL constructs that only take up one line
        (unlike define commands). All line commands must be registered in
        this method by adding them to the line_command_registry and setting
        their key-value to the method to be called when their key appears
        in an ATL source file.
        '''
        self.line_command_registry["load"] = self.load
        self.line_command_registry["start-place"] = self.world.set_starting_place

    def interpret_atl(self, atl_source):
        '''
        This method interprets a list of ATL source lines, passing them
        on to the relevant define commands.
        '''
        define_command = None # The define command currently being executed
        line_no = 0
        for line in atl_source:
            # TODO allow for linebreaks
            # while line[-2] == "\\":
            #     line = line + atl_source[line_no+1]
            #     line_no = line_no+1
            line_no = line_no + 1
            if len(line) < 2 and define_command:
                # Empty lines finish off define blocks
                object_type, game_object = define_command.return_object()
                self.world.add_object(object_type, game_object)
                define_command = None
            elif len(line) < 2 or line[0] == "#":
                pass #comments and empty lines are ignored
            else:
                command = line.split()[0]
                print("Line "+str(line_no)+": '"+line+"'\nCommand: '"+command+"'")
                # execute a line command
                if command in self.line_command_registry.keys():
                    self.line_command_registry[command](line[line.find(" ")+1:-1])
                # start of a define block
                elif command in define_command_registry.keys():
                    define_command = define_command_registry[command]
                    object_name = line[line.find(" ")+1:-1]
                    define_command.init_object(object_name)
                    print("Found define command '"+define_command.name)
                # parse an option command
                elif line[0] == " " or line[0] == "\t":
                    while line[0] == " " or line[0] == "\t":
                        line = line[1:]
                    if line and define_command:
                        option_command = line.split()[0]
                        option_arg = line[line.find(" ")+1:-1]
                        define_command.pass_option(option_command, option_arg)
                    else:
                        # XXX: What should be done here? Do nothing, raise a
                        # syntax error, or something else?
                        print("Unrecognized syntax in line "+str(line_no)+"!")

