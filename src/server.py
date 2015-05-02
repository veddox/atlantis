#!/usr/bin/python3
# 
# Atlantis is a framework for creating multi-user dungeon worlds.
# This is the server module which is in ultimately in charge of all game logic.
#
# Licensed under the terms of the GPLv3
# author: Daniel Vedder
# date: 02/05/2015
#

from parser import Parser

#TODO: lock file for the server!

class Server(object):
    '''
    This is the master server class in charge of setting up everything
    necessary for a game.
    '''

    def __init__(self, world_file=None):
        print("The server is still under construction!")
        if not world_file:
            print("ATLANTIS: which world file should be loaded?")
            print("Please provide an absolute or relative path.")
            world_file = input(">> ")
        parser = Parser(world_file)
        self.world = parser.generate_world()
