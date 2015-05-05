#!/usr/bin/python3
# 
# Atlantis is a framework for creating multi-user dungeon worlds.
# This is the server module which is in ultimately in charge of all game logic.
#
# Licensed under the terms of the GPLv3
# author: Daniel Vedder
# date: 02/05/2015
#

from interpreter import Parser
from world import World

#TODO: lock file for the server!

class Server(object):
    '''
    This is the master server class in charge of setting up everything
    necessary for a game.
    '''

    def __init__(self, port, world_file):
        print("The server is still under construction!")
        self.port = port
        self.world_file = world_file
        parser = Parser(self.world_file)
        self.world = parser.load(self.world_file)
        self.test_parser()

    def test_parser(self):
        print("World loaded. Details:")
        places = self.world.places.keys()
        for p in places:
            print("Place: "+self.world.get_place(p).name)
            print("Neighbours: "+str(self.world.get_place(p).neighbours))
        print("Starting place: "+self.world.starting_place)
