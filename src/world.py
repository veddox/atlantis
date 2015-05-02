#!/usr/bin/python3
# 
# Atlantis is a framework for creating multi-user dungeon worlds.
# The world module saves the current state of the game world.
#
# Licensed under the terms of the GPLv3
# author: Daniel Vedder
# date: 02/05/2015
#

class World(object):
    '''
    The World class saves and gives access to the current state
    of the game world.
    '''

    def __init__(self):
        self.places = []
        self.players = []
        self.starting_place = None

    def register_place(self, place):
        if self.place not in places:
            self.places.append(place)
        else:
            print("Attempted to add a place that already exists!")

    def register_player(self, player):
        if player not in self.players:
            self.players.append(player)
            if player.place == None:
                if self.starting_place:
                    player.place = self.starting_place
                else:
                    print("No starting place set!")
        else:
            print("Attempted to add a player that already exists!")

    def get_place(self, name):
        for p in self.places:
            if p.name == name:
                return p

    def get_player(self, name):
        for p in self.players:
            if p.name == name:
                return p

    def set_starting_place(self, name):
        for p in self.places:
            if p.name == name:
                self.starting_place = p

