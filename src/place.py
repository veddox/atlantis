#!/usr/bin/python3
# 
# Atlantis is a framework for creating multi-user dungeon worlds.
# This module contains the code for a place (a delimited location
# in the world).
#
# Licensed under the terms of the GPLv3
# author: Daniel Vedder
# date: 02/05/2015
#

from parser import DefineCommand


class Place(object):
    '''
    A Place is one discrete location in the game world, which can contain
    players, monsters, items, NPCs, and links to other places.
    '''

    def __init__(self, name, description,
                 neighbours=[], monsters=[],
                 npc=[], items=[]):
        '''
        The constructor for a new place.
        Arguments:
        name: the name of this place
        description: a description string
        neighbours: a list of place names of places bordering on this one
        monsters: a list of instances of monsters
        npc: a list of instances of NPCs
        items: a list of instances of items
        '''
        self.name = name
        self.description = description
        self.neighbours = neighbours
        self.monsters = monsters
        self.npc = npc
        self.items = items

    def add_neighbour(self, place_name):
        if place_name not in self.neighbours:
            self.neighbours.append(place_name)

    def add_monster(self, monster):
        self.monsters.append(monster)

    def add_npc(self, npc):
        self.npc.append(npc)

    def add_item(self, item):
        self.items.append(item)

    def remove_neighbour(self, place_name):
        self.neighbours.remove(place_name)

    def remove_monster(self, monster):
        self.monsters.remove(monster)

    def remove_npc(self, npc):
        self.npc.remove(npc)

    def remove_item(self, item):
        self.items.remove(item)


class NeighbourOption(OptionCommand):
    '''
    Add a new neighbour to the current place
    '''

    def __init__(self, place):
        OptionCommand.__init__("neighbour",
                               "Add the place given in the argument as a neighbour")
        self.place = place

    def act(self, arg):
        self.place.add_neighbour(arg)


class DefinePlace(DefineCommand):
    '''
    The Atlantis language construct to create a place.
    '''

    def __init__(self, place):
        DefineCommand.__init__("define-place",
                               "Describe a new location in the game world")
        self.place = place
        self.add_option(NeighbourOption(self.place))
        
