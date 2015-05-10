#!/usr/bin/python3
# 
# Atlantis is a framework for creating multi-user dungeon worlds.
# The world module saves the current state of the game world.
#
# Licensed under the terms of the GPLv3
# author: Daniel Vedder
# date: 02/05/2015
#

import copy
import place
from character import Race, CharacterClass, Player


class World(object):
    '''
    The World class saves and gives access to the current state
    of the game world.
    '''
    
    #   Originally it was intended that the world hold a copy of every game
    # object defined, and thus act as a meta-registry. However, that will
    # most likely lead to circular dependencies. (For example, CharacterClass
    # requires access to the item registry. If the latter was located here in
    # world.py, character.py would have to import world.py, which in turn
    # imports character.py...)
    #   Thus it seems better that each registry be moved to the module that
    # deals with the relevant game object. The item registry has already been
    # moved to item.py. The monster registry needs to follow suit. I am not
    # yet sure what to do with the place and NPC registries.

    def __init__(self):
        '''
        The constructor initializes dicts consisting of name:object pairs
        for all the places, players, NPCs, items and monsters available in
        the world.
        '''
        self.places = dict()
        self.players = dict()
        self.starting_place = None
        self.npc = dict()
        self.monsters = dict()
        
    def register_player(self, player):
        if player.name not in self.players.keys():
            if player.place == None:
                if self.starting_place:
                    player.place = self.starting_place
                else:
                    print("No starting place set!")
            self.players[player.name] = player
        else:
            print("Attempted to add a player that already exists!")

    def add_object(self, object_type, game_object):
        '''
        Add a game object to the world. Acts as a wrapper around add_place, etc.
        object_type: 'place', 'npc', 'item' or 'monster'
        game_object: the actual object
        '''
        if object_type == "place":
            self.add_place(game_object)
        elif object_type == "npc":
            self.add_npc(game_object)
        elif object_type == "monster":
            self.add_monster(game_object)
            
    def add_place(self, place):
        self.places[place.name] = place

    def add_monster(self, monster):
        self.monsters[monster.name] = monster

    def add_npc(self, npc):
        self.npc[npc.name] = npc

    def get_player(self, name):
        return self.players[name]

    def get_place(self, name):
        return self.places[name]
        
    def set_starting_place(self, place_name):
        self.starting_place = place_name

    def get_npc(self, npc_name):
        return self.npc[npc_name]


    # TODO Move to monster.py
    def get_monster(self, monster_name):
        '''
        Returns a copy of the monster object stored under the passed name.
        This means that World retains a "pure" instance of every monster type
        for future reference. Each Place holds its own monster objects that
        can be changed at will (e.g. in a fight).
        '''
        # is deepcopy the right copy method to use? if shallow copy is
        # sufficient, we could just use the inbuilt dict.copy()
        return copy.deepcopy(self.monsters[monster_name])
