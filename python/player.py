#!/usr/bin/python3
# 
# Atlantis is a framework for creating multi-user dungeon worlds.
# The player module is in charge of everything to do with the internal
# representation of a player character.
#
# Licensed under the terms of the GPLv3
# author: Daniel Vedder
# date: 02/05/2015
#

class Player(object):
    '''
    The Player class represents a game character.
    '''

    def __init__(self, name, start_health,
                 character_race, character_class,
                 start_attributes):
        '''
        Create a player. Arguments:
        name - The name of the character
        start_health - The initial max HP
        character_race - a race instance the character belongs to
        character_class - a class instance that the character belongs to
        start_attributes - A dict containing the player attributes
            ('strength', 'constitution', 'dexterity', 'intelligence')
        '''
        self.name = name
        self.location = ""
        self.health = start_health
        self.attributes = start_attributes
        self.weapons = []
        self.items = []

