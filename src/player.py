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

    def change_location(self, new_place):
        self.location = new_place

    def change_health(self, difference):
        self.health = self.health+difference

    def increase_attribute(self, attribute, difference):
        self.attributes[attribute] = self.attributes[attribute] + difference

    def add_item(self, item):
        self.items.append(item)

    # Warning: might cause bugs!
    def remove_item(self, item):
        self.items.remove(item)

    def add_weapon(self, weapon):
        self.items.append(weapon)

    # Warning: might cause bugs!
    def remove_weapon(self, weapon):
        self.items.remove(weapon)

