#!/usr/bin/python3
# 
# Atlantis is a framework for creating multi-user dungeon worlds.
# The character module is in charge of everything to do with the internal
# representation of a player character, including its race and class.
#
# Licensed under the terms of the GPLv3
# author: Daniel Vedder
# date: 02/05/2015
#

from define import DefineCommand, register_define_command

class Race(object):
    '''
    A class to describe the races inhabiting the game world
    '''

    def __init__(self, name, description,
                 str_bonus, dex_bonus,
                 int_bonus, const_bonus):
        '''
        Create a new race. Arguments:
        name, description: the name and description of the new race
        str_bonus, dex_bonus, int_bonus, const_bonus: attribute bonuses given 
            to all players of this race
        '''
        self.name = name
        self.description = description
        self.bonuses = {'strength':str_bonus, 'dexterity':dex_bonus,
                        'intelligence':int_bonus, 'constitution':const_bonus}

    # Do we need any methods here? I can't think of any at the moment...
    # If not, we might be able to turn this class into a simple list.


class DefineRace(DefineCommand):
    '''
    The ATL construct to define a race
    '''

    def __init__(self):
        super.__init__("define-race", "Define and describe a race of beings")
        #TODO

register_define_command(DefineRace())


class CharacterClass(object):
    '''
    A class to describe all the character classes a player can assume
    '''

    def __init__(self, name, description, special_items):
        '''
        Create a new character class. Arguments:
        name, description: the name and description of this character class
        special_items: a list of item instances that all players of this
            class carry from the start
        '''
        self.name = name
        self.description = description
        self.items = special_items

    # Again, do we need any methods here? If not, turn this class into a list.

class DefineClass(DefineCommand):
    '''
    The ATL construct to define a character class
    '''

    def __init__(self):
        super.__init__("define-class", "Define and describe a character class")
        #TODO

register_define_command(DefineClass())


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

