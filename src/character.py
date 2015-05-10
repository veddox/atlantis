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

import Item
from define import DefineCommand, register_define_command


class Race(object):
    '''
    A class to describe the races inhabiting the game world
    '''

    def __init__(self, name, description="",
                 str_bonus=0, dex_bonus=0,
                 int_bonus=0, const_bonus=0):
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

    def set_description(self, description):
        self.description = description

    def set_strength(self, str_bonus):
        self.bonuses['strength'] = str_bonus

    def set_dexterity(self, dex_bonus):
        self.bonuses['dexterity'] = dex_bonus

    def set_constitution(self, const_bonus):
        self.bonuses['constitution'] = const_bonus

    def set_intelligence(self, int_bonus):
        self.bonuses['intelligence'] = int_bonus


class DefineRace(DefineCommand):
    '''
    The ATL construct to define a race
    '''

    def __init__(self):
        super.__init__("define-race", "Define and describe a race of beings")
        self.race = None
        self.add_option("description", "A description of this race",
                        self.race.set_description)
        self.add_option("strength", "The strength bonus that this race gets",
                        self.race.set_strength)
        self.add_option("dexterity", "The dexterity bonus that this race gets",
                        self.race.set_dexterity)
        self.add_option("constitution", "The constitution bonus that this race gets",
                        self.race.set_constitution)
        self.add_option("intelligence", "The intelligence bonus that this race gets",
                        self.race.set_intelligence)

    def init_object(self, race_name):
        self.race = None
        self.race = Race(race_name)

    def return_object(self):
        return self.race

register_define_command(DefineRace())


class CharacterClass(object):
    '''
    A class to describe all the character classes a player can assume
    '''

    def __init__(self, name, description="", special_items=[]):
        '''
        Create a new character class. Arguments:
        name, description: the name and description of this character class
        special_items: a list of item instances that all players of this
            class carry from the start
        '''
        self.name = name
        self.description = description
        self.items = special_items

    def set_description(self, description):
        self.description = description

    def add_item(self, item_name):
        item = Item.get_item(item_name)
        self.items.append(item)


class DefineClass(DefineCommand):
    '''
    The ATL construct to define a character class
    '''

    def __init__(self):
        super.__init__("define-class", "Define and describe a character class")
        self.char_class = None
        self.add_option("description", "Describe this character class",
                        self.char_class.set_description)
        self.add_option("item", "An item that all members of this class carry",
                        self.char_class.add_item)

    def init_object(self, class_name):
        self.char_class = None
        self.char_class = CharacterClass(name)

    def return_object(self):
        return self.char_class

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

