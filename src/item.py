#!/usr/bin/python3
# 
# Atlantis is a framework for creating multi-user dungeon worlds.
# This module takes care of the internal representation of all game items.
#
# Licensed under the terms of the GPLv3
# author: Daniel Vedder
# date: 10/05/2015
#

import copy
from define import DefineCommand, register_define_command


class Item(object):
    '''
    This is the actual Item class, which represents a game item.
    '''

    def __init__(self, name):
        pass


class ItemCommand(DefineCommand):
    '''
    The ATL construct to describe an item
    '''

    def __init__(self):
        super.__init__("define-item",
                       "Describe a new item that players can interact with")
        self.item = None
        # TODO Add option commands

    def init_object(self, item_name):
        self.item = None
        self.item = Item(item_name)

    def return_object(self):
        return self.item

register_define_command(ItemCommand())

# A list of all the items that have been defined in the game world
item_registry = dict()

def register_item(item):
    item_registry[item.name] = item

def get_item(self, item_name):
    '''
    Returns a copy of the item object stored under the passed name.
    This means that registry retains a "pure" instance of every item type
    for future reference.
    '''
    # is deepcopy the right copy method to use? if shallow copy is
    # sufficient, we could just use the inbuilt dict.copy()
    return copy.deepcopy(item_registry[item_name])        

