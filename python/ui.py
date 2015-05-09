#!/usr/bin/python3
# 
# Atlantis is a framework for creating multi-user dungeon worlds.
# This is the (text) UI module - all commandline IO should pass through here.
#
# Licensed under the terms of the GPLv3
# author: Daniel Vedder
# date: 02/05/2015
#


#
# DO NOT USE YET !!!
#

class IO(object):
    '''
    A wrapper to stdout, stderr and stdin.
    Intendend for development work only!
    '''

    def __init__(self):
        pass

    def error(self, error_text):
        pass
