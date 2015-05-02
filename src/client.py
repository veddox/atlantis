#!/usr/bin/python3
# 
# Atlantis is a framework for creating multi-user dungeon worlds.
# This is the client module that the player interacts with.
#
# Licensed under the terms of the GPLv3
# author: Daniel Vedder
# date: 02/05/2015
#

class Client(object):
    '''
    The client is the interface between the player and the server.
    '''

    def __init__(self, server_ip=None, server_port=None):
        if not (server_ip and server_port):
            print("What server IP do you want to connect to?")
            server_ip = input(">> ")
            print("What port are you using?")
            server_port = input(">> ")
        print("The client would connect to "+server_ip+":"+server_port)
