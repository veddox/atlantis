#!/usr/bin/python3
# 
# Atlantis is a framework for creating multi-user dungeon worlds.
#
# Licensed under the terms of the GPLv3
# author: Daniel Vedder
# date: 02/05/2015
#

VERSION = (0, 0, 5) #release, major revision, minor (git) revision

import sys
import os
from server import Server
from client import Client


global clear
if "linux" in sys.platform:
    clear = "clear"
elif "win" in sys.platform:
    clear = "cls"


def start_menu():
    '''
    Show the start menu and process the player's input
    '''
    global clear
    os.system(clear)
    try: #Print the Atlantis logo
        fn = os.path.join(os.path.dirname(__file__), "banner.txt")
        img_file = open(fn, "r")
        logo = img_file.read()
        img_file.close()
        print(logo)
    except IOError:
        print("Could not find logo!")
    print("Welcome! What do you want to do?")
    print(" -> (S)tart a server")
    print(" -> (J)oin game")
    print(" -> (A)bout")
    print(" -> (E)xit")
    print()
    choice = input("Please choose an option: ")
    act_on_choice(choice)    

#TODO!
def act_on_choice(choice):
    if choice == "s" or choice == "S":
        print("What port do you want to start the server on?") #TODO indicate range!
        server_port = input(">> ")
        print("What world file do you want to load?")
        print("(Specify an absolute or relative path)")
        world_file = input(">> ")
        Server(server_port, world_file)
    elif choice == "j" or choice == "J":
        print("What server do you want to connect to?")
        print("Format: <server IP>:<port>")
        server_address = input(">> ")
        server_ip = server_address[:server_address.find(":")]
        server_port = server_address[server_address.find(":")+1:]
        Client(server_ip, server_port)
    elif choice == "a" or choice == "A":
        print_version()
        print("(c) 2015 Daniel Vedder")
        input("\nPlease press ENTER")
        start_menu()
    elif choice == "e" or choice == "E":
        print("Goodbye!")
        exit()
    else:
        print("Invalid choice!")
        input("Please press ENTER")
        start_menu()


def print_version():
    version_string = str(VERSION[0])+"."+str(VERSION[1])+"."+str(VERSION[2])
    print("Atlantis "+version_string)
    print("Licensed under the terms of the GNU General Public License v3.")

def print_help():
    help_text = """
Usage: atlantis [options]

    --help      -h        Print this help text and exit
    --version   -v        Print the version number and exit
    --server <port>       Start an atlantis server on <port> (requires --world)
    --world <file>        Use <file> as the world file (requires --server)
    --client <ip>:<port>  Connect as a client to the server at <ip>:<port>

If no arguments are passed, atlantis starts an interactive interface.
"""
    print_version()
    print(help_text)

def main():
    if "--version" in sys.argv or "-v" in sys.argv: print_version()
    elif "--help" in sys.argv or "-h" in sys.argv: print_help()
    elif "--server" in sys.argv and "--world" in sys.argv:
        server_port = sys.argv[sys.argv.index("--server")+1]
        world_file = sys.argv[sys.argv.index("--world")+1]
        Server(server_port, world_file)
    elif "--client" in sys.argv:
        server_address = sys.argv[sys.argv.index("--client")+1]
        server_ip = server_address[:server_address.find(":")]
        server_port = server_address[server_address.find(":")+1:]
        Client(server_ip, server_port)
    else:
        start_menu()

if __name__ == "__main__":
    main()
