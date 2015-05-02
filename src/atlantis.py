#!/usr/bin/python3
# 
# Atlantis is a framework for creating multi-user dungeon worlds.
#
# Licensed under the terms of the GPLv3
# author: Daniel Vedder
# date: 02/05/2015
#

VERSION = (0, 0, 3) #release, major revision, minor (git) revision

import sys
from server import Server
from client import Client


def print_version():
    version_string = str(VERSION[0])+"."+str(VERSION[1])+"."+str(VERSION[2])
    print("Atlantis "+version_string)
    print("Licensed under the terms of the GNU General Public License v3.")

def print_help():
    help_text = """
Usage: atlantis [options]

    --help      -h        Print this help text and exit
    --version   -v        Print the version number and exit
    --server    -s        Start an Atlantis server
    --world <file>        Use <file> as the world file (implies --server)
    --client <ip>:<port>  Connect as a client to the server at <ip>:<port>

By default, atlantis starts a client instance and asks for all necessary input.
"""
    print_version()
    print(help_text)

def main():
    if "--version" in sys.argv or "-v" in sys.argv: print_version()
    elif "--help" in sys.argv or "-h" in sys.argv: print_help()
    elif "--world" in sys.argv:
        world_file = sys.argv[sys.argv.index("--world")+1]
        Server(world_file)
    elif "--server" in sys.argv or "-s" in sys.argv:
        Server()
    elif "--client" in sys.argv:
        server_address = sys.argv[sys.argv.index("--client")+1]
        server_ip = server_address[:server_address.find(":")]
        server_port = server_address[server_address.find(":")+1:]
        Client(server_ip, server_port)
    else:
        Client()

if __name__ == "__main__":
    main()
