# 4chan-bitlbee

This a plugin for bitlbee to access the threads in a 4chan board as an IRC
channel.

To build, use the provided Makefile, you are going to need to install Haskell's
stack and the headers of bitlbee.

To install, just do "make install" as root.


# Commands

In the main channel of bitlbee you can use this two commands to list and enter a
thread:
  * catalog BOARD: list the current threads in a board
  * jthread BOARD NUMBER: to join a thread, a new IRC channel is going to be created.

After doing jthread, you need to join the IRC channel with /join #NUMBER
