# trivial-client
This branch has a minimal implementation
It creates a socket and creates a connection and then closes the connection.

This branch includes exception handling for the setup of the socket and connection

The acquisiton and release of the socket are done by using Control.Exception.Bracket.

Control.Exception.Bracket will execute the release function and rethrow the exception.

So, now it's a good opportunity to use the Control.Exception.Handle function to catch the exceptions
do something with the exception...  In this case display a message and then die.

This branch now includes the ability to read and write...  

The socket is converted to a handle

A receive loop is created

The user entry is done on the main thread

