Mafia IRC Bot
=============

IRC bot for hosting a game of mafia.

Compling:
---------

You can use SBT to compile it:

    $ sbt package

Running the bot
---------------

You can run it either with a Scala distribution (Scala >=2.10.0 required):

    scala mafia_irc_bot_2.10-0.1.jar PARAMETERS

Or with Java (Java >=1.7 required):

    java -cp /opt/scala-2.10.0/lib/scala-library.jar:mafia_irc_bot_2.10-0.1.jar stasiak.karol.mafiaircbot.Main PARAMETERS

On Windows:

    java -cp C:\scala-2.10.0\lib\scala-library.jar;mafia_irc_bot_2.10-0.1.jar stasiak.karol.mafiaircbot.Main PARAMETERS

The paths to `scala-library.jar` and `mafia_irc_bot_2.10-0.1.jar` are of course examples. The Scala library has to be of version 2.10.x.

The parameters to the bot are as follows:

    -s SERVER         IRC server to connect to (required)
    -p PORT           unencrypted IRC port to connect to (default: 6667)
    -n NICKNAME       bot's nickname (required)
    -u USER_DETAILS   bot's user details (default: "The Great And Powerful Mafia Bot")
    -p PASSWORD       bot's password (optional)
    -A NICKNAME       nickname of a bot that authenticates users (optional)
    -M MESSAGE        message to send to that bot (required and allowed only if -A is used)
    -c CHANNEL        channel to join (required)
    -C PASSWORD       channel password (optional)

Using the bot
-------------

Bot responds to commands. Type `!mafiahelp` to the chat to get a list of supported commands.

License:
--------

Copyright (C) 2013 Karol Stasiak

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.