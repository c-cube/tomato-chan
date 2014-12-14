tomato-chan
===========

IRC bot.

The main design goal is to divide the bot into:

- a stable _core_ that manages the connection to the IRC server, and manages
  plugins;
- plugins that are started by the core, and communicate with it through
  their stdin/stdout in JSON. They can be written in any language.


## License

BSD license.


