# tetrisforanothertime

## Run:

Build the project

`stack build`

Run it:

`stack exec tetrisforanothertime-exe [rows] [cols] [keyboard configuration]`

The keyboard configuration is either qwerty or dvorak (I use dvorak).
You enter either "Qwerty" or "Dvorak". This is case sensitive.

## Some notes

~~This project is far from perfect, and there are corner cases that I
have yet to attend to. I'll clear them up in the future.~~

As of now, most of the core logic is sound. I could add extra
capabilities like increasing the speed at which blocks fall as your
score increases, adding menu options, restarting the game etc.
