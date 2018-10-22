# tetrisforanothertime

## Run:

Build the project

`stack build`

Run it:

`stack exec tetrisforanothertime-exe [rows] [cols] [keyboard configuration]`

The keyboard configuration is either qwerty or dvorak (I use dvorak).
You enter either "Qwerty" or "Dvorak". This is case sensitive.

## Controls

If you are using the Qwerty keyboard, the controls are:

  * j <- Rotate piece clockwise
  * k <- Move piece to the right
  * d <- Move piece to the left
  * f <- Rotate piece counterclockwise
  * o <- Move piece down
  * q or ESC <- quit game
  * p <- Pause game
  * SPACE <- "Fastfall" pieces


For those weirdos out there that use Dvorak (this is mostly
self-deprecation), the Dvorak controls:

  * h <- Rotate piece clockwise
  * t <- Move piece to the right
  * e <- Move piece to the left
  * u <- Rotate piece counterclockwise
  * r <- Move piece down
  * q or ESC <- quit game
  * p <- Pause game
  * SPACE <- "Fastfall" pieces

## Some notes

~~This project is far from perfect, and there are corner cases that I
have yet to attend to. I'll clear them up in the future.~~

As of now, most of the core logic is sound. I could add extra
capabilities like increasing the speed at which blocks fall as your
score increases, adding menu options, restarting the game etc.
