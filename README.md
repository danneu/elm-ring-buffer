
# elm-ring-buffer

An Elm implementation of: <https://en.wikipedia.org/wiki/Circular_buffer>

Live demo: <https://www.danneu.com/elm-ring-buffer/>

----

A classic use-case is when you have a chatbox that should only hold the latest 100 messages. You can initialize a ring buffer with a capacity of 100 and then simply `push` into it as new messages arrive.

Oldest values will be evicted once capacity is reached.

More work could be done to make this library more performant than managing your own `Array`, but it's the ergonomics of the API that make this library useful.

## Development

### Tests

`npm test`

### Code

`npm start` and then visit <http://localhost:3000>.

Changes to `Main.elm` will be picked up.