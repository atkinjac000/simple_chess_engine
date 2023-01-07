# A Simple Chess Engine in Rust

A basic chess engine, just to explore Rust a little bit more. I don't really know
much at all about Rust or chess engines, so this should be fun.

## Initial Thoughts

- Find legal moves
	- As each move is found, rank it
- Stick each move in a priority queue, if it is already there don't add it again
- Pop the highest priority move off the top of the queue
- Clear the queue (This makes me think that I might want to use a tree or a map instead)
- Repeat until checkmate
