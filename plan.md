# Elm

Elm is a functional reactive language that compiles to JavaScript with features like hot-swapping and time-travel debugging. Inspired by Haskell, it's a great way to learn function reactive programming at its purest.

We'll introduce the language and tooling, learn the basics of FRP with Elm and build a small application in the dojo session.

* Why should you care about Elm?
	* Establish that it's interesting
		* Learn FRP in a strict environment
		* Try out a Haskell-like language without having to worry about Functors and Monads
	* Demo with time-travel debugging (real app, not Mario thing?)
	* Hot code swapping
	* Emphasise that this is possible out of the box
* What makes this possible?
	* Functional programming, of course
* Elm
	* Haskell-like language, but may feel familiar if you've used ML, OCaml etc
	* More simple language
		* For example, no type constructors (Functor/Applicative/Monad-ness isn't exposed)
	* Focused on FRP
		* First class Signals and Channels
		* Designed for building interactive applications like games
	* Look at some ELm and the roughly equivalent JS
* Super-simple "hello world" app
	* Link Channel up to output
	* Show the reactor
