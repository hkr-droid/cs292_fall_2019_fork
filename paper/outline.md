__Report outline__

# Abstract 

# Intro
- what your problem is
	- writing a concurrent language is hard
	- reasoning about a concurrent languages is hard, too
- why is it interesting
- how 
- contributions

# Background
- cs170
	- synchronization what is it
	- what could be interesting, why care about my TA class
- programming languages lab
- midterm example in C
	- this is what we're going to try and mimic
	- basic test of what we want

# Implementation/Methodology

## DSL
- imp
- rosette needs it
- our version of midterm example 

## big step
- semantics to our syntax
- most straightforward, commonly presented, lots of resources
- p trivial tho

- what a thread is, our notion of context switching
	- implicit
	- vaguely guarded command like

## start over, small-step 
- needs abstract machines thank you, Ben
- secd -> cek -> cesk
- refactor interpretation function, machine state etc
- mod recursion for greater granularity
- in anticipation of threadedness, because at this point, identical to big step

## par and atomic operators
- semantics
	- just have structural operational to go on
	- non-obvious to translate to register-like syntax
	- our implementation, quite imperative (fancy: "transitive closure")
	- future direction: real deal continuation stacks for `par` and `atomic`
- Racket's call/cc 
	- is widely cited as a form this thing we're doing so why didn't we do it
	- host/obj lang weirdness
	- good for racket prog, maybe not for us

# Evaluation

## confirming our example program
- looks good!
- discussion about differences in end global state

## Rosette
- solve
	- symbolic values, where we can put 'em 
- verify
	- counter example
- synthesize :( 
	- this what we tried and what we want

# Conclusion

## future work

## discussion 
