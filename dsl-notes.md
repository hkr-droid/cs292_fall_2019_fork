Okay, so here's a few notes on a simplified version of our DSL.

Here's the grammar:

```
Artihmetic expression  e ::= e + e | e * e | v | n 
Boolean expression     b ::= #t | #f | e = e | e <= e | ~b | b /\ b
Command                c ::= v := e | c : c | if b then c else c 
                         | while b do c end | load g v | store v g | lock | unlock
Thread-exec t ::= c | c []* c 
```

```
Maybe add asserts to Thread-exec like:

Thread-exec t  ::= c | c []* c | assert p 
Predicate   p  ::= e' = e' | e' < e' | true | false
Expression' e' ::= e' + e' | e' * e' | g | n 

The idea being terminate normally if p is true, else fail if false. 
This might be a way to specify what the programmer wants to be satisfied from the synthesized program.
```

```
Another possible construct is p -> c, that is, activate c from a state where p is true. Also, if c fi (activate c until is succeeds). The combination: 
if p -> c fi  has the semantics of "blocking if" that waits for p to be true and then executes c.

These constructs that do the syncronization can be part of a Thread-exec+ construct that the programmer doesn't need to mess with and is synthesized from what they specify in Thread-exec. 
```

With `v` a "thread-local" variable name such that: `Var(v) -> n`, 
and `g` is a shared global variable, similar to `Var` but exists outside of threads and all threads can access it: `Global(g) -> n` 
and `n` is an integer.

I made assigning "thread-local" variables distinct from manipulating shared global data (which is done through loads and stores) for the purpose of being able to show how the midterm question example could work in this DSL.

So we have simple arithmetic expressions, commands that are basically just sequences of assignments, plus lock/unlock. 

Then "thread" is just made up of commands or the nondeterministic execution of multiple threads (indicated by `[]*` which is kind of an extension of Dijkstra's fat bar except not fleshed out all the way). 

So here's an example to try to show what I mean by `[]*`, using the midterm question. 

Define the two threads, `t1`, `t2`:

```
t1 = load A x;
	 x := x + 1; 
	 store x A;

     lock; 

	 load A x;
	 x := x + 1; 
	 store x A;

	 load B x;
	 x := x + 1; 
	 store x B;

	 unlock

t2 = lock; 

	 load A x;
	 x := x + 1; 
	 store x A;

	 load B x;
	 x := x + 1; 
	 store x B;

	 unlock
```

Then, we execute `t1 []* t2`, given that `Global(A) -> 5` and `Global(B) -> 50`, and possibly get a trace like this: 

```
t1( load A x )   // load x with 5 
t2( lock )       // t2 interrupts and locks
t2( load A x; x := x + 1; store x A;  // Global(A) -> 6
    load B x; x := x + 1; store x B ) // Global(B) -> 51
t2( unlock )
t1( x := x + 1; store x A; ) // Global(A) -> 6
t1( lock )
t1( load A x; x := x + 1; store x A;  // Global(A) -> 7
    load B x; x := x + 1; store x B ) // Global(B) -> 52
t1( unlock )
```

Big missing piece is what the heck `[]*` means. It's like, "nondeterministically execute commands from these threads unless you are in a mutex region."
