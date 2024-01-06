---
layout: post
title: "Confessions of a functional programming apostate"
toc: true
---

> The venerable master Qc Na was walking with his student, Anton. Hoping to
> prompt the master into a discussion, Anton said "Master, I have heard that
> objects are a very good thing --- is this true?" Qc Na looked pityingly at
> his student and replied, "Foolish pupil --- objects are merely a poor man's
> closures."
> 
> Chastised, Anton took his leave from his master and returned to his cell,
> intent on studying closures. He carefully read the entire "Lambda: The
> Ultimate..." series of papers and its cousins, and implemented a small Scheme
> interpreter with a closure-based object system. He learned much, and looked
> forward to informing his master of his progress.
> 
> On his next walk with Qc Na, Anton attempted to impress his master by saying
> "Master, I have diligently studied the matter, and now understand that
> objects are truly a poor man's closures." Qc Na responded by hitting Anton
> with his stick, saying "When will you learn? Closures are a poor man's
> object." At that moment, Anton became enlightened.
> 
> --- Anton van Straaten

I used to love pure functional programming.

In fact, I was a functional programming *evangelist*. All mutable state was
inherently incomprehensible and impossible to properly reason about, an evil
that must be banished from the code base. All side effects should be safely
sequestered behind an `IO` monad. Monads and functors were the coolest shit
around, and further abstraction via powerful type systems was the path to
solving all our problems. Pure functional programming was the ultimate end
game, the best possible method of designing software, the next wave, the end of
history. I loved showing people the pure beauty of Haskell:

```haskell
qsort1 []     = []
qsort1 (p:xs) = qsort1 lesser ++ [p] ++ qsort1 greater
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs
```

And the mind-boggling power of its type system:

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
    (<$) :: a -> f b -> f a
```

I still do, in a lot of ways. One of the first things I ever showed my
now-girlfriend, when we first broached the subject of programming on our
informal dates, was that Haskell code! And I've talked about monads and
functors so much now she knows to roll her eyes whenever I start.

But at some point, I stopped pushing for pure functional programming as the
be-all end all of code styles, the end of software development history, the
only true way to program.

It started when I realized that as much as I enjoyed writing a clever
functional program and learning about type theory... when I sat down to *write
shit*, to *get shit done*, I never did pure functional programming. I simply
couldn't bring myself to practice what I preached, despite enjoying it in
theory, because there was a taxing mental overhead to all the code-golfing and
abstraction trickery needed to make pure functional programming work, and that was
too great a price to pay when the problem I was working on was already complex
and I just wanted to sit down and get stuff done. All this abstraction and
code-golfing to get pure functional programming with no mutable state and side
effects to work....  wasn't getting work done. It was doing the same
architecture astronaut bullshit I made fun of object-oriented programmers for.
It made for less easily grokkable code that was less closely mapped to the
underlying problems at hand. And whenever I did write pure FP and tried to go
back to it later, it was unreadable.

So I started preferring to program in a more procedural way. I started
criticizing pure functional programming, and looking on excessive use of it
with annoyance and exhaustion. **I became a functional programming apostate.**

And I stopped buying into programming ideologies.

Now of course pure functional programming cargo cultists will come after me for
this. They'll say that I just didn't understand pure functional programming,
that I just *wasn't good enough at it*, that if I truly trained with it enough,
I would get so good at it that reading even the densest thicket of recursion
and abstraction would be like second nature to me, that imperative programming
doesn't *really* map better to what the CPU does, or to most algorithms or
problem spaces, or to the natural human mental model of how the world works,
that that's just my ingrained programming instruction talking and that [if I'd
grown up being taught Haskell instead of Scheme, Lua, and
JavaScript](https://www.huffpost.com/entry/darmond-speers-dad-spoke_n_363477)
it would all melt before my eyes into easily readable code.

I'm going to completely ignore this, because it's completely unprovable and
unfalsifiable. It's a combination of shifting the goal posts and no true
Scotsman. It's also what every language and programming ideology person says
when challenged: C++ programmers insist memory safety errors are just a "skill
issue," despite even the best and most knowledgeable and experienced C++
programmers regularly producing memory safety errors; OOP fanatics claim design
patterns are good actually and you're just not good enough at it, and so on.
This is a common tactic, and I'm not going to fall for it.

Especially since I have a deeper philosophical bone to pick with pure
functional programming.

## Ideologies versus worldviews

To my mind, an ideology is not just a coherent set of beliefs that guide how
you interpret the world and structure how you think about things; instead, it
is a *totalizing system*, a metanarrative that admits no exceptions to its
rules, no knowledge it doesn't have, no possible valid difference of opinion or
perspective, no contextual situations and knowledge, and dominates the person
who holds it, so that *even if it would be better for them to drop it*, they
remain beholden to it.

## Worldviews

Yes, everyone needs a framework for interpreting the world and a set of values
or rules of thumb for approaching things. Facts don't just present themselves
somehow already freighted with information about their importance, their
meaning, and their implications for the larger whole, so we have to look at our
experiences, and listen to the experiences of others, and piece together what
we think it all means. This is perfectly fine, and I'm not saying you shouldn't
do this, that you should somehow exist in a Pyrrhonic state of suspension of
belief at all times. Building your worldview, and even sticking by it, having
confidence in it, to a certain degree, is reasonable and good.

What you have to keep in mind at all times, however, is that your worldview is
inherently limited, because the only data we have access to is our own
experiences. We can't step outside of our own perspectives and get the complete
and unfiltered truth of any given matter, because we aren't ethereal spirits.
We will always have a particular history, a particular place in the world,
a particular range of knowledge, and even if we could step outside of those, we
would always have particular biases, assumptions, preferences, and
personalities to filter whatever we see. Yes, we can also consult with other
people, try to get their experiences and integrate them into our own worldview,
but even that is inherently filtered through our own perspective: you don't
actually have access to the raw experiences of others, you only have access to
their *claims* to have experiences, and seeing something with your own two eyes
is different than only being told about something. They could be lying, they
could be mistaken, you could disagree with them about what those experiences
mean --- was that DMT trip just a trip, or were they really communing with
higher dimensional beings?

And then there's the problem of actually building a worldview out of that
perspectival data: theories are always underdetermined by their data! There are
an *infinite* number of ways to interpret anything, and usually more than one
semi-reasonable way, too. Moreover, we construct worldviews to serve specific
needs for ourselves, and different people's needs can be different, so what
might be a totally sensible worldview for one person might be untenable or
useless for another. In fact, not even the definition of truth is itself
agreed-upon! There are a million different ways of understanding what truth
means, because truth is not some extant ontological entity, but an idea that we
use, like any other component of our worldviews, for its use-factor. Many
people might say truth is what "corresponds to reality," but I would disagree,
since we can never step outside our own heads and perspectives in order to
carefully compare the propositions we have in our heads to some list of
propositions inscribed in stone somewhere to see how well they match, so that
definition is completely useless and nonsensical. So, ultimately, there are no
facts, only interpretations, and there isn't even always central, authoritative
rulebook for judging those interpretations! 

Crucially, this doesn't mean that all interpretations are of equal value,
though. All worldviews share some common goals, like usefully guiding action
and predicting reality, and even within someone's perspective some theories can
be worse or better at those things, or even inconsistent, nonsensical, or
poorly reasoned. Additionally, worldviews might be too narrow, unable to
integrate and account for new information, and thus useless if the person who
made them steps even a little outside their comfort zone, which limits their
range of action and understanding. They can even be harmful either to the
person who holds them or those around them.

A proper worldview is aware of these limitations, accepts them, and is
built in a manner that explicitly adapts to them, by seeking out other
people's perspectives and experiences in order to make your worldview more
useful by being more flexible, broader, and more able to be communicated
and exist in communication with others different from you. You don't have
to agree with or even take seriously or everything you hear, and you
certainly don't need to waste time listening to worldviews you've already
found useless to you or bad, but building an intersubjective understanding
of the world with others is an important endeavor.

### Ideologies

Ideologies are fundamentally opposed to this process, and are therefore
inherently *less useful* than worldviews, because they are *more constricting*.

Instead of admitting that our perspective is limited by our standpoint and the
senseless noise and fury of reality must be filtered through many layers of
interpretation before it becomes a worldview in the first place, and thus
holding ideas loosely, ready and willing to change them if they stop being
useful to us or if the change would make them more useful, and really listening
to other people with different perspectives and experiences than you in order
to find useful things to expand your worldview with, ideologies lock you down
into one place. No matter how unpleasant or self-destructive or useless its
ideas, no matter how limiting, you can't leave an ideology. It blinds you to
any path of escape or expansion. Moreover, instead of being able to recognize
that people may think differently about things than you because they have
different needs and ways of thinking, as well as a different landscape of
experiences to navigate, and thus being willing to allow people to live and let
live, ideologies tell you that there cannot possibly be a reason why someone
thinks differently than you --- it isn't that they could be right or wrong,
it's that they *must* be wrong.

For an ideology, there is only one way to do something correctly, and
everything else is heresy.

Even if that way is actually impractical.

## Monomania and programming ideologies

### Object-oriented programming: the old ideology

Whew, sorry about that! That got into some rather
[philosophical](https://en.wikipedia.org/wiki/Perspectivism) waters for a bit
there. I hope you're okay, friend! Now let's get down to the good bits.

Let's start by referring back to one of the classic, canonical programming blog
posts, Steve Yegge's ["Execution in the Kingdom of
Nouns."](https://steve-yegge.blogspot.com/2006/03/execution-in-kingdom-of-nouns.html).
If you haven't read it, I highly recommend you go read it now, it's an
excellent (and funny!) just-so story about why the sort of conceptual monomania
object-oriented and pure functional programming engage in is bad, and how it
leads to bad outcomes. Go read it, you won't regret it. I'll wait.

Back? Okay. To review, here's what seems like the clearest statement of Yegge's
point from the article:

> Object Oriented Programming puts the Nouns first and foremost. Why would you
> go to such lengths to put one part of speech on a pedestal? Why should one
> kind of concept take precedence over another? It's not as if OOP has suddenly
> made verbs less important in the way we actually think. It's a strangely
> skewed perspective. As my friend Jacob Gabrielson once put it, advocating
> Object-Oriented Programming is like advocating Pants-Oriented Clothing.

For Steve, the problem with object-oriented programming wasn't so much the
*object* part of "object-oriented" as it was the *oriented* part. There wasn't
anything wrong with having little collections of related data and behavior that
encapsulated some mutable state in your code, in moderation, within reason ---
the problem was trying to force *everything*
into that model, by any means necessary, even if it massively bloated the
resulting code with abstractions and confused the issue, making the underlying
problem that was being solved and the underlying actions that were being
executed less clear.

There are other criticisms to be made of OO, plenty of them, but I think Steve
Yegge's original point is probably the most fundamental, foundational one
anyone's ever made about object-oriented programming. All the rest criticize
the outcomes, the consequences, but Yegge questions the premise:

Why make all our programming just about *one* kind of thing, *one* way of
thinking about things?

Doesn't that seem weird to you?

Yes *computation* can be modeled with neat, uniform, totalizing mathematical
systems like Turing machines, state machines, and lambda calculus, but that
isn't the question at hand --- the question is can *your problem* or *your
algorithm* be *neatly* expressed in terms of the concepts you've chosen?
Because those two things aren't the same. Just because it is *possible* to
express any computation with a certain tiny set of concepts doesn't mean that
actually leads to the program being readable and flexible, or otherwise we'd
all be programming in Unlambda or Brainfuck. So we add "extra" "impure"
features to our languages that represent more concepts than strictly necessary,
more operations than just the totalizing conceptual model, to make things
readable by giving us other tools for abstraction that can fit better to
different problems. So we don't have to sit around typing:

```unlambda
`.d`.c`.d`.c`.d`.c`.d``e
`````````````.H.e.l.l.o.,. .W.o.r.l.dii```````````````iid.l.r.o.W. .,.o.l.l.e.H.`````````````
e``d.`c.`d.`c.`d.`c.`d.`
```

or

```brainfuck
++++++++[>++++++++<-]>++++++++.>++++++++[>++++++++++++<-]>+++++.+++++++..+++.>++++++++[>+++++<-]>++++.------------.<<<<+++++++++++++++.>>.+++.------.--------.>>+.
```

or even

```assembly
    org  0x100

    mov  dx, msg
    mov  ah, 9
    int  0x21

    mov  ah, 0x4c
    int  0x21

    msg  db 'Hello, World!', 0x0d, 0x0a, '$'
```

So yes, you can use almost any programming paradigm to express almost any problem,
because languages are Turing-complete, but that doesn't make the process and
product of that attempt necessarily clear or good anymore than saying you can
write any possible program in Unlambda or Brainfuck means anything. Different
concepts, algorithms, and problems have different shapes, different joins, and
so benefit from being modelled in different ways. Yes you can even beat your
own intuition into shape so that everything looks like objects or recursion to
you, but why would you attempt that, when even that is just artificially
limiting yourself, as well as deepening the problem by making *your intuition*
at odds with the problem?

When you try to force every concept you need to communicate --- remember,
programming is at least as much about communicating to other programmers,
including your future self, as it is about controlling the computer! --- and
every algorithm you want to write, and every problem you want to solve, through
one specific conceptual framework, you're going to be forced to do things in an
obtuse, overly-abstracted way, because reality doesn't like being fit to neat
theories and you'll need to compensate for that. You'll need to build the
abstraction you should have been using for the problem all along out of the
smaller set of first-order abstractions you've allowed yourself, so that you
can perform the same action as you would have before, in a reasonable manner,
but you'll be doing it at two or more layers of abstractions instead of one.
For example, the visitor pattern exists because Java only has single dispatch,
the factory method pattern exists to replace regular functions, the strategy
pattern exists to deal with a lack of first-class functions, as does the
observer pattern. That's the ultimate reason behind Greenspun's tenth rule:

> Any sufficiently complicated C or Fortran program contains an ad hoc,
> informally-specified, bug-ridden, slow implementation of half of Common Lisp.

Fundamentally, narrowing the range of your conceptual toolkit just makes life
harder for you. It's like being limited by an ideology: it only lets you see
what it wants you to see. It doesn't let you leave it behind when it isn't
practical anymore. You're stuck. And all the while, it's telling you that
you're *better*, that you're *right* and everyone else is wrong, that this
suffering makes you more worthy, that it's natural, that it's what you deserve,
and that without it, it would be a slippery slope to anarchy. After all,
without *rules*, you could do anything! The problem with object-orientation is
that it's an ideology, instead of a possible tool in some people's tool belts.
It shuts out everything else. It makes you rigid, inflexible, unable to see
when other tools might be more useful, might more naturally map to the concepts
or problem at hand, might let you use less abstraction, might be clearer,
faster, better. It forces you to go after everything with the inheritance
hammer.

Even people who don't explicitly treat object-orientation as a dogmatic
ideology have often had it baked into their presuppositions, their basic
instincts, by their programming languages and whatever classes they took or
books they read, so they can often just automatically approach every problem
through a hierarchy of interrelated objects without thinking about it, blinded
by their own perspective to the other, more practical options available to
them, even if they don't explicitly reject them.

Now, I hasten to add that there are plenty of other problems with
object-disoriented programming; for instance, the classic problem that it's
*really, really hard* to build rigid hierarchical taxonomies of things that
allow for later extension, because inevitably you'll find that some things you
want to add exist in between the rigid boxes you've constructed, or combine
them in some way, or cut across your hierarchy at a ninety-degree angle, and in
general just fuck with your carefully constructed house of cards. It's been
a topic of discussion in philosophy for quite literally millennia and probably
longer that reality just refuses to cooperate with simple, clear, beautiful
hierarchies of Platonic Forms. Of course, there are still Platonists around,
denying all the problems, much the same way there are always OOP-proponents
around, but that doesn't make them right. For another problem, consider the
fact that, in classical object oriented programming, calling any method on any
object might well mutate *its* state secretly behind your back, without you
realizing, and might well mutate the state of any other objects that object has
a reference to as well, so that by the time the changes have stopped rippling
through the inevitably complex web of object reference dependencies in your
code, well, who knows where you'll be! I'm sure there are plenty more problems
where those two came from, too, but I'll leave that as an exercise for the
reader.

### Functional programming: the new ideology

And herein also lies the problem I have with pure functional programming: **pure
functional programming is the ideological cousin of functional programming.**
It's right there in the name: pure. If you take anything away from this
overlong blog post, I want it to be this: if you see the word "pure" used as
the ultimate be-all end all in anything but food or medicine --- run the other
way. There lies ideology.

Where functional programming merely introduces new tools to our toolkit, like:

1. functions as first-class values,
2. conceptualizing programs as data flow and transformations on that data flow,
3. iterator tools like filter, map, and foldl,
4. referential transparency,
5. the danger of mutable state and side effects and the need to limit them,
6. functional reactive programming,
7. algebraic data types and exhaustive deep pattern matching,
8. monads and error handling monads like Maybe and Either,
9. automatic currying and point-free programming,
10. faster immutable data structures ala OCaml and Clojure,
11. and more,

*pure* functional programming goes much further than that. Pure functional
programming proclaims that *all* problems must be solved with these tools and
nothing else. Mutable state and side effects must be banished from your
programs, referential transparency must exist not only at the boundaries of
some functions but around and inside every function, everything must be either
plain data or behavior and never the twain shall meet, and so on. If you ever
needed anything like side effects or mutability, the pure functional
programmers say it must be done through the application of ever more abstract
and type-theoretical architectures via monads and functors and cryptic
operators and seventy layer deep trees of function composition and closures, or
at the very least through complex webs of mutual tail call recursion.

While you might find the totalizing, uncompromising vision of pure functional
programming attractive in its conceptual beauty and uniformity, in the way it
uses the same powerful concepts everywhere, in the way it makes you feel like
a god of abstraction, *that was what proponents of object-oriented programming
found so appealing about OOP, too*. It seems like the hardcore proponents of
the new functional programming ideology have forgotten the fact that it was
this *exact* sort of attraction to totalizing, uniform conceptual beauty that
got us into the object-orientation mess in the first place.

Maybe that's because we've forgotten the original feelings we had about
OO, because the shine wore off so long ago through long industry experience
that we can't remember it anymore. Maybe because we're so desperate for
something to save us from the ever-growing complexity monster that is modern
software, and the nightmare that is legacy object-oriented code. But that *is*
how we got into this mess. Yet instead of learning that totalizing visions that
force you down a path of abstraction and shaping your code into strange and
unintuitive shapes that don't match the shapes of the algorithm you're using or
problem you're trying to solve is bad as a whole, we've only learned the most
narrow version of the lesson we should've learned: that *OOP* is bad, and we
need to move on to something else.

"Oh, dictatorships aren't bad, we just had one bad dictator.  What we really
need is a good dictator, then everything will be fine."

That isn't to say pure functional programming doesn't have good points, or else
it wouldn't be as popular as it is. Ideologies always form around a kernel of
truth, no matter how small, that initially attracts their adherents so that the
rest of the conceptual apparatus can spring closed around them like a Venus fly
trap.

Pure functional programming *is* right, your code should not be having
surprising side-effects behind the scenes that might unpredictably change how
that code behaves. So it's important to limit side effects to specific
functions and try to keep side effects only being called from some core
dispatch part of your code that then also brings in the rest of your
side-effect-less code.

It's also right that mutable state can be difficult to reason about, and not
just *global* mutable state. When anything anywhere in your program can reach
into any other part of your program and mutate almost any piece of state,
whether that mutation takes place using an object's own methods and is
"encapsulated" behind "safe barriers" or takes place directly, or whether that
state was global or "just" propagated through a decentralized network of object
references, is all of little real consequence. It's still difficult to reason
about and buggy spooky action at a distance. So it's important to explicitly
mark what things any given piece of code can mutate, and therefore treat
immutability as the default.

Pure functional programming is also right about error handling using the Maybe
and Either monads being superior to exceptions or return-value errors,
especially when paired with a strong set of standard library functions for
dealing with monads.

What I don't buy from the pure functional programming side of the argument is
the idea that all mutable state is bad. It's possible to have imperative
algorithms in functions that maintain referential transparency.  Why should
those have to be marked out as unclean and only callable from the central
dispatch part of the code that's already marked out as unclean, when you can
just make sure that functions can't access any data but what's passed into
them, and make side effects governed by stack-allocated structs that have
single ownership, so that in order to perform side effects, any function would
have to have the struct governing that side-effect passed in.

It's also possible to have the central data and control flow of your program be
very clean and straightforward, so that it still largely follows the data
pipeline model that functional programming introduced, but still uses mutation
along the way for performance or convenience reasons. Just ensure that data has
a single owner, and only owners of that data, or those who already have
a mutable reference, can possibly hand mutable references out to anyone else.
Then make the central control flow path of your program the owner of your data,
and have it only hand out mutable references when necessary, to functions that
represent central stages in the control flow path.

Relatedly, it's possible to enforce this, and require the programmer to
explicitly mark what things each piece of code has access to --- both to
determine its state and output, and to know what it could possibly mutate ---
without needing to make mutations or side effects this viral type thing that
straitjackets you into only programming with functions like this in certain
ways. Just, as I've reiterated over and over above, make pass-by-reference
explicit, pass-by-copy the default, and make pass-by-reference ***rigidly
immutable by default***, and mutable pass-by-reference something that must be
specified explicitly at both the call site and the definition site. Yes, you
might say, you could just manually label everything as a mutable reference and
collapse back into Java, but you could also do that in Haskell by just putting
everything in an `ST` and `IO` monad and calling it a day. The real enforcement
mechanism is the fact that that would be a code smell to your co-workers in
both cases. Additionally, if you enforce rules like Rust's "one mutable
reference XOR many immutable ones" model, this would quickly become basically
impossible to actually do!

Likewise, one of the claimed benefits of pure functional programming is the
prevention of data races at compile time, since data can't be mutated. But
again, we can do this just as well, and far more flexibly, with borrow checking
rules, in that borrow checking rules will force you to *either* go the pure
functional immutable data route when sharing data between threads, *or* use
synchronization primitives, whereas pure functional programming just relegates
you to the former, or to turning off the checks that force that and wantonly
sharing memory without any check to see if you've synchronized it or not. So
again, the pure functional programming ideology is not the only way to account
for its criticisms.

In short, it's possible to take the critiques that pure functional programming
has of imperative programming and apply them to imperative programming using
very simple modifications to already existing imperative programming concepts
like pointers/references, single ownership, move semantics, and RAII. (This is
in fact precisely what Rust does!)

Why is this strategy better than just going with pure functional programming
though? Well, first of all, it's a lot more flexible and gives you a lot more
tools in your toolbox to easily express algorithms and optimize performance,
thanks to being less ideological, as we've covered, while at the same time
being just as enforcible when it comes to the things that ***actually matter***
when it comes to mutable state. And don't confuse the fact that it's *less
strict about mutable state* with it being less strict about what matters when it
comes to mutable state! Strictness toward mutable state isn't an end in itself,
it's a means to achieving maintainable programs. So the two are separable.
A pure functional programmer might look at a language that's less strict about
mutable state and feel less safe, like there are fewer guarantees, but that's
the entire confusion at the heart of the pure functional programming ideology:
assuming that because mutable state is often bad, it is always bad, and
therefore must be maximally minimized.

## Some miscellaneous counter-examples

To explore some of these points, let's look at some counter examples to the
pure functional programming anti-imperative ideology.

### Factorial

Yeah I know, boring right? *Yawn*. Well, sure, but I think it illustrates
something.

Ignoring list comprehension / iterator tools for a moment, which are a *great*
and *very useful* invention of functional programming, but bear little relation
here to whether mutation is good or not, let's write two versions of this. One
in OCaml and pure, and one in Rust and impure:

```ocaml
(* OCaml *)

let factorial n =
    let rec loop acc i =
        if i <= n then loop (acc * i) (i + 1)
        else acc
    in loop 1 1
```

```rust
// Rust, OCaml's love-child with C++

fn factorial(n: isize) -> isize {
    let acc = 1;
    for i in 1..=n {
        acc *= i;
    }
    acc
}
```

Now, I don't know about you, but I don't see much of a benefit to the first
code sample over the second. The mutation in the second is small, local,
specific, and very easy to reason about, because it's limited to just one scope
and can't escape. The larger function remains referentially transparent. In
fact, although writing recursive algorithms is almost second nature to me
(having gotten my start largely with Scheme and read through all the Little
Schemer books and done the exercises), the second one is actually a little
*easier* to reason about: while the first one requires you to hold a little bit
of ongoing state in your head, essentially modeling the nested stack frames of
the recursive call, and requires an inner function to actually define your task
and then an outer function to call it, and requires remembering the order of
arguments and both a base case and a recursive case, the Rust one is obviously
bounded, requires only one piece of ongoing semantic state (the accumulator)
and the operation it's doing is front and center.

Moreover, conceptually, they are *both* mutating something. Yeah yeah, you're
technically not mutating variables in the OCaml sample, only creating new ones,
but you're using a statement (the recursive call to `loop`) that changes the
value of a name (`acc` and `i`) from what it was before in order to change the
behavior of code that remains visibly identical. There's no real, practical,
principled difference here, just the distinctions of language lawyers and
pedants. In essence, tail call recursion is just a loop with extra steps. Yet
this is the canonical way to mutate state in pure functional programming. Yes,
most pure functional programming languages and libraries provide a myriad of
library functions (like `List.foldl`) to abstract over this, but only for
a range of general use-cases. If you have a use-case for mutation that doesn't
neatly fit into the existing library functions, you either have to find a way
to jam your idea into existing list comprehension format --- leading to classic
problems like trying to turn "iterate until some condition is met" into some
kind of sensible bounded list so you can iterate over it, or trying to figure
out how to break out of a `map` or `fold` early --- and produce some
monstrosity, or you have to use tail call recursion and gain literally nothing
over using a loop except functional masturbation.

Not only that, but the Rust program is actually *much closer* conceptually to
what the computer is doing under the hood, and therefore much more easily
reasoned-about in terms of performance, whereas in the case of the OCaml one,
the compiler will see that actually doing a recursive call and nesting stack
frames would be inefficient, and do a tail call optimization to turn your tail
call recursion into a loop very similar to the Rust one, but you don't see that
or know that, and the initial input is much farther removed from what the
compiler is actually doing, making it much harder to reason about the
performance properties of things. (We'll talk about performance much, much more
later).

So we're starting to see the cracks in the pure functional programming holy
doctrine.

*But,* a reasonable reader might ask, *this is just a simple, toy case. You
can't generalize this!*

And you're right.... sort of.

Yes, there are exceptions to every rule, and that doesn't always invalidate the
rule. But it's just as important to keep the first part of that sentence in
mind as the second. Additionally, if someone is going for absolutism, all you
need is a counter-example to prove them wrong. To put this into language the
Haskellers will understand: if someone says *∀x,P(x)*, that means *!∃x,¬P(x)*
so all you need to do is show *∃x,¬P(x)*. And once the totalizing aspect of the
theory starts to crack, it starts to free your mind to consider other
possibilities.

Let's think about another example.

### Application state

Conceptually, any application is going to have some ongoing state that its
interface must mirror: what screen the user is on, what they're currently doing
on that screen, things like that. And while applying a functional programming
model to the production of the interface from the application state behind it
is very useful, allowing you to program your UI in a declarative way and
ensuring that there is a single source of truth for your application's state,
thereby eliminating whole categories of errors, the pure functional insistence
that we model that single source of truth for state as an immutable data
structure itself is going to far. Instead of sending messages back to the
thread that handles application state and it modifying its state and
propagating those changes --- perhaps through a double buffer where whenever
a new state is produced, we copy it over to the version of the application
state the interface is looking at --- to the front-end, which is conceptually
one-to-one with the problem space (having the application state change over
time in response to events from the front end, and then having the front end
always just mirror the application state), we will have to have to do one of
two things:

1. model the application thread's event loop as a tail-call recursion
   operation, or
2. figure out how to model the events coming from the front end as a lazy
   generator/stream and then how to fold-left over that stream, with the
   accumulator acting as your state object.

The first option has no benefits over a loop itself, since it will end up
having to take the form of:

```ocaml
let loop state =
    let messages = recieve_messages message_channel in
    let state = List.fold_left apply_message state messages in
    let state = update_timers state in
    let state = update_notifications state in
    let state = poll_backend state in
    send_state state_channel state;
    loop state
```

which is no easier or harder to understand, ignoring for a moment Rust's
slightly more verbose syntax and lack of automatic currying, than:

```rust
loop {
    message_channel.for_each(|m| apply_message(&mut state, m));
    update_timers(&mut state);
    update_notifications(&mut state);
    poll_backend(&mut state);
    send_state(&mut state_channel, state.clone());
}
```

In both cases, the logic is linear, proceeds naturally through the function,
and the values that a name points to change over time. There's no conceptual
difference or improvement here. Likewise, the functional reactive programming
option is in the end little different from the tail call recursive one, but
brings with it its own problems, such as: how do we represent a tick function in
terms of a stream, if we need to do regular updates? That's not to mention the
performance issues, which again, we'll get to in a bit.

The problems with mutability start not when you do an imperative algorithm
inside a referentially transparent function, or represent your central application
control flow/dispatch as proceeding through a series of mutation transformations instead
of a series of filters, but when you have a complex web of mutual mutation, and
moreover, when you don't know what can and can't be mutated and when. Mutation
itself isn't inherently a problem, it's an excess of mutable references and
mutation that does it. On a small scale, mutation is little different than no
mutation, and can often have clarity benefits.

### Quicksort

To further expand on those clarity benefits, there is a reason that some
algorithms are expressed as imperative in the computer science literature, like
dynamic programming algorithms, and others are expressed in a more functional
way, like divide-and-conquer recursive algorithms, or even ones that are
expressed using mathematics alone. Some things are easier to express in one
way, and some are easier to express in another.

For an example of an inherently imperative algorithm, let's consider quicksort.
In order to do this, though, I have to come clean. I lied to you in the
beginning. Well, that's not quite true --- I didn't lie intentionally, I only
lied because *I'd* been lied to. You see, that Haskell quicksort implementation
above? [That isn't the real quicksort algorithm at
all.](http://augustss.blogspot.com/2007/08/quicksort-in-haskell-quicksort-is.html)
It's a fake. A facsimile made to be elegant, not real. That version of the
quicksort algorithm, while correct in the abstract, constructs new lists for
everything --- as a pure functional program is wont to do --- which means that
it actually requires something like O(log_2 n) memory. Meanwhile, the real
quicksort algorithm is ***in place***. That's a key part of its elegance and
appeal, because that means it uses almost no additional memory at all. What's
more important, even Haskell's advanced compiler can't turn the qsort code from
the introduction into the in-place sort, because they're not a simple rote
transformation away from each other. They're *fundamentally different
algorithms*, and pure functional programming *can't express* the more
efficient version of the algorithm, because guess what, it's *fundamentally*
imperative, due to the nature of in-place sorting. Here's Wikipedia's
pseudocode:

```
algorithm quicksort(A, lo, hi) is
    if lo >= hi || lo < 0 then
        return
    p := partition(A, lo, hi)
    quicksort(A, lo, p - 1)
    quicksort(A, p + 1, hi)

algorithm partition(A, lo, hi) is
    pivot := A[hi]
    i := lo - 1
    for j := lo to hi - 1 do
        if A[j] <= pivot then
            i := i + 1
            swap A[i] with A[j]
    i := i + 1
    swap A[i] with A[hi]
    return i
```

Now, there actually is a Haskell implementation of in-place quicksort! How, you
ask, is this possible, if Haskell is purely functional as they claim? Well, you
see, in typical Haskeller fashion, they have type-theoretic abstractions for
this occasion. What they do is they totally 100% allow any kind of mutation,
unsafe cast, access, or whatever else you please --- they just wrap it in
a monad and call it safe, not because any invariants have been maintained, but
merely because it has been wrapped in a monad and therefore declared unsafe.
Then, to make this process of doing multiple operations that result in things
being wrapped in monads, and require values that are *also* wrapped in monads,
less painfully clunky, they wrap the entire thing first in a layer of operator
overloading, and then in a thick layer of syntactic sugar, to produce an
extremely basic imperative DSL that resembles nothing so much as ALGOL --- and
isn't capable of passing the result of one imperative expression directly to
another imperative expression, leading to much more awkward and unreadable code
more akin to high level assembly language than anything else. Remember: if you
can't express something with your language's first order abstractions, all you
need to do is pile more abstractions on top until you've produced the original
abstraction you needed out of the tools you've limited yourself to.

***And, fundamentally, the very existence of code like this is proof of my
point, that some algorithms really are best described imperatively, and there
really isn't getting around that, and if your programming paradigm or language
is too restrictive to allow you to drop down to imperative programming when it
suits you, you'll end up sitting on a pile of abstractions to get there.***

Thus, we
get (pulled from a Stack Overflow thread instead of the blog post linked above,
because this implementation is as direct a translation of the Wikipedia
pseudocode as possible, whereas the other one wasn't, and I want this to be as
fair a comparison as possible):

```haskell
import Control.Monad.ST
import Data.Array.ST
import Data.Foldable
import Control.Monad

partition arr left right = do
    pivotValue <- readArray arr right
    storeIndex <- foreachWith [left..right-1] left (\i storeIndex -> do
        val <- readArray arr i
        if val <= pivotValue
          then do
                 swap arr i storeIndex
                 return (storeIndex + 1)
          else do
                 return storeIndex )
    swap arr storeIndex right
    return storeIndex

qsort arr left right = when (right > left) $ do
    pivot <- partition arr left right
    qsort arr left (pivot - 1)
    qsort arr (pivot + 1) right

sortList xs = runST $ do
    let lastIndex = length xs - 1
    arr <- newListArray (0,lastIndex) xs :: ST s (STUArray s Int Int)
    qsort arr 0 lastIndex
    newXs <- getElems arr
    return newXs
```

Thanks to Haskell's terseness, despite the increased level of abstraction, the
meat of each implementation of the quicksort algorithm is roughly the same
length: for the imperative one, it's 16 lines, and for the Haskell one it's 17
(although I would like to point out that the conceptual complexity of what's
going on, and the amount of syntactic tokens, in the Haskell one, is much
greater).

The problem here, though, is that in the end we've produced code that is no
easier to reason about than traditional imperative code, because in the end,
[even according to the Haskell documentation
itself](https://hackage.haskell.org/package/array-0.5.6.0/docs/Data-Array-Base.html#t:STUArray),
this *is* just imperative code, with all that that entails except that you're
operating on top of a few layers of abstraction --- in this case, on top of two
higher kinded types (`MArray` and `STUArray`) and a monad (`ST`) --- and
several layers of syntactic sugar --- first the `>>=` operator and nested
lambdas and the like, and then `do` notation, `<-` notation, `return`, etc) ---
in order to get right back to where imperative languages started, which means
that you have a much higher level of cognitive overhead if you want to really
understand what's being done and why GHC is giving you the errors it is.
Because believe me, GHC will never let you forget that you're operating in
a world of higher kinded types and monads, not for a second. You can't treat
the abstractions Haskell offers you as anything but leaky, transparent ones,
not unlike C++'s own leaky abstractions over C-like semantics, but a lot more
academic and stodgy.

In my opinion, the biggest problem in software engineering --- as opposed to
computer science --- is not too much mutability, or a lack of a powerful enough
or strict enough type system, or a paucity of complex mathematical doodads, but
complexity. And while mutability is a kind of complexity, so is the sort of
cognitive overhead that Haskell requires you to take on, in translating
problems that would be more easily expressed other ways into pure functional
programming, or in figuring out how to use it like an imperative language when
you need to. In fact, I would go further and say one of the biggest problems in
computer software today isn't just complexity, but over-abstraction. That's the
fundamental problem with Electron: trying to abstract too much over the
hardware. That's also the fundamental problem with many extant codebases, mired
deep in object hierarchies and design patterns. That's also the reason why
Entity Component Systems and Data-Oriented Programming are taking over the
video game engine development industry, dethroning object-oriented programming:
they're *less abstract*, a much simpler and therefore more flexible conceptual
model, and also much closer to what the computer actually does or needs to do,
and therefore more performant and easier to understand the behavior of.

In my experience, finding an elegant, clever way to express an algorithm in
pure functional programming, or an idea in Haskell's type system, can be an
intoxicating feeling in isolation. But as much as you might feel like Reed
Richards when you're writing that code, when you come around to that code again
and just want to actually get shit done, instead of spending all your time
making clever logic puzzles with the language, things will begin to feel less
like a page from a Fantastic Four comic book and more like a *Saw* movie.

[When you're operating at such an unwieldy level of abstraction that you have
to wade two or three levels of syntactic sugar, custom operators, and
typeclasses deep for your language to look even remotely as clean as other
languages](http://augustss.blogspot.com/2007/08/programming-in-c-ummm-haskell-heres.html),
in my opinion that indicates something is deeply wrong with your language. 

While the algorithmic "meat" of the code sample above is the same length,
roughly speaking, as the example from Wikipedia, it's also very important to
point out the extra several lines of boilerplate needed to interface that
imperative code with the rest of your code, in the form of `sortList`, and also
the inevitable custom-made helper function that's so generic it feels like you
shouldn't have to write it, and yet somehow you do, that's so common in
Haskell, in this case the foreachWith function, which I had to write myself:

```haskell
foreachWith :: (Monad m) => [i] -> a -> (i -> a -> m a) -> m a
foreachWith is acc f = foreachWith' is (return $ acc) f
    where foreachWith' [] acc f = acc
          foreachWith' (i:is) acc f = foreachWith' is (acc >>= (f i)) f
```

So in all, you get code with the exact same level of mutability in practice,
but with a much higher conceptual overhead. I don't find working with monads
too burdensome myself for small projects like this --- although I'll freely admit
I never got far enough in my Haskell education to learn too much about the `ST`
monad and mutable arrays in Haskell, so what is going on at a type level with
the code in that sample is a bit beyond me at the moment --- but the higher the
number of concepts, and level of abstraction, that you have to keep in your
head to understand the code you're looking at, the more overhead there is to
understanding the code, and in a large, complex codebase, with its own
domain-specific abstractions going on, I wouldn't be surprised if the extra
mental load of dealing with pure functional programming nonsense is
a significant hindrance, just as the equivalent deep hierarchies of classes
and objects would be in object-oriented code.

In the end, a good program in Rust or Scala or Swift or any of the new
languages that take immutability-by-default seriously will contain a mix of
imperative code, clearly marked out and within reasonable boundaries, with its
relationships to the outside world made clear, and so will any Haskell program,
except Haskell, by virtue of its ideology, will make that imperative
programming much harder, and make everyone work at a higher level (often much
higher, we've barely even touched on the real complexity of Haskell programs,
just used a very mild example) of abstraction to get to the same place, which
brings with it plenty of cognitive overhead.

### Systems programming

I keep hammering on about how ideology limits you, preventing you from having
the tools you need to accurately and productively address anything outside of
the narrow confines within which it was invented. Let's do a really big example
of that now, to really drive the point home: an entire area of programming that
pure functional programming isn't very suited for, despite the claims of some
of its proponents.

Systems programming has some important requirements, but one of the most
important ones is predictable, deterministic memory and processor usage, and
general efficiency in the use of memory and the processor. Pure functional
programming fails at both of these.

The fundamental problem is that while pure functional programming may map
closer to (some categories of) the abstract algorithms you want to use, it
doesn't map well at all onto the actual architecture of computers. Computers
have CPU cores, which linearly process *instructions* --- that is, imperative
commands to do one sort of thing or another --- one after another that change
the state of that computer's registers and memory over time, and changing
states of memory can in turn produce side-effects, such as causing something to
be written to disk, or to display on the screen, or be sent to the GPU. There
are multiple cores running at the same time, all sequentially executing
imperative instructions, and all accessing the same pool of memory, which can
effect the state of the calculations of the others.

Now, there are exceptions to this. Sometimes, processors can execute one or two
instructions in parallel, or speculatively execute an instruction and then roll
it back when that turns out not to be true. Additionally, each logical core has
its own cache, separate from main memory, and then each physical core has
another layer of cache, and then the CPU die as a whole has another layer of
cache, before main memory is actually reached. But these, too, are
fundamentally mutable, imperative, stateful ideas, and imperative language
constructs therefore tend to map much more clearly to them as well, meaning
a clearer map between the behavior of these things and the way your code looks.
For instance, imperative instructions map more clearly to the instructions the
CPU is doing, and therefore will give you a better picture of whether those
instructions can be parallelized by the CPU or not, because it'll correspond to
which instructions can be conceptually parallelized when you look at the code.
That direct correspondence in processing models just keeps paying dividends,
even in the gotcha cases pure functional programmers like to bring up, because
guess what, parallelism in the CPU is also imperative. Likewise, the state of
the cache is something that changes over time, as a stateful mutable
side-effect of running *any* operation, even one that doesn't seem like it
should mutate anything, like an array access. So viewing code as fundamentally
imperative is useful here.

And believe it or not, at the level of efficiency and determinism that systems
programming requires, being able to have your programs directly map to these
concepts can be extremely important. Being able to understand exactly what the
memory and processor behavior of your code is doing, so for instance you can
avoid constantly invalidating your cache line, can sometimes effect your
performance [to a massive
degree](https://igoro.com/archive/gallery-of-processor-cache-effects/).  So
even though one expression or statement in a systems language like C or C++ or
Rust might expand to a couple hundred or thousand or more CPU instructions, it
is still crucial to be able to predict what *sorts* of instructions they expand
to and roughly how many, and therefore the performance properties of what
you're doing in your language. 

This means that the way the compiler compiles your language needs to be
deterministic and relatively linear, and most operations you do in your
language needs to be a discrete operation that maps relatively cleanly to
a discrete set of operations under the hood, instead of being intertwined with
the expansion and interpreting of every other expression or statement in your
language, so changing something in one part of your program doesn't completely
remap the performance behavior of everything else.

The other approach, which pure functional programming fanatics, especially
Haskell lovers, like to suggest is just to "leave it up to the compiler." After
all, the compiler is infinitely "smarter" than you, so surely it must be able
to optimize your code better than you can, right? And sure enough, for an
endless sea of local optimizations, and an equally large number of
coarse-grained structural optimizations, this is absolutely true: compilers are
much better at producing performant assembly from C code than you or I or
basically anyone will ever be, most of the time. They can sometimes even do
some pretty clever things.

The problem with this logic is that this only works when the instructions
you're given to your compiler, once all of the abstractions are reduced out to
easily predictable and known-ahead of time lower-level language instructions
like a form of algebra (so classes convert down to structs and v-tables, and
iterators convert to loops, and so on), map cleanly to the operations you
actually want the CPU to perform in a relatively discrete and linear way. When
they are simple expansions of one or the other, largely reversible. That way,
the compiler can piggyback off the inherent (hopefully, anyway) memory and
processor efficiency of the algorithm you've given it, and focus on the tiny
nitty gritty housekeeping details of *fine tuning* that algorithm to be
performant. No compiler can fully fix a shitty, inefficient, over-abstracted
algorithm for you. That's why you can write dog-slow code even in C or C++ or
Rust.

And that's why the "leave it up to the compiler" thing doesn't work: when you
write a program in a purely functional, immutable manner, instead of an
imperative one, you can't be sure that the compiler will be able to convert
your higher level resource-hungry algorithm to a faster, better imperative one
transparently under the hood, because that's a much more complicated thing.
Sure, compilers can do that for you with chains of iterators, turning them into
just one single loop with all the classic imperative mutation in play, and
*sometimes* compilers can optimize away things like tail call recursion, but
the more abstract, advanced, complex stuff? That sort of thing isn't what
compilers are actually good at. Pure functional programmers often confuse the
compiler being better than you at granular optimizations (which is why C++
programmers don't do the `++i` vs `i++` trick anymore) or simple
pattern-matching structural optimizations (like converting iterators to loops)
with more complex ones, like perhaps turning that slow but purely functional
quicksort into the imperative one --- which is what proponents of Haskell seem
to think their compiler is capable of. But you can't really rely on compilers
to magically make things fast enough for you, because while the compiler is
much smarter than you for local and small optimizations, in terms of
structuring your algorithms and stuff like that it simply isn't, because it's
a machine doing deterministic pattern matching operations to make known
optimizations, and it can't convert problem-domain-specific algorithms into
entirely different problem-domain-specific algorithms, because that would
require it to be an Artificial General Intelligence. So the compiler can't make
an algorithm that follows a million pointers and jumps all over memory and
blows out the cash a bunch of times and has no linearity to it into a data
oriented algorithm that works really well. 

Additionally, even if you have a compiler that is an awe-inspiring, incredibly
skilled feat of both programming language theory, systems programming, and
software engineering like the OCaml compiler or the GHC, and it really *can* do
some incredibly complex and fancy optimizations, ultimately, it just isn't
enough for a systems programmer, because in the end you just have to rely on
the compiler to be clever enough to essentially take the general *gesture* at
the problem you want solved that you gave it in your high level language like
Haskell, and translate it into a *completely different algorithm* at the
machine code level that somehow is guaranteed to do the right thing in the end.
You're giving up all your control, all your ability to make algorithm or
domain-specific optimizations, and much of your ability to predict ahead of
time what the performance cost of anything might be. The fact that the OCaml
and GHC Haskell compilers are as performant as they are is a miracle, but it is
fundamentally non-deterministicly, and so is difficult to rely on in the systems
programming world. One moment your compiler might translate your high level
purely functional immutable code into a highly efficient imperative mutable
algorithm under the hood, and everything's fine, and the next you might change
something seemingly innocuous about how it works, and everything is a hundred
times slower. Meanwhile, there's really no such fear in the Rust or C or C++
worlds: something is either a slow construct, or a fast one, and will tend to
predictably be one or the other.

It isn't just me saying this, either. Here are a few complaints from a [Reddit
thread](https://www.reddit.com/r/haskell/comments/4f47ou/why_does_haskell_in_your_opinion_suck/) to illustrate the fact in the Haskell community that show that this is a well-known problem in Haskell:

> My major gripe with Haskell was that I could never tell the space/time
> complexity of the my code without serious analysis (that among other things
> involves second-guessing the compiler's ability to optimize). This makes
> writing good quality code harder than it needs to be.
> 
> --- npatil

> Haskell needs more ways to ensure performance related guarantees at
> compile-time, it's a source of bugs that Haskell is completely blind to. 
> 
> --- 0ldmanmike

> Haskell sucks because the performance of a piece of code is often both fragile
> (seemingly unimportant changes have a massive performance impact) and hard to
> predict.
> 
> --- FUZxxl

> My Haskell experience is limited to an introductory course, but thunk leaks
> made me scared to use the language further. The particular example I remember
> is that when adding up a big range of numbers, either foldl or foldr would
> cause the sum to take a linear amount of memory, because of lazy evaluation.
> I don't remember which fold it was, and of course that's part of the problem.
> 
> I'm willing to put up with lots of complexity in a language, if the language
> can catch my mistakes quickly. That was how I felt about the type system in
> Haskell -- yes, it was difficult to learn, and I made tons of mistakes, but
> I didn't have to worry about those mistakes sitting in some dark and untested
> corner of my codebase waiting to bite me. The compiler took care of all of
> that, and my programs were safer in the end. But not so with gigantic thunks.
> My code could very easily compile and pass tests without discovering a memory
> bug like that. The bug might be in code that only runs on Tuesdays. Or it might
> only happen if an attacker decides to pass an unusually large value in some
> random field. It felt like avoiding this kind of mistake in Haskell was going
> to take constant vigilance, which was the opposite of what drew me to Haskell
> in the first place.
> 
> --- oconnor663

> Haskell sucks because using straightforward, idiomatic Haskell code to read
> and parse a 50MB CSV file (called train.csv), using the most popular CSV
> library, requires over 2GB of RAM, and no-one on
> [/r/haskellquestions](https://www.reddit.com/r/haskellquestions/comments/4dzhoe/loading_a_50mb_csv_into_memory_with_cassava/)
> or
> [stackoverflow](https://stackoverflow.com/questions/36221125/loading-a-csv-in-memory-with-cassava)
> can explain why. 
> 
> --- budgefrankly

Here's [another
thread](https://www.reddit.com/r/haskell/comments/r3nf0o/how_performant_is_haskell_how_hard_do_you_have_to/):

> It's possible to write low-level code that comes out quite fast with GHC,
> but it's often un-ergonomic (by Haskell standards; still often more
> ergonomic than C).
> 
> The problem is that fast low-level code depends heavily on the specifics of
> GHC's optimizer - specifics which often change without warning and cause fast
> code to suddenly become slow. All it takes is some minor tweak to the
> strictness analyzer or inlining heuristics or base rewrite rules and suddenly
> you have an allocation in your hot loop and you lose 2 orders of magnitude of
> performance without any compile-time indication that anything even changed.
> It's quite brittle (as you can see by the GHC issue tracker being filled with
> such issues in core libraries like random and text).
> 
> For this reason, as of late I've been opting to write the low-level unboxed
> parts of my code in C. 
> 
> --- dnkndnts

> Some of those things have a tradeoff with performance. For example, in
> Haskell it's harder to control the representation of your data in the heap (a
> trade off with machine abstraction). This in turn may mean you need to jump
> through hoops to get space utilization similar to what a "naive" C++ version
> of the program would see. A classic example of this is [Char]. Super
> convenient representation for the programmer but the heap utilization is
> terrible and that's why text has gained so much popularity in recent years. 
> 
> --- dagit

> In languages like C and rust, you can figure out optimizations with
> relatively little understanding of the compiler. Not so in Haskell. 
> 
> --- maerwald

Now, the obvious objection here is that Haskell has lazy evaluation by default,
whereas other pure functional programming languages like OCaml and Idris don't,
and it isn't strictly a part of "pure functional programming" as an
ideology/practice, and while I would agree that many of Haskell's performance
predictability problems are due to lazy evaluation, I think many of these
problems stem from the deeper fact of the level of abstraction at which pure
functional programming takes place. You can gather a sense for this in [this
article about getting a Haskell implementation of a simple thing to equal, and
eventually outperform, unoptimized
C](https://two-wrongs.com/on-competing-with-c-using-haskell), which finishes
with a list of tips for writing high performance code in Haskell. Here are
a few of them:

> You want to avoid more complicated language constructs, which are harder to
> convert to machine code: type classes, higher order functions, partial
> application, nested expressions, and so on. It's better to repeat a function
> call a few times in the code than to loop through a repeated list of it.

> You want to use simple language constructs, which are easy to convert to
> machine code: if, case, explicit recursion, int, let bindings, tail calls,
> fully saturated function calls, and so on.

> You want to avoid operators and combinators when you can express the same
> thing explicitly. I.e. \x -> f x (g x) is better than ap f g.

So, it's not that pure functional languages can *never* reach systems
programming levels of performance --- it's just that you have to forgo a lot of
the reason to use a pure functional programming language in the first place if
you want to get there, and you have to do a lot of empirical testing and
optimization to reach or surpass the performance of C code because it isn't
obvious what the time or space complexity of any given implementation will be,
so you just have to guess and see what happens. And yes, empirical tests are
important in *any* systems programming endeavor, even one in C, but not to this
degree of *a priori* unpredictability. And keep in mind, lest you still feel
like the fact that this benchmark outperforms C disproves my point, this is
a cherry-picked benchmark that was *notable for its performance*. That's why
a blog post was made about it at all. Furthermore, that was compared to an
unoptimized stand-alone C program and FFI from Haskell to C, which is [very
slow].

There's also the problem of overhead. To start with, let's look at the most
famous performance optimization of pure functional programming languages,
[persistent immutable](https://en.wikipedia.org/wiki/Persistent_data_structure)
[data
structures](https://hypirion.com/musings/understanding-persistent-vector-pt-1).
When you talk to pure functional programmers about the performance of pure
functional languages, they'll wax lyrical about how "writing higher level code
lets the compiler be more intelligent about optimizations," which is dubious,
as I've outlined above, but they'll also point out stream/iterator fusion and
persistent immutable data structures and their structural sharing attributes as
major advances in programming language theory. They'll accuse you of being
stuck in the 70s, or 80s at best, if you express doubt that these are enough to
bring pure functional programming up to speed.

While stream fusion *is* really cool and immensely useful, something which both
Rust and C++ have actually adopted, allowing code doing series of piped
transformations on stream-like data structures to be translated into the
equivalent imperative code with basically zero cost, this is still just one of
the classic cases I was talking about above of compilers being really good at
simple, rote pattern matching optimizations. It doesn't demonstrate the
intelligence of compilers in the general case --- nor that we should *want* to
have to rely on advanced, difficult to understand compilers taking code written
on piles of abstractions a mile high and transforming it into something
completely different at the other end. So yes, just as compilers can transform
a tail call recursive loop into an imperative loop because even at the code
level they're functionally identical, so too can compilers transform iterators,
but that's because they're a pretty specific and extremely widely used use
case, a pre-existing tool in the toolbox that comes with most languages. But
whatever specific program you're writing to solve some specific problem,
whatever domain-specific algorithm you're writing, or whatever else, isn't
going to be a widely used pattern they can build recognition into the compiler
for. So smart compilers won't be able to save you with arbitrary code.

Likewise, persistent immutable data structures, while indubitably better than
naively copying your data for every modification, are still massively inferior
to in-place mutation and classic data structures like arrays, regular structs,
and vectors: while structural sharing cuts down a on how much memory needs to
be copied, turning it from an O(n) operation to an O(log_2 n) operation in both
time and (I think?) memory, a significant amount still needs to be copied
compared compared to an O(1) operation like updating a vector. *Every
insertion* becomes O(log_2 n) in memory and time on top of every previous
insertion, because you need to clone your way down to the place that needs to
be modified, clone *that*, then make the modification, and construct a new
structure that points to the old structure wherever there isn't new clone data.
This isn't a zero-cost abstraction at all, either, because now you need to
represent all your data structures, even ones that should be linear and have
exponentially decreasing memory allocation frequency like vectors, as binary
trees with logarithmic time and space complexity to do *anything*, including
reading.

This isn't just speculation on my part either: Microsoft recently implemented
persistent immutable data structures in .NET according to the Haskell/Clojure
model, and did some empirical tests on them. Mind you, Microsoft's runtime is
not known for being slow or unoptimized to my knowledge, and I have no reason
to assume they performed a half-assed implementation. Especially since, as
illustrated by the opening section of their documentation on immutable
collections, like most concepts from pure functional programming, immutable
persistent data structures *really, truly do have good uses!* Not to mention
that they have their own pure functional programming language, F#, and they
anyway have every possible reason to promote their library as much as possible.
In any case, [here are their
results](https://learn.microsoft.com/en-us/archive/msdn-magazine/2017/march/net-framework-immutable-collections#performance-and-memory-consumption-of-immutable-collections):

|          | Mutable   | ImmutableList | ILBuilder | ImmutableArray | IABuilder |
| Append   | 0.2       | 12            | 8         | Horrible!      | 0.2       |
| Prepend  | Horrible! | 13.3          | 7.4       | Horrible!      | Horrible! |
| 32b size | 128       | 320           | 320       | 128            | 128       |
| 64b size | 128       | 480           | 480       | 128            | 128       |

> [!NOTE]
> It's important to note here that while prepending to mutable arrays has bad
> properties, you could easily use a slightly different mutable data structure
> to fix that problem.

[Here's a nice short article that empirically tests the performance
characteristics of immutable data structures at large
scales](https://ayende.com/blog/164739/immutable-collections-performance) which
further illustrates the point that while persistent immutable data structures
are superior to naive copies, they're simply not acceptable for either very
large tasks, or extremely time-constrained small tasks where every millisecond
counts.

> I think we are going to need to do something about this, even though I love
> the idea of immutable collections, 16 – 32 times slower is just going to be
> utterly unacceptable.

Even the OCaml wiki has this to say of immutability:

> The most significant drawback is the requirement to create new arrays for
> each modification. This can lead to increased memory usage, particularly if
> the original array is large and frequently modified. Furthermore, some
> algorithms and data structures heavily rely on mutable arrays for efficiency,
> such as in-place sorting algorithms. In such cases, using immutable arrays
> may result in reduced performance compared to mutable alternatives.

Furthermore, besides their inherent problems, persistent immutable data
structures, and in fact most of the data model of pure functional programming
languages, relies heavily on boxing and allocating *absolutely everything* on
the heap. You can't share structure and content between old collections and new
collections based on modifications from the old one unless all the individual
parts of that old collection exist on the heap for the new collection to point
back to. This obviously has horrendous memory usage and cache behavior, and to
add insult to injury, while these data structures eliminate a significant
amount of the overhead inherent to copying data structures in order to modify
them, they still require a fair amount of copying to work, and the tricks they
use to avoid this --- such as representing maps and sets and vectors and etc
--- as binary trees incur computational complexity penalties, taking
operations that would be O(1) for `std::vec::Vec` in Rust or `std::vector` in
C++ to O(log_2 n) at best. In OCaml, every member of an [immutable
record](https://ocamlbook.org/records-and-references/#immutable-records-and-functional-updates_)
(the OCaml equivalent of a C/Rust/C++ struct) has to be individually boxed and
allocated on the heap so that the record can be (***relatively***) cheaply
cloned:

> This is called “functional update”. As we have seen in previous chapters,
> immutable values benefit from structural sharing. Inside the program memory,
> a record value is a collection of pointers to its fields. Since field values
> are known to be immutable, it is safe only copy the pointers rather than
> actual values. 

So instead of having things compact and in line --- which is becoming
increasingly *more and more* important as technology gets ambitious and CPUs
and GPUs get so powerful that we are far more memory and cache speed limited
than anything else --- pure functional programming languages would have us
allocating every little thing on the heap, with all the slowness *that*
entails, and then consequently chasing pointers all throughout the heap, which
has absolutely gigantic penalties because you will be invalidating your entire
cache every time you jump to get the value of a property or an element instead
of being able to take advantage of the cache because everything in memory is
right next to each other and in place, not to mention the sheer *fragmentation*
of it all! There's a reason real systems programmers that actually understand
how computer processors work tend to try to avoid keep allocations and pointer
interaction at all costs.

Again, it's worth reiterating that "pure functional" code in Haskell or OCaml
that approaches the speed of C or C++ or Rust tends to begin exponentially
looking more and more like imperative code, unless some canned built-in
optimization can be brought into play like stream fusion. I feel this largely
negates any advantage such languages have in the first place, even if there is
more "conceptual purity" to it because, say, all the side effects are hidden
behind an `IO` type name and all the state changes behind the `ST` type. In the
end, you'll find yourself working in a very restrictive, *extremely* imperative
sub-DSL of a language, on top of a pile of abstractions, when all you wanted to
do was get some work done. Because working on top of a pile of abstractions to
write a pure functional algorithm has an inherent cost in performance because
you're expressing an algorithm in mathematical terms divorced from how the
computer operates (and how must human beings think as well), while working on
top of a pile of abstractions to produce imperative code, while it can be
compiled down to imperative native code by the compiler and so lacks as much of
a runtime cost, has an important conceptual cost.

Even Jane Street themselves say, [when talking about how to get OCaml to
compete with C++ in
performance](https://signalsandthreads.com/performance-engineering-on-hard-mode/#4535),
that they're often force to write in a tiny, garbage-collectorless,
data-structureless, likely mostly imperative subset of the language:

> It’s much less pleasant to use than real OCaml. It’s difficult, and we only
> do this in the places that it matters, but you can do it. This is what I like
> to call a dialect of OCaml. We speak in sometimes and sometimes we gently say
> it’s zero alloc OCaml. And the most notable thing about it, it tries to avoid
> touching the garbage collector, but implied in that zero alloc dialect is
> also a lot of representational things. We have little weird corners of the
> language that are slightly less pleasant to use, but will give you more
> control over layout and more control over not touching the GC and using
> malloc instead. And it works pretty well. It’s harder, but you can do it...
> Yeah, we fight a fundamental disadvantage. We’re working on reducing it. I’m
> super excited about getting more control over the layout of OCaml types. This
> is like the biggest change to me that maybe will ever happen in the compiler.
> Being able to write down a representation of memory that is what I want it to
> be in a real OCaml type that is fun to play with...

He goes on to say that Jane Street tries to use smarter architectures and
algorithms to compensate for fighting at a fundamental disadvantage for using
a high-level functional-first programming language:

> ...but fundamentally we’re kind of at a disadvantage and we just have to work
> hard and we have to think more about, okay, we’re going to have a higher
> cache footprint. What does this mean about our architecture? How can we get
> cache from other places? How can we spread out the job across more steps,
> more processes, pre-process this one place. It gets back to, you don’t want
> to focus on over optimizing this one function. You want to make your overall
> architecture do the right things and just informs infrastructural changes.

But this raises the question --- can't we just do those more clever algorithms
and architectures in the inherently faster languages? And the answer is yes,
yes we can. And in the cases where concepts from pure functional programming
like persistent immutable data structures and such become useful, rather than
a hindrance, for those algorithms and architectures, even then pure functional
languages are at little advantage, because polyglot languages like Rust and C++
can *also* make use of those features handily, if not *quite* as easily.
That's the benefit of not being locked down to one single programming ideology:
being able to pull from any side when it becomes useful! Because the point here
is not to say that pure functional programming concepts are *never* useful, but
that they're just a tool in the toolkit, to be used alongside others, and
programming worldviews that eschew ideology in favor of using the "right tool
for the right job" will *always* be at an advantage here. 

### 

None of this is to say that *this alone* is what makes pure functional
programming bad. You could easily say, as many more reasonable Haskell types
do, that you should use C/C++/Rust for low level systems programming, and
Haskell anywhere you could use any other language like Python or Ruby or what
have you. This is just an illustration of how adhering over-much to one
particular programming ideology blinds you to the facts of the case outside
your narrow domain, which can lead you to saying ignorant and dismissive things
like "Haskell can be as fast as C, so we don't really need imperative
programming." This is just another straw on the camel's back:  if you really
care about performance, you shouldn't stick to a language where performance can
only be very indirectly effected with winks and nudges at the magic genie in
the hopes it will understand you. Trying to grasp some fundamental thing
through many layers of magic and abstraction is simply not a productive way to
do it.

## Conclusion
