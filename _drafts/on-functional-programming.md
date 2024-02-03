---
layout: post
title: "Confessions of a functional programming apostate"
toc: true
---

> The venerable master Qc Na was walking with his student, Anton. Hoping to
> prompt the master into a discussion, Anton said "Master, I have heard that
> objects are a very good thing --- is this true?" Qc Na looked pityingly at his
> student and replied, "Foolish pupil --- objects are merely a poor man's
> closures."
> 
> Chastised, Anton took his leave from his master and returned to his cell,
> intent on studying closures. He carefully read the entire "Lambda: The
> Ultimate..." series of papers and its cousins, and implemented a small Scheme
> interpreter with a closure-based object system. He learned much, and looked
> forward to informing his master of his progress.
> 
> On his next walk with Qc Na, Anton attempted to impress his master by saying
> "Master, I have diligently studied the matter, and now understand that objects
> are truly a poor man's closures." Qc Na responded by hitting Anton with his
> stick, saying "When will you learn? Closures are a poor man's object." At that
> moment, Anton became enlightened.
> 
> --- Anton van Straaten

I used to love pure functional programming.

In fact, I was a functional programming *evangelist*. All mutable state was
inherently incomprehensible and impossible to properly reason about, an evil
that must be banished from the code base, whereas pure functional programming
was this practice of crystalline beauty and clarity. All side effects should be
safely sequestered behind an `IO` monad. Monads and functors were the coolest
shit around, and further abstraction via powerful type systems so that our code
could be perfectly correct before it was ever run was the path to solving all
our problems. Pure functional programming was the ultimate end game, the best
possible method of designing software, the next wave, the end of history. I
loved showing people the pure beauty of Haskell:

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
informal dates, was that Haskell code! And I've talked about monads and functors
so much now she knows to roll her eyes whenever I start.

But at some point, I stopped pushing for pure functional programming as the
be-all end all of code styles, the end of software development history, the only
true way to program.

It started when I realized that as much as I enjoyed writing a clever functional
program and learning about type theory... when I sat down to *write shit*, to
*get shit done*, I never did pure functional programming. I simply couldn't
bring myself to practice what I preached, despite enjoying it in theory, because
there was a taxing mental overhead to all the code-golfing and abstraction
trickery needed to make pure functional programming work, and that was too great
a price to pay when the problem I was working on was already complex and I just
wanted to sit down and get stuff done. All this abstraction and code-golfing to
get pure functional programming with no mutable state and side effects to
work.... wasn't getting work done. It was doing the same architecture astronaut
bullshit I made fun of object-oriented programmers for. It made for less easily
grokkable code that was less closely mapped to the underlying problems at hand.
And whenever I did write pure FP and tried to go back to it later, it was
unreadable.

So I started preferring to program in a more procedural way. I started
criticizing pure functional programming, and looking on excessive use of it with
annoyance and exhaustion. **I became a functional programming apostate.**

And I stopped buying into programming ideologies.

Now of course pure functional programming cargo cultists will come after me for
this. They'll say that I just didn't understand pure functional programming,
that I just *wasn't good enough at it*, that if I truly trained with it enough,
I would get so good at it that reading even the densest thicket of recursion and
abstraction would be like second nature to me, that imperative programming
doesn't *really* map better to what the CPU does, or to most algorithms or
problem spaces, or to the natural human mental model of how the world works,
that that's just my ingrained programming instruction talking and that [if I'd
grown up being taught Haskell instead of Scheme, Lua, and
JavaScript](https://www.huffpost.com/entry/darmond-speers-dad-spoke_n_363477) it
would all melt before my eyes into easily readable code.

I'll explain why I don't buy these arguments in this essay, but first I want to
say that I'm going to completely ignore the assertion that I *just don't
understand*, because it's completely unprovable and unfalsifiable. It's a
combination of shifting the goal posts and no true Scotsman. It's also what
every language and programming ideology person says when challenged: C++
programmers insist memory safety errors are just a "skill issue," despite even
the best and most knowledgeable and experienced C++ programmers regularly
producing memory safety errors; OOP fanatics claim design patterns are good
actually and you're just not good enough at it, and so on. This is a common
tactic, and I'm not going to fall for it.

Especially since, to reiterate this point because it is absolutely worth
reiterating at the start of an extremely long article that might be perceived as
dissing functional programming, *I appreciate* the elegance and expressiveness
of Haskell and its powerful type system. I *enjoy* writing programs in [Haskell
or OCaml](https://prog21.dadgum.com/38.html). I understand monads and functors
and applicatives, and what their applications are. I even tend to model my
software architecture in terms of pure functions and immutable data flow as much
as possible when I'm thinking about them in the abstract, and only as
[data](https://yewtu.be/watch?v=rX0ItVEVjHc)-[oriented](https://yewtu.be/watch?v=yy8jQgmhbAU)
or [structured](https://yewtu.be/watch?v=SFv8Wm2HdNM)
[procedural](https://yewtu.be/watch?v=mrY6xrWp3Gs) imperative
[programming](https://yewtu.be/watch?v=Hi6ICEVVRiw), if those are even
necessary, when it comes down to implementing them, or if there's an
architectural problem that's difficult to reasonably solve with pure functional
programming. This is not a "Blub" problem. I know that Haskell's type system is
more powerful than basically any other language's, and I know what uses that can
provide. The power in it is evident in the existence of its ability to
encapsulate mutable state and side-effects in monads, despite my problems with
that solution.

My issue with pure functional programming isn't that I don't understand it, or
don't see the appeal, or whatever. It's that, in the end, like any totalizing
system, it has weak areas, blind spots. It isn't the most optimal, simple to use
tool for every conceivable job, and I don't think having to spend precious time
and brain space I could be spending producing interesting *results* instead
focused on code-golfing my **methods** into just the right shape is worth it. To
quote [James Hague](https://prog21.dadgum.com/55.html):

> My real position is this: 100% pure functional programing
> [doesn't](https://prog21.dadgum.com/54.html)
> [work](https://prog21.dadgum.com/3.html). Even 98% pure functional programming
> doesn't work. But if the slider between functional purity and 1980s
> BASIC-style imperative messiness is kicked down a few notches--say to
> 85%--then it really does work. You get all the advantages of functional
> programming, but without the extreme mental effort and unmaintainability that
> increases as you get closer and closer to perfectly pure [links inserted to
> give context to what he's saying].

I think this is true for a fundamental reason: pure functional programming
solves entirely the wrong problem, and as a result, just as much as it has
benefits over imperative programming, it also has *drawbacks* and you have to
pick where you want to be on that curve to optimize for yourself and your
project. But before you'll be able to see what I mean by "pure functional
programming solves the wrong problem," I'm going to have to give concrete
examples of the drawbacks, and before I do that, we need to talk about the
foundational, theoretical problem with pure functional programming:

It's an ideology.

## Ideologies versus worldviews

To my mind, an ideology is not just a coherent set of beliefs that guide how you
interpret the world and structure how you think about things; instead, it is a
*totalizing system*, a metanarrative that admits no exceptions to its rules, no
knowledge it doesn't have, no possible valid difference of opinion or
perspective, no contextual situations and knowledge, and dominates the person
who holds it, so that *even if it would be better for them to drop it*, they
remain beholden to it.

### Worldviews

Yes, everyone needs a framework for interpreting the world and a set of values
or rules of thumb for approaching things. Facts don't just present themselves
somehow already freighted with information about their importance, their
meaning, and their implications for the larger whole, so we have to look at our
experiences, and listen to the experiences of others, and piece together what we
think it all means. This is perfectly fine, and I'm not saying you shouldn't do
this, that you should somehow exist in a Pyrrhonic state of suspension of belief
at all times. Building your worldview, and even sticking by it, having
confidence in it, to a certain degree, is reasonable and good.

What you have to keep in mind at all times, however, is that your worldview is
inherently limited, because the only data we have access to is our own
experiences. We can't step outside of our own perspectives and get the complete
and unfiltered truth of any given matter, because we aren't ethereal spirits. We
will always have a particular history, a particular place in the world, a
particular range of knowledge, and even if we could step outside of those, we
would always have particular biases, assumptions, preferences, and personalities
to filter whatever we see. Yes, we can also consult with other people, try to
get their experiences and integrate them into our own worldview, but even that
is inherently filtered through our own perspective: you don't actually have
access to the raw experiences of others, you only have access to their *claims*
to have experiences, and seeing something with your own two eyes is different
than only being told about something. They could be lying, they could be
mistaken, you could disagree with them about what those experiences mean --- was
that DMT trip just a trip, or were they really communing with higher dimensional
beings?

And then there's the problem of actually building a worldview out of that
perspectival data: theories are always underdetermined by their data! There are
an *infinite* number of ways to interpret anything, and usually more than one
semi-reasonable way, too. Moreover, we construct worldviews to serve specific
needs for ourselves, and different people's needs can be different, so what
might be a totally sensible worldview for one person might be untenable or
useless for another. In fact, not even the definition of truth is itself
agreed-upon! There are a million different ways of understanding what truth
means, because truth is not some extant ontological entity, but an idea that we
use, like any other component of our worldviews, for its use-factor. Many people
might say truth is what "corresponds to reality," but I would disagree, since we
can never step outside our own heads and perspectives in order to carefully
compare the propositions we have in our heads to some list of propositions
inscribed in stone somewhere to see how well they match, so that definition is
completely useless and nonsensical. So, ultimately, there are no facts, only
interpretations, and there isn't even always central, authoritative rulebook for
judging those interpretations!

Crucially, this doesn't mean that all interpretations are of equal value,
though. All worldviews share some common goals, like usefully guiding action and
predicting reality, and even within someone's perspective some theories can be
worse or better at those things, or even inconsistent, nonsensical, or poorly
reasoned. Additionally, worldviews might be too narrow, unable to integrate and
account for new information, and thus useless if the person who made them steps
even a little outside their comfort zone, which limits their range of action and
understanding. They can even be harmful either to the person who holds them or
those around them.

A proper worldview is aware of these limitations, accepts them, and is built in
a manner that explicitly adapts to them, by seeking out other people's
perspectives and experiences in order to make your worldview more useful by
being more flexible, broader, and more able to be communicated and exist in
communication with others different from you. You don't have to agree with or
even take seriously or everything you hear, and you certainly don't need to
waste time listening to worldviews you've already found useless to you or bad,
but building an intersubjective understanding of the world with others is an
important endeavor.

### Ideologies

Ideologies are fundamentally opposed to this process, and are therefore
inherently *less useful* than worldviews, because they are *more constricting*.

Instead of admitting that our perspective is limited by our standpoint and the
senseless noise and fury of reality must be filtered through many layers of
interpretation before it becomes a worldview in the first place, and thus
holding ideas loosely, ready and willing to change them if they stop being
useful to us or if the change would make them more useful, and really listening
to other people with different perspectives and experiences than you in order to
find useful things to expand your worldview with, ideologies lock you down into
one place. No matter how unpleasant or self-destructive or useless its ideas, no
matter how limiting, you can't leave an ideology. It blinds you to any path of
escape or expansion. Moreover, instead of being able to recognize that people
may think differently about things than you because they have different needs
and ways of thinking, as well as a different landscape of experiences to
navigate, and thus being willing to allow people to live and let live,
ideologies tell you that there cannot possibly be a reason why someone thinks
differently than you --- it isn't that they could be right or wrong, it's that
they *must* be wrong.

For an ideology, there is only one way to do something correctly, and everything
else is heresy.

Even if that way is actually impractical.

### Totalizing programming paradigms as ideology

#### Object-oriented programming: the old ideology

Whew, sorry about that! That got into some rather
[philosophical](https://en.wikipedia.org/wiki/Perspectivism) waters for a bit
there. I hope you're okay, friend! Now let's get down to the good bits.

Let's start by referring back to one of the classic, canonical programming blog
posts, Steve Yegge's ["Execution in the Kingdom of
Nouns."](https://steve-yegge.blogspot.com/2006/03/execution-in-kingdom-of-nouns.html).
If you haven't read it, I highly recommend you go read it now, it's an excellent
(and funny!) just-so story about why the sort of conceptual monomania
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
the problem was trying to force *everything* into that model, by any means
necessary, even if it massively bloated the resulting code with abstractions and
confused the issue, making the underlying problem that was being solved and the
underlying actions that were being executed less clear.

There are other criticisms to be made of OO, plenty of them, but I think Steve
Yegge's original point is probably the most fundamental, foundational one
anyone's ever made about object-oriented programming. All the rest criticize the
outcomes, the consequences, but Yegge questions the premise:

Why make all our programming just about *one* kind of thing, *one* way of
thinking about things?

Doesn't that seem weird to you?

Yes *computation* can be modeled with neat, uniform, totalizing mathematical
systems like Turing machines, state machines, and lambda calculus, but that
isn't the question at hand --- the question is can *your problem* or *your
algorithm* be *neatly* expressed in terms of the concepts you've chosen? Because
those two things aren't the same. Just because it is *possible* to express any
computation with a certain tiny set of concepts doesn't mean that actually leads
to the program being readable and flexible, or otherwise we'd all be programming
in Unlambda or Brainfuck. So we add "extra" "impure" features to our languages
that represent more concepts than strictly necessary, more operations than just
the totalizing conceptual model, to make things readable by giving us other
tools for abstraction that can fit better to different problems. So we don't
have to sit around typing:

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

So yes, you can use almost any programming paradigm to express almost any
problem, because languages are Turing-complete, but that doesn't make the
process and product of that attempt necessarily clear or good anymore than
saying you can write any possible program in Unlambda or Brainfuck means
anything. Different concepts, algorithms, and problems have different shapes,
different joins, and so benefit from being modelled in different ways. Yes you
can even beat your own intuition into shape so that everything looks like
objects or recursion to you, but why would you attempt that, when even that is
just artificially limiting yourself, as well as deepening the problem by making
*your intuition* at odds with the problem?

When you try to force every concept you need to communicate --- remember,
programming is at least as much about communicating to other programmers,
including your future self, as it is about controlling the computer! --- and
every algorithm you want to write, and every problem you want to solve, through
one specific conceptual framework, you're going to be forced to do things in an
obtuse, overly-abstracted way, because reality doesn't like being fit to neat
theories and you'll need to compensate for that. You'll need to build the
abstraction you should have been using for the problem all along out of the
smaller set of first-order abstractions you've allowed yourself, so that you can
perform the same action as you would have before, in a reasonable manner, but
you'll be doing it at two or more layers of abstractions instead of one. For
example, the visitor pattern exists because Java only has single dispatch, the
factory method pattern exists to replace regular functions, the strategy pattern
exists to deal with a lack of first-class functions, as does the observer
pattern. That's the ultimate reason behind Greenspun's tenth rule:

> Any sufficiently complicated C or Fortran program contains an ad hoc,
> informally-specified, bug-ridden, slow implementation of half of Common Lisp.

Fundamentally, narrowing the range of your conceptual toolkit just makes life
harder for you. It's like being limited by an ideology: it only lets you see
what it wants you to see. It doesn't let you leave it behind when it isn't
practical anymore. You're stuck. And all the while, it's telling you that you're
*better*, that you're *right* and everyone else is wrong, that this suffering
makes you more worthy, that it's natural, that it's what you deserve, and that
without it, it would be a slippery slope to anarchy. After all, without *rules*,
you could do anything! The problem with object-orientation is that it's an
ideology, instead of a possible tool in some people's tool belts. It shuts out
everything else. It makes you rigid, inflexible, unable to see when other tools
might be more useful, might more naturally map to the concepts or problem at
hand, might let you use less abstraction, might be clearer, faster, better. It
forces you to go after everything with the inheritance hammer.

Even people who don't explicitly treat object-orientation as a dogmatic ideology
have often had it baked into their presuppositions, their basic instincts, by
their programming languages and whatever classes they took or books they read,
so they can often just automatically approach every problem through a hierarchy
of interrelated objects without thinking about it, blinded by their own
perspective to the other, more practical options available to them, even if they
don't explicitly reject them.

Now, I hasten to add that there are plenty of other problems with
object-disoriented programming; for instance, the classic problem that it's
*really, really hard* to build rigid hierarchical taxonomies of things that
allow for later extension, because inevitably you'll find that some things you
want to add exist in between the rigid boxes you've constructed, or combine them
in some way, or cut across your hierarchy at a ninety-degree angle, and in
general just fuck with your carefully constructed house of cards. It's been a
topic of discussion in philosophy for quite literally millennia and probably
longer that reality just refuses to cooperate with simple, clear, beautiful
hierarchies of Platonic Forms. Of course, there are still Platonists around,
denying all the problems, much the same way there are always OOP-proponents
around, but that doesn't make them right. For another problem, consider the fact
that, in classical object oriented programming, calling any method on any object
might well mutate *its* state secretly behind your back, without you realizing,
and might well mutate the state of any other objects that object has a reference
to as well, so that by the time the changes have stopped rippling through the
inevitably complex web of object reference dependencies in your code, well, who
knows where you'll be! I'm sure there are plenty more problems where those two
came from, too, but I'll leave that as an exercise for the reader.

#### Functional programming: the new ideology

And herein also lies the problem I have with pure functional programming: **pure
functional programming is the ideological cousin of functional programming.**
It's right there in the name: pure. If you take anything away from this overlong
blog post, I want it to be this: if you see the word "pure" used as the ultimate
be-all end all in anything but food or medicine --- run the other way. There
lies ideology.

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

*pure* functional programming goes much further than that.

Before we get to where it goes too far, however, let's remember that ideologies
always form around a kernel of truth, no matter how small. It is that seed that
initially attracts adherents, so that the rest of the conceptual apparatus can
spring closed around them like a Venus fly trap. But we must give the devil his
due, and show why the rest of the concepts, the ones that make up the ideology,
don't follow. So let's see what the truths of pure functional programming are
before I dive into arguing with its ideological dogmas.

1. **Surprising side effects are bad**: pure functional programming *is* right,
your code should not be having surprising side-effects behind the scenes that
might unpredictably change how that code behaves. When I heard C++ programmers
occasionally warn about how "some class constructors or destructors might write
to disk or call to the network, so you need to be careful of how you call them",
I recoiled in horror. Such things should not be! So it's important to limit side
effects to specific functions that obviously perform side-effects.

2. **Complex webs of mutable state are difficult to reason about**: It's also
right that mutable state can be difficult to reason about, and not just *global*
mutable state. When anything anywhere in your program can reach into any other
part of your program and mutate almost any piece of state, whether that mutation
takes place using an object's own methods and is "encapsulated" behind "safe
barriers" or takes place directly, or whether that state was global or "just"
propagated through a decentralized network of object references, is all of
little real consequence. It's still difficult to reason about and buggy spooky
action at a distance. So it's important to explicitly mark what things any given
piece of code can mutate, and therefore treat immutability as the default.

3. **Error handling could be better and monads are cool**: Similarly, pure
functional programming is also right that monads, as a concept that can
encompass list transformations, error handling, and anything that iteratively
builds an ordered series of operations, among many other things, are an
extremely powerful concept when used right. Being able to use the same
operations you can use on a list or iterator --- such as filter, map, reduce,
zip, fold, concatenate, and so on --- on *errors* in your code, or data
structures representing parsers, or many other things, representing them in data
types that cause no runtime disturbance at all unless you want them to, is an
incredibly powerful error handling tool. The possibility of handling errors like
you handle lists in a functional language makes the added requirement that you
*must* handle errors in some way, because they're *wrapping* any value you might
want to get to and operate on or pass elsewhere, less of a chore and more of an
opportunity.

From these premises, however, pure functional programming comes to a few
startling conclusions:

1. **Imperative code is *inherently* difficult to reason about**: this is never
   actually demonstrated, only merely stated: pure functional programmers
   typically gesture vaguely in the direction of gigantic webs of
   incomprehensible mutable references between objects, or global state, or some
   other such obviously extreme and degenerate case, and from this conclude that
   all mutation is problematic, but that's just cherry picking.
2. **Imperative code is *more difficult* to reason about than pure functional
   programming**: this is what makes them pure functional programmers, instead
   of just programming ideological nihilists who view it as a lost cause ---
   they thing they've found a problem, but they think they have a solution! The
   clean, provable, mathematical-equational reasoning of pure functional
   programming to the rescue! Of course, never mind that mathematics is often
   deeply unintuitive even to very intelligent people, and that what mathematics
   is doing and what programming is doing is practically speaking fairly
   different from what mathematics is doing --- this is why we have a separate
   field, computer science, with computational models, like Turing machines and
   lambda calculus, that aren't just classical mathematics.
3. **If imperative code is bad, and pure functional programming is better, we
   have to minimize imperative code...**: instead of viewing imperative
   programming as another tool in our toolkit that is good for some things and
   bad at others, and having a healthy understanding of its pros and cons, when
   it is harmless and when it isn't, and mixing and matching functional and
   imperative styles where needed, both to suit the problem *and our mental
   model of it*, we conclude that imperative things are, well, impure. Heresy.
4. **...And when we do use it, we should wrap it in a monad**: taken to its
   furthest extreme pure functional programming would have all languages operate
   like Haskell, where the langauge's entire execution model is built on the noble
   lie that there is actually no impurity in your code at all; you're just building
   an `IO` or `ST` data structure, returned from your main function, that describes
   the side effects and mutations (respectively) you want to perform to the
   runtime, and then the runtime just performs them for you.

While you might find the totalizing, uncompromising vision of pure functional
programming attractive in its conceptual beauty and uniformity, in the way it
uses the same powerful concepts everywhere, in the way it makes you feel like a
god of abstraction, *that was what proponents of object-oriented programming
found so appealing about OOP, too*. It seems like the hardcore proponents of the
new functional programming ideology have forgotten the fact that it was this
*exact* sort of attraction to totalizing, uniform conceptual beauty that got us
into the object-orientation mess in the first place.

Maybe that's because we've forgotten the original feelings we had about OO,
because the shine wore off so long ago through long industry experience that we
can't remember it anymore. Maybe because we're so desperate for something to
save us from the ever-growing complexity monster that is modern software, and
the nightmare that is legacy object-oriented code. But that *is* how we got into
this mess. Yet instead of learning that totalizing visions that force you down a
path of abstraction and shaping your code into strange and unintuitive shapes
that don't match the shapes of the algorithm you're using or problem you're
trying to solve is bad as a whole, we've only learned the most narrow version of
the lesson we should've learned: that *OOP* is bad, and we need to move on to
something else.

> "Oh, dictatorships aren't bad, we just had one bad dictator. What we really
> need is a good dictator, then everything will be fine."

I just don't buy pure functional programming's claims. I agree with it about the
dangers, but I don't think they are as extreme, or as binary, as it claims, and
I believe that it is possible to solidly and effectively contain those dangers
without going as far as pure functional programming would have us go.

Contrary to that, in my opinion and experience (such as it is), both pure
functional code and imperative code have a place in almost any code base,
depending on the *specific* problem at hand for the specific piece of code
you're writing.

Yes, you should carefully consider whether side effects and mutable state are
really necessary in any given block of code, and avoid them if they aren't, but
just as often, you need to consider whether spending time code-golfing something
that would be simple, direct, intuitive, and map clearly to the operations the
computer is actually doing in imperative form into a purely functional form, and
whether you *really* need that extra functor or monadic abstraction or whether
things could be achieved just as well by simpler means.

Which means, in turn, that I don't think imperative programming should be rare
enough, or shunned enough, to justify making them a second-order, annoying,
unwieldy abstraction in your language as pure functional programming would have
you do, rather then have them as first-class constructs that you can pull out of
your toolkit when necessary. The comparison I would make is with how OOP-first
languages like Java and C# often represent pure functional concepts like monads,
iterators/streams, lambdas, and so on, in terms of clunky retoolings of their
existing abstractions, and how much nicer it is to work with a functional
language that has first-class support for these things. I want to avoid pure
functional programming ideology taking over and producing a sort of reverse
situation, where doing imperative things is an annoyance and so code becomes
overly complex and abstruse and abstracted to compensate for that.

This is actually why, despite me predominantly using OCaml as my stand-in for
pure functional programming languages throughout the rest of this article --- at
least, when I write code samples myself --- I actually think OCaml has the right
idea: provide excellent, high-quality support for pure functional programming,
but also the tools to natively do imperative programming when necessary. It's
something I really appreciate about it. It's part of why I like Rust: Rust is a
systems programming language that brings much of the power of OCaml ---
algebraic data types, a powerful and strong type system and type *inference*
system capable of representing complex invariants, parametric polymorphism,
ergonomic use of iterators --- and some of the power of Haskell --- like traits,
which allow constrained parametric polymorphism in a way that OCaml struggles to
do, and which will eventually allow (some kinds of) higher-kinded types when
[GADTs are
merged](https://rust-lang.github.io/generic-associated-types-initiative/) ---
along with the speed and control over your data layout and automatic yet
deterministic and comprehensible memory management (that also works for other
resources, which isn't true of GCs!), as well as minimal overhead and basically
no runtime of C++.

To see why I disagree with the conclusions pure functional programmers have come
to, besides my inherent distrust of ideology and my logical issues with the
fallacies at play here, let's delve deeper.

## Into the thick of it

Here's what the rest of this essay will look like:

In the next section of the essay, what I'm going to do is explore the following:
first, why I think pure functional ideology is wrong about how bad imperative
programming is, second, what drawbacks I think pure functional programming has
and what its limitations and myopias are, and finally, why I think pure
functional programming is ultimately solving the wrong problem. I'll explore all
of these concepts threaded through easy to understand, small, toy examples, so
keep in mind that whenever I point out a difference, the difference would
actually be much larger at the scale of an entire codebase.

In the section after that, I'm going to explain why although I think pure
functional programming is correct about the specific problems it points out, I
also think it has failed to see through them to the deeper underlying problem,
and as a result, it's trying to solve the entirely *wrong problem*.

Then, in the penultimate section, I'll outline in brief, with a few examples, an
alternate way of programming that I think is superior to, or at least equivalent
to,pure functional programming, a sort of metaparadigm that can guide how you
use any other given paradigm.

Finally, I'll wrap this all up in a conclusion, where I'll briefly summarize all
my points and feelings on everything.

## Some miscellaneous counter-examples

Alright, now let's get down to business. In the next few sections, I'm going to
explore a heterogenous list (cover your ears, statically typed programmers!) of
the various areas that I think illustrate my issues with pure functional
programming best (or at least, best within the limit of an essay that isn't made
out of literate code!).

Take a deep breath. Ready? Let's rock.

### Is imperative programming always bad?

#### Fibonacci

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

Now, I don't know about you, but I don't see much of a benefit to the first code
sample over the second. The mutation in the second is small, local, specific,
and very easy to reason about, because it's limited to just one scope and can't
escape. The larger function remains referentially transparent. In fact, although
writing recursive algorithms is almost second nature to me (having gotten my
start largely with Scheme and read through all the Little Schemer books and done
the exercises), the second one is actually a little *easier* to reason about:
while the first one requires you to hold a little bit of ongoing state in your
head, essentially modeling the nested stack frames of the recursive call, and
requires an inner function to actually define your task and then an outer
function to call it, and requires remembering the order of arguments and both a
base case and a recursive case, the Rust one is obviously bounded, requires only
one piece of ongoing semantic state (the accumulator) and the operation it's
doing is front and center.

Moreover, conceptually, they are *both* mutating something. Yeah yeah, you're
technically not mutating variables in the OCaml sample, only creating new ones,
but you're using a statement (the recursive call to `loop`) that changes the
value of a name (`acc` and `i`) from what it was before in order to change the
behavior of code that remains visibly identical. There's no real, practical,
principled difference here, just the distinctions of language lawyers and
pedants. In essence, tail call recursion is just a loop with extra steps. Yet
this is the canonical way to mutate state in pure functional programming. Yes,
most pure functional programming languages and libraries provide a myriad of
library functions (like `List.foldl`) to abstract over this, but only for a
range of general use-cases. If you have a use-case for mutation that doesn't
neatly fit into the existing library functions, you either have to find a way to
jam your idea into existing list comprehension format --- leading to classic
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

#### The slight of Eratosthenes

Let's look at some more comparisons. What about the sieve of Eratosthenes? Let's
look at some comparative implementations. First we see a non-ideological mix of
imperative and functional code, using each when it's most efficient and sensible:

```rust
fn find_primes_less_than(n: usize) -> Vec<usize> {
    let mut numbers: Vec<_> = vec![true; n];
    for i in 2..(n as f64).sqrt() as usize {
        if numbers[i] {
            for j in (i*i..n).step_by(i) {
                numbers[j] = false;
            }
        }
    }
    numbers
        .into_iter()
        .enumerate()
        .filter_map(|(i, t)| if t { Some(i) } else { None })
        .collect()
}
```

And next we see the purely functional implementation:

```ocaml
let sieve n =
    let rec sieve' = function
        | [] -> []
        | p :: xs -> p :: sieve' (List.filter (fun x -> x mod p > 0) xs) in
    List.init (n - 2) (fun x -> x + 2) |> sieve'
```

Now, at first glance, this might seem like an obvious win for pure functional
programming over imperative programming, right? After all, the core idea of the
Sieve of Eratosthenes, that of progressively having to do less and less
calculational work by using each current iteration of the loop's work of finding
an additional prime number less than N to write off many future candidates that
we would otherwise have had to search through. That's *why* this example ---
typically written in Haskell, not OCaml, though --- is so popular, like
quicksort, for demonstrating the power of pure functional programming.

There is a tradeoff here, though. While the imperative code is somewhat less
clear about the pure beauty of the algorithm, since for instance you have to
actually go through the rigamarole of specifying that you're going to loop
through all the possible unique candidates for being divisors below n (so, from
2 to $\sqrt{n}$) that are not known to be non-prime (so, are marked `true`), the
imperative algorithm is *much* clearer about what sort of calculation is
actually being done than the pure functional programming one is.

So much so that you might not realize that the pure functional programming one
***isn't the real sieve at all.***

You heard me right. It's a fake!

To quote [*The Genuine Sieve of Eratosthenes* by Melissa E.
O'Neill](https://www.cs.tufts.edu/~nr/cs257/archive/melissa-oneill/Sieve-JFP.pdf):

> The code is short, looks elegant, and seems to make a persuasive case for the
> power of [pure] functional programming. Unfortunately, on closer inspection,
> that case begins to fall apart. For example, the above algorithm actually runs
> rather slowly, sometimes inspiring excuses as extreme as this one:
> 
> > Try primes !! 19.You should get 71. (This computation may take a few
> > seconds, and do several garbage collections, as there is a lot of recursion
> > going on.)
> 
> A skeptic might very well ask whether it is really okay for computing the first
> few thousand primes (or, in the above case, only first twenty!) to be such a
> taxing problem, and begin to wonder whether laziness or functional programming
> as a whole is a hopelessly inefficient waste of time.
> 
> The culprit, however, is neither laziness nor functional programming: It is
> the algorithm. Despite widespread assertion to the contrary, this algorithm is
> not the Sieve of Eratosthenes!

Although the author *says* this confusion is not the fault of pure functional
programming, they later say this:

> Some readers may feel that despite all of these concerns, the earlier
> algorithm is somehow “morally” the Sieve of Eratosthenes. I would argue,
> however, that they are confusing a mathematical abstraction drawn from the
> Sieve of Eratosthenes with the actual algorithm. The algorithmic details, such
> as how you remove all the multiples of 17, matter.

Which, I feel, is tantamount to it being the fault of pure functional
programming, when the entire appeal that pure functional programming ideologues
make in the first place is that you shouldn't *have* to think about the
nitty-gritty implementational algorithmic details of time and space complexity
and what the computer is actually doing under the hood, the compiler should just
"do that for you", and all you should have to worry about is the abstract
mathematical properties of your algorithm. The entire *point* of pure functional
programming ideology, at least as I understand it, is to be able to think of
your program in a purely abstract, mathematical, equational-reasoning manner,
without reference to what's really being done (imperatively) under the hood.

In any case, whether or not this mistake really is or isn't the fault of pure
functional programming, we can certainly take a look at what the real pure
functional solution is and compare that to the simple imperative implementation.
The real purely functional sieve of Eratosthenes required an entire research
paper (the paper linked above) to figure out, as well as several iterations and
a lot of refinement and empirical performance testing, since it is *actually
impossible* to directly translate the algorithm in a pure functional manner ---
instead, a new, related, but different algorithm had to be chosen, using an
entirely different data structure, and with worse memory characteristics. Here's
the penultimate version (since the final version actually optimizes the Sieve
*past* a naive imperative implementation, although the same optimizations could
extremely easily be used by a modern imperative language):

> We will suppose a priority queue type that includes the operations
> ```haskell
> empty :: PriorityQ k v
> minKey :: PriorityQ k v −> k
> minKeyValue :: PriorityQ k v −> (k,v)
> insert :: Ord k => k −> v −> PriorityQ k v −> PriorityQ k v
> deleteMinAndInsert :: Ord k => k −> v −> PriorityQ k v −> PriorityQ k v
> ```
> We can adjust our previous sieve code to use to use the priority queue as follows:
> ```haskell
>   sieve [] = []
>   sieve (x:xs) = x : sieve’ xs (insertprime x xs PQ.empty)
>       where
>           insertprime p xs table = PQ.insert (p*p) (map (* p) xs) table
>           sieve’ [] table = []
>           sieve’ (x:xs) table
>               | nextComposite <= x = sieve’ xs (adjust table)
>               | otherwise = x : sieve’ xs (insertprime x xs table)
>                   where
>                       nextComposite = PQ.minKey table
>                       adjust table
>                           | n <= x = adjust (PQ.deleteMinAndInsert n’ ns table)
>                           | otherwise = table
>                       where
>                           (n, n’:ns) = PQ.minKeyValue table
> ```

Does this seem as clear as the simple imperative code to you? It certainly
doesn't to me.

Now, at first I thought this pure functional version of the algorithm was just
this complicated because the paper was trying to maintain laziness as well as
functional purity, and *that* was why they had to use a custom (left as an
exercise for the reader!) data structure and do all of this complicated data
structure management, so before I'd read through the above implementation and
been contaminated by it too much, I sat down to clean-room engineer my own
*strict* pure functional version of the algorithm in OCaml. This is what I came
up with:

```ocaml
let sieve n =
    let replace l pos a = List.mapi (fun i x -> if i = pos then a else x) l in
    let rec sieve_inner j i ns = 
        match j with
        | j when j < n -> sieve_inner (j + i) i (replace ns j Option.none)
        | _ -> ns in
    let rec sieve' ds ns =
        match ds, ns with
        | [], ns -> List.filter_map (fun x -> x) ns
        | i::is, ns -> sieve' is (sieve_inner (2 * i) i ns) in
    sieve' (List.init (int_sqrt n - 2) (fun x -> x + 2))
           (List.init (n - 2) (fun x -> Option.some x))
```

Now, is this any clearer or safer than the imperative code?

No, not really. We've replaced loops with clear boundary conditions and steps
with the tail call recursive equivalent of while loops, but that's about it. If
anything, the logic and structure of what's being done is once again *less*
clear.

And the real kicker is, after *all that*... these still aren't really the Sieve
of Eratosthenes as imperative programmers know it, because they have
*completely* different space complexity! After all, we're creating and
recreating new data structures at every point. (You might point out, rightly,
that this isn't quite as bad as it seems, because Haskell and most other pure
functional programming languages have persistent data structures, but I have bad
news for you: that doesn't mean as much as you think it does. We'll discuss this
later). This means that we will actually be *forced* to program in an
imperative, mutable fashion if we want to get the wonderful in-place space and
time complexity of the real Sieve. ***Imperative programming is just sometimes
unavoidable.***

#### Generating a random array of position structures

In preparation for a benchmark later on in this article, let's look at three
programs, all doing the same thing: generating an array of 100,000,000 records
which contain three-dimensional position data that is randomly generated. Now,
this is actually a problem that lends itself very nicely to a *certain* degree
of functional programming, so the OCaml and Rust versions of this end up
basically identical semantically, and roughly as easy as each other:

```ocaml
let entities = Array.init 100000000 (fun i ->
        { x = Random.float 100.0;
          y = Random.float 100.0;
          z = Random.float 100.0 })
```

```rust
// In the code posted on my GitHub for this benchmark, I used a custom random implementation because I didn't want to have to make a new crate for such a simple benchmark if I didn't need to, but it would look basicaly the same using `rand`, which is basically part of Rust's standard library anyway.

let mut entities = (0..100_000_000).map(|_| Position { 
    x: rand_range(100.0),
    y: rand_range(100.0),
    z: rand_range(100.0),
}).collect::<Vec<_>>();
```

Now let's look at the Haskell version:

```haskell
chunks :: Int -> [a] -> [[a]]
chunks n = takeWhile (not . null) . unfoldr (Just . splitAt n)

toPC [x, y, z] = PositionComponent { x = x, y = y, z = z }

-- inside `do` block:
    gen <- getStdGen
    let rands = (randomRs (0.0, 100.0) gen)
    let ichunks = take 100000000 $ chunks 3 rands
    let entities = listArray (0,100000000 - 1) $ map toPC ichunks
```

It took me about an hour and a half to come up with this solution. Not because
this code is particularly hard to read and understand (it isn't at all), nor
because the monadic concepts involved were hard to grasp in the abstract, but
because I had to start with a simple, intuitive description of my problem ---
"generate a list of 100,000,000 position components, where each property of that
component is a random number between 0 and 100" --- and find my way to a
slightly less intuitive description of it --- "generate an infinite list of
random numbers between 0 and 100, take chunks of three from them, take
100,000,000 of them, then convert each chunk of three into a position
component." Seeing that second description, you might think to yourself "oh, but
that's easy --- that obviously does the same thing as the previous one!" And
you'd be right. The problem, though, is getting there in the first place:
knowing *that's* the right way to phrase your problem, out of the infinite
number of ways to do it, to get Haskell's type system to allow you to do what
you want. You have to spend minutes on end futzing with this or that random
number generation function --- each only separated from the others by the way
they model their referential transparency or lack thereof, from doing it the
linearly typed way to doing it with the `IO` monad --- and a bunch of other
library functions to figure out what combination of them all at once will allow
you to do what you want to do. And if you start down the wrong path, heaven help
you, so it's not like you can lock one thing in and then lock the next thing in
--- you need to just stare hard enough at the type theoretic constraints the
Haskell compiler and standard library have imposed upon you, and suddenly, all
at once, figure out the entire chain of operations, or you won't be able to get
there.

Haskell proponents will claim, as they often do, that this is merely "a
different way of thinking," but I think that's playing word games a little bit.
The first phrasing of the problem is the natural way to think about how to
achieve that goal, and that can be directly and naturally encoded in impure
functional programming --- and even fairly naturally in imperative programming!
The second one, meanwhile, has to be broken down not in a "how would I do it" or
"how would a simple abstraction of the computer do it" but in a "how would pure
category theory want me to do it" way. These are "just different ways of
thinking" [from a certain point of view](https://yewtu.be/watch?v=pSOBeD1GC_Y),
but I would certainly argue that the mathematical, computer scientist way of
thinking required by Haskell is not as intuitive to most people as other means
of expressing your programs.

Furthermore, what does all this monkeying around to make the type system happy
about how we've used a non-referentially transparent function gain us? Zilch.
And this precisely illustrates the difference between mere functional
programming and pure functional programming ideology.

#### Highly-stateful application structure

Consider a simple graphical application. You have a user interface thread, and a
separate application logic thread, to ensure that the user interface remains
responsive even when the application is churning away at something. Maybe that
application thread in turn has a thread pool for doing parallel work. The
simplest way to architect this would be to have all user interface events send a
message to the application thread with the content of that event, which is
stored in a queue or ring buffer or whatever on the application's side, and
processed whenever it has the time to do so. Then, whenever the application is
done processing an event, or maybe a batch of events, it presents the new current
application state to the user interface, which draws it. You could even have events have a
time stamp, so that the application thread won't replay actions that are so old
the user probably forgot about them, so there isn't that weird rubber-banding
effect that can happen where the application thread freezes, but the user does a
bunch of things, and a few seconds later the application recapitulates all those
actions at once.

You can get 90% of the benefits of pure functional programming in this
application by just properly using this Erlang-like thread actor model, and
ensuring that the user interface is specified declaratively, as a pure function
in terms of the application state, so that there is only a single source of
state/truth --- the application thread's state --- from which the user interface
is transparently generated, instead of needing to maintain two separate states
and ensure they remain in sync. But how do we model changing the application
thread's state? And how do we model presenting that new state to the user
interface thread for it to chew on?

The fundamental nature of the application at this point *is stateful*. You can't
get away from that one. You can't abstract it away. The only real way to deal
with it is to, in effect, make your pure functional program stateful: so you'd
model the application thread's event loop as a tail-call recursion operation
that continually calls itself with the new state, or similarly figure out how to
model the events coming from the front end as a lazy generator/stream and then
how to fold-left over that stream, with the accumulator acting as your state
object. Both of these are trival and meaningless transformations, though.
They're basically a while loop that's #NotLikeOtherGirls.

Compare these two pieces of code:

```ocaml
(* Oh how I long for a State monad! *)
let loop state =
    let messages = recieve_messages message_channel in
    let state = List.fold_left apply_message state messages in
    let state = { state with timers = update_timers time state.timers } in
    let state = { state with notifications = update_notifications notification_queue state.notifications } in
    let state = poll_backend state in
    send_state state_channel state;
    loop state
```

That is, really, neither harder nor easier to read than the equivalent
imperative program:

```rust
loop {
    message_channel.for_each(|m| apply_message(&mut state, m));
    update_timers(&time, &mut state.timers);
    update_notifications(&notification_queue, &mut state.notifications);
    poll_backend(&mut state);
    state.buffer_swap();
}
```

In both cases, the logic is linear, proceeds naturally through the function, and
the values that a name points to change over time. There's no conceptual
difference or improvement here.

This is why pure functional programmers tend to compare a pure functional
programming architecture not to a sensible structured programming imperative
implementation, but to some kind of pure-imperative nightmare rats nest with
lines of global state snaking all over the place, or some kind of equally
nightmarish web of OOP objects.

When your code is reasoned through carefully, and laid out simply, it doesn't
matter whether it's structured/procedural/imperative or purely functional. It
will be clear and easy to reason about. All I had to do here was take advantage
of the fact that immutability is the default in Rust and Rust does not have
mutable global variables, so all mutable data relationships are explicit, to
make it clear what each function does, and what it depends on. It's the
equivalent of the difference between having `goto` everywhere in your code and
using procedures, which demonstrates that the problem isn't jumping around in
your code, the problem is just not doing it in an organized, structurally clear
way: the problem isn't mutability, it's mutability not being done in a clear,
explicit way.

Now, the pure functional programmer might well argue that pure functional
programming is *all about* making your mutability, or your state flow,
structured and clear like that. My point is, you can just as easily make an
imperative procedural code base structured as you can a purely functional one.
And, likewise, you can just as easily make a pure functional codebase a
confusing mess of accidental side effects if you want to --- in fact, the
default instinct of most pure functional programmers to just pass the entire
world/application state into each function that might need to change something,
and pass a new world state out, creates just exactly this sort of opportunity.
You could just refrain from doing that, know it's a code smell and choose to
avoid it, but one can very easily say the same thing about global variables or
world-state passing in an imperative language. Plus, this sort of thing [isn't
limited to just the worst case](https://prog21.dadgum.com/36.html).

But let's keep going for a bit. How *do* we actually communicate state between
threads in a purely functional manner? To be honest, I'm not sure. Sending
things over channels is inherently a side-effectful, mutation-like operation.
Swapping out a double buffer so the user interface thread is looking at a new
state in the same place in memory / with the same name in the code is even more
problematic, from a purely functional standpoint, because you're clearly
mutating a variable. Which can become a serious problem if you want to be able
to, say, have your thread pool working on things based on the application state,
and want to be able to update what state they're looking at. How else could you
do this but a double buffer swap? You could maybe have each thread have its own
internal copy of state, instead of just referring to the double buffer, and
coordinate state changes by message between them and the application thread
($O(n)$) and between each other ($O(n^2)$), but that seems needlessly wasteful
--- not to mention the fact that if you're not sending the whole state object,
that means that there's actually multiple sources of truth again, since each
thread needs to internally update its own state to match that of the others.
Moreover, this solution, while theoretically pure, and pure enough for Erlang,
is ultimately just re-inventing (Alan Kay's original vision of) object-oriented
programming, and you'll end up with the same problems: even though the state of
each object/thread is incapsulated, and internally represented as our
handy-dandy fold-over-state solution, that doesn't actually change the fact that
that's a complex web of state that can all modify each other.

There are a lot of problems like this that you run into in certain types of
programs, where state is necessary, unavoidable, and largely pervasive.
Ultimately, it'll be forced to operate imperatively to some degree, and the
question is, how much? And how do we manage that? And as we'll see in the next
section, the monadic answer isn't much of an answer, in my opinion.

#### Site of grace (I)

Before we move on to the next section and begin exploring other topics, let's
recap. What I've been trying to illustrate here --- with toy examples, so things
don't get too out of hand, and so I don't have to do too much programming, since
I really don't like mixing programming and writing --- is that there isn't
really necessarily any *inherent* readability or reasonability advantage to
purely functional code. It all depends on what you're trying to do. If you have
some particular algorithm in mind --- whether that's an Algorithm or just
business logic or something in between --- which is clearer will greatly depend
on the algorithm itself. Some algorithms, as we've seen, tend to lend themselves
better to imperative programming, and some (not pictured because if you like
functional programming you already know them) tend to lend themselves to being
represented as streams of immutable data being transformed through a pipeline.
We've also learned that this isn't even only true when mutability is restricted
to existing only within functions, and disappearing in favor of referential
transparency at the boundaries: imperative models can also be a more natural fit
for the course-grained structure of your applications as well.

What this proves isn't necessarily that there's *no* problem with mutability,
though --- just that when localized, controlled, with the data dependencies
clearly marked out, it can be a useful tool, and that only when it grows outside
the bounds of a clear structure does it become a problem. Mutation only becomes
difficult to reason about in a situation where you don't know what data depends
on what and what can mutate what and keep that directed graph in your head.
*That's* when the problem actually begins.

At this point, at least to a certain degree, even Haskellers will be nodding
along. After all, they've got their `IO` and `ST` monads, which are designed to
facilitate exactly the kind of encapsulation and maintenence and making-explicit
of mutability data relations that I'm championing here. So let's talk about
monads.


### Quicksort & imperative programming in a functional paradigm

There is a reason that computer scientists choose to express some algorithms as
imperative, like dynamic programming algorithms, and others as more functional,
like divide-and-conquer recursive algorithms, or even sometimes using
mathematics alone. Some things are easier to express in one way, and some are
easier to express in another. It all depends on what the natural contours of the
problem or algorithm is, and how human intuition models the world.

For an example of an inherently imperative algorithm, let's consider quicksort.
In order to do this, though, I have to come clean. I lied to you in the
beginning. Well, that's not quite true --- I didn't lie intentionally, I only
lied because *I'd* been lied to. You see, the pure functional programming
mistake of *solving the wrong problem* has happened again: that Haskell
quicksort implementation above [isn't the real quicksort algorithm at
all.](http://augustss.blogspot.com/2007/08/quicksort-in-haskell-quicksort-is.html)

![If I had a nickel for every time a famous "elegant" Haskell algorithm was a
lie, I'd have two nickels. Which isn't a lot, but it's weird that it happened
twice](/dev-blog/assets/twice.jpg){:width="300"}

It's a fake. A facsimile made to be elegant, not real. That version of the
quicksort algorithm, while correct in the abstract, constructs new lists for
everything --- as a pure functional program is wont to do --- which means that
it actually requires something like $O(\log_2 n)$ memory. Meanwhile, the real
quicksort algorithm is ***in place***. That's a key part of its elegance and
appeal, because that means it uses almost no additional memory at all. What's
more important, even Haskell's advanced compiler can't turn the qsort code from
the introduction into the in-place sort, because they're not a simple rote
transformation away from each other. They're *fundamentally different
algorithms*, and pure functional programming *can't express* the more efficient
version of the algorithm, because guess what, it's *fundamentally* imperative,
due to the nature of in-place sorting. Here's Wikipedia's pseudocode:

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
unsafe cast, access, or whatever else you please --- they just wrap it in a
monad and call it safe, not because any invariants have been maintained, but
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

Thus, we get (pulled from a Stack Overflow thread instead of the blog post
linked above, because this implementation is as direct a translation of the
Wikipedia pseudocode as possible, whereas the other one wasn't, and I want this
to be as fair a comparison as possible):

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
this *is* just imperative code, with all that entails, except that you're
operating on top of a few layers of abstraction --- in this case, on top of two
higher kinded types (`MArray` and `STUArray`) and a monad (`ST`) --- and several
layers of syntactic sugar --- first the `>>=` operator and nested lambdas and
the like, and then `do` notation, `<-` notation, `return`, etc) --- in order to
get right back to where imperative languages started, which means that you have
a much higher level of
[abstraction](https://en.wikibooks.org/wiki/Haskell/do_notation)
[and](https://en.wikibooks.org/wiki/Haskell/Understanding_monads)
[complexity](https://en.wikibooks.org/wiki/Haskell/Understanding_monads/State)
if you want to really understand what's being done and why GHC is giving you the
errors it is. Because believe me, GHC will never let you forget that you're
operating in a world of higher kinded types and monads, not for a second. You
can't treat the abstractions Haskell offers you as anything but leaky,
transparent ones, not unlike C++'s own leaky abstractions over C-like semantics,
but a lot more academic and stodgy. IMHO, additionally, [when you're operating
at such an unwieldy level of abstraction that you have to wade two or three
levels of syntactic sugar, custom operators, and typeclasses deep for your
language to look even remotely as clean as other
languages](http://augustss.blogspot.com/2007/08/programming-in-c-ummm-haskell-heres.html),
in my opinion that indicates something is deeply wrong with your language. It
may be clean and theoretically beautiful, but it's frustrating to write and be
productive in. This is why no one I know of writes serious large scale projects
in pure R6RS Scheme: as conceptually
[small](https://gist.github.com/alexispurslane/c0edfa1849cdd4b53b851cc4dde6cd8a)
and [beautiful](https://www.goodreads.com/series/163245-the-little-schemer) and
[orthogonal](https://queue.acm.org/detail.cfm?id=1039523) as it is, it just
isn't usually pragmatic to program in it.

This isn't to say that these concepts are really that hard to grok --- despite the
internet memes, if you sit down and think for a bit you'll easily figure out
what a monad is, how it works, and what it's used for, and the state monad is a
very convenient abstraction over linear typing made possible by the elegence of
monads. What I'm saying is that, if the goal of pure functional programming is
to make things *easier* to read about and *more clear*... this isn't that. And
if that *isn't* the goal of pure functional programming, well. We'll talk about
that [later](#why-functional-programming-solves-the-wrong-problem).

Something I also didn't mention earlier which I should probably mention before
we move on, too, is that while the algorithmic "meat" of the code sample above
is the same length, roughly speaking, as the example from Wikipedia, there are
actually a few extra lines of boilerplate needed to interface that imperative
code with the rest of your code, in the form of `sortList`, and also the
inevitable custom-made helper function that's so generic it feels like you
shouldn't have to write it, and yet somehow you do, that's so common in Haskell,
in this case the foreachWith function, which I had to write myself:

```haskell
foreachWith :: (Monad m) => [i] -> a -> (i -> a -> m a) -> m a
foreachWith is acc f = foreachWith' is (return $ acc) f
    where foreachWith' [] acc f = acc
          foreachWith' (i:is) acc f = foreachWith' is (acc >>= (f i)) f
```

<br>

To illustrate more in depth the complexity of what you have to understand and
keep in mind while doing imperative programming in a pure functional paradigm,
let's look at the full versions of the imperative benchmarks real quick. Here's
the imperative benchmark in OCaml:

```ocaml
type position = { mutable x: float; mutable y: float; mutable z: float }

let main () =
    let entities = Array.init 100000000 (fun i ->
        { x = Random.float 100.0;
          y = Random.float 100.0;
          z = Random.float 100.0 }) in
    let starttime = Unix.gettimeofday () in
    let () = for i = 0 to Array.length entities - 1 do
        (Array.get entities i).y <- (Array.get entities i).y *. 200.0
    done in
    let endtime = Unix.gettimeofday () in
    Printf.printf "%f\n%!" (endtime -. starttime)

let _ = main ()
```

and here's the Haskell one:

```haskell
data PositionComponent s = PositionComponent { x :: STRef s Double , y :: STRef s Double , z :: STRef s Double }

chunks :: Int -> [a] -> [[a]]
chunks n = takeWhile (not . null) . unfoldr (Just . splitAt n)

updatePos :: (Num i, Ix i) => i -> STArray s i (PositionComponent s) -> ST s (STArray s i (PositionComponent s))
updatePos 0 a = return a
updatePos i a = do
    thing <- readArray a i
    modifySTRef (y thing) (*200.0)
    updatePos (i - 1) a

toPC :: [Double] -> ST s (PositionComponent s)
toPC [x, y, z] = do
    x <- newSTRef x
    y <- newSTRef y
    z <- newSTRef z
    return $ PositionComponent { x = x, y = y, z = z }

getCPUTimeDouble :: IO Double
getCPUTimeDouble = do t <- System.CPUTime.getCPUTime; return ((fromInteger t) * 1e-12)

num = 100000000

main :: IO ()
main = do
    gen <- getStdGen
    let rands = (randomRs (0.0, 100.0) gen)
    let ichunks = take num $ chunks 3 rands :: [[Double]]
    start <- getCPUTimeDouble
    let ne = runST $ do
         e0 <- sequence $ map toPC ichunks
         e1 <- newListArray (0,num) e0
         updatePos (num-1) e1
         return ()
    stop <- getCPUTimeDouble
    putStrLn $ "Execution time: " ++ show (stop - start)
```

Yeah, I'm sure some Haskell guru will come along with a neater, smaller, more
elegant imperative version, but I would like to consider myself a fairly skilled
programmer and reasonably well-versed in the ideas behind Haskell, although not
particularly practiced with it, and this is what I was able to come up with
after maybe three hours of work. That very fact is kind of the point here: that
doing Haskell *isn't* as easy as imperative programming, and I really don't
think that's a matter of just familiarity, because there's more inherent
complexity to this than imperative programming. You can teach the average person
to program some basic things (much like this example) in a week or two *tops*. I
know, I've been a (paid) programming tutor for a bit. It's possible. But
teaching monads, and explaining all the levels of abstraction and type theory
and generic programming and shit you'd need to be able to understand to write
*this*? No fucking way, unless they're already a mathematician.

So sure, it's not that hard to read, but as you might begin to guess from the
complex type signatures and conversions back and forth between different layers
and arrangements of monads, I cannot *begin* to describe to you how
excruciatingly annoying this was to *write*, especially not knowing the final
arrangement of monads I would need. It's an incredibly frustrating fact that the
excruciating exploratory process needed to write a Haskell problem is rarely
fully expressed in the aesthetics of the final program itself. That's the thing
--- once you know Haskell well enough, reading it is honestly not that hard.
Writing it, though? Well, if you need to do anything remotely stateful or
imperative, get ready for the pain to set in. Dealing with errors like this...

```haskell

test-imperative.hs:37:68: error: [GHC-83865]
    • Couldn't match type: ST s0 (PositionComponent s0)
                     with: [a2]
      Expected: [Double] -> [a2]
        Actual: [Double] -> ST s0 (PositionComponent s0)
    • In the first argument of ‘map’, namely ‘toPC’
      In the second argument of ‘($)’, namely ‘map toPC ichunks’
      In the second argument of ‘($)’, namely
        ‘sequence $ map toPC ichunks’
    • Relevant bindings include
        entities :: m (a1 i [a2]) (bound at test-imperative.hs:37:9)
   |
37 |     let entities = newListArray (0,100000000 - 1) $ sequence $ map toPC ichunks
   |                                                                    ^^^^

test-imperative.hs:39:25: error: [GHC-83865]
    • Couldn't match type ‘a0 i0’ with ‘PositionComponent’
      Expected: STArray [a] i1 (PositionComponent [a])
        Actual: STArray [a] i1 (a0 i0 [a])
    • In the first argument of ‘updatePos’, namely ‘entities’
      In the expression: updatePos entities
      In an equation for ‘nes’: nes = updatePos entities
   |
39 |     let nes = updatePos entities
   |                         ^^^^^^^^
```

...is so excruciatingly and unnecessarily painful for very little gain. What'd
happened at this point in the development process, to explain for the
uninitiated, is that I want to be able to modify each of the `x`, `y`, and `z`
properties separately without replacing the entire position component, in order
to make this imperative!Haskell benchmark as fairly competitive as I can with
the OCaml and Rust ones. This means putting
[`STRef`s](https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-STRef.html)
as each member of the `PositionComponent` record. STRefs are basically the
equivalent of boxed mutable pointers in OCaml: they allow you to mutate things
by storing a pointer to a value allocated on the heap instead of storing the
value itself, so you can use different functions to reach through the box and
modify the value in the heap without technically modifying the box. But of
course, this *does* have side effects, which means that `STRef` needs to be
contained within a monad somehow. This is where the `ST` monad and the `s` type
argument come in: basically, all constructs in Haskell that have state-related
side-effects return types, or require inputs with types, that have this `s`
parameter along with whatever other parameters that type has, and that `s`
parameter must have an `ST` applied to it, but it is *locally-quantified*
(essentially, lexically scoped, like an argument in a lambda) by the type
signatures of the `ST`-creating functions to only exist inside `ST` itself,
forcing all `ST`-based objects to exist inside the `ST` monad because otherwise
`s` would be undefined. Essentially, it's statically enforcing, at compile time,
at a type-system level, that certain types not be divorced from a certain
context. This is extremely cool! I enjoyed reading about it. It's also a
nightmare to use. Anyway, to get back on track, in order to produce a record
(structure in C parlance) with `STRef`s in it, you obviously need to wrap that
structure in an `ST s` monad. Fine enough! The problem is, of course, if you're
producing a list of them, you're going to end up with `[ST s Thing]` which is
less than helpful, since it means every single mutation you do on each `Thing`
is going to be in a separate context, and that will come with additional
burdens. What to do? Well, there's the magical `sequence` --- another in the
incredibly long and subtly varied list of standard helper functions in Haskell
which help you get around, to a certain degree, the inherent boilerplate of
using monads (and other things) in the language, something which no other
language needs or wants --- which you can find with enough spelunking around and
osmosis, which will convert a list of monad-wrapped things into a monad-wrapped
list of things! Great! Now we need to turn that list into a mutable array with
`newListArray`, for performance reasons. Great! So we just call that on the
output of `sequence`? In retrospect of *course* not, because the output of
`sequence` is wrapped in `ST s`, and `newListArray` *produces things that go
inside an ST context from things that don't*, so obviously we need to use the
`>>=` operator on the partial application of `newListArray` to its bounds but
not the list that will populate them, to pass the unwrapped list safely to the
partially applied function and then wrap it again (this is a very system level
and basic Haskell operation, so we can be sure it's "safe").

It gets better though. Here's the next error I got:

```haskell
test-imperative.hs:41:15: error: [GHC-46956]
    • Couldn't match type ‘s0’ with ‘s’
      Expected: ST s (a0 Integer (PositionComponent s0))
        Actual: ST s0 (a0 Integer (PositionComponent s0))
        because type variable ‘s’ would escape its scope
      This (rigid, skolem) type variable is bound by
        a type expected by the context:
          forall s. ST s ()
        at test-imperative.hs:(40,22)-(45,18)
    • In a stmt of a 'do' block: e <- entities
      In the second argument of ‘($)’, namely
        ‘do e <- entities
            bnd <- getBounds e
            traceShow (snd $ bnd) (pure ())
            ne <- updatePos (snd $ bnd) e
            ....’
      In the expression:
        runST
          $ do e <- entities
               bnd <- getBounds e
               traceShow (snd $ bnd) (pure ())
               ne <- updatePos (snd $ bnd) e
               ....
    • Relevant bindings include
        entities :: ST s0 (a0 Integer (PositionComponent s0))
          (bound at test-imperative.hs:38:9)
   |
41 |          e <- entities --- have to do this to unwrap
   |               ^^^^^^^^

```

You see, in attempting to actually *run* the operation I'd constructed inside
the `ST` monad (the audacity!), because they don't run by default, because
they're supposed to be "just data structures." I ran into a problem: `ST`
differentiates between different mutability contexts (as an attempt to be thread
safe for parallel programming) which means that you can't access or modify
anything from another `ST` context *at all, ever* by default. Thought the Rust
borrow-checker's ability to prevent data races by preventing you from having a
mutable reference to something *and* any other reference at the same time was
annoying? Imagine you couldn't have a reference to anything else in your code
base at all that wasn't from the lexical "thread" of scope you're currently in,
single threaded or not. Sounds annoying? Yup, it absolutely is. It's worth
noting here that this type error only *remotely* make sense if you understand
all sorts of things about the Haskell compiler and type system and pure
functional programming, and this is just to make a simple mutation on struct in
an array.

And finally, this error:

```haskell
test-imperative.hs:42:10: error: [GHC-25897]
    • Couldn't match type ‘e’ with ‘PositionComponent s’
      Expected: ST s (STArray s Integer e)
        Actual: ST s (STArray s Integer (PositionComponent s))
      ‘e’ is a rigid type variable bound by
        the inferred type of ne :: GHC.Arr.Array Integer e
        at test-imperative.hs:(39,9)-(42,31)
    • In a stmt of a 'do' block: updatePos 100000000 e1
      In the second argument of ‘($)’, namely
        ‘do e0 <- sequence $ map toPC ichunks
            e1 <- newListArray (0, 100000000) e0
            updatePos 100000000 e1’
      In the expression:
        runSTArray
          $ do e0 <- sequence $ map toPC ichunks
               e1 <- newListArray (0, 100000000) e0
               updatePos 100000000 e1
    • Relevant bindings include
        e1 :: STArray s Integer (PositionComponent s)
          (bound at test-imperative.hs:41:10)
        e0 :: [PositionComponent s] (bound at test-imperative.hs:40:10)
        ne :: GHC.Arr.Array Integer e (bound at test-imperative.hs:39:9)
   |
42 |          updatePos 100000000 e1
```

How did it come to the conclusion that what needed to be returned from the
`runST` (at that time a `runSTArray`) was a *nested* array of arrays? Well, you
see, I piped one monad-wrapped array into a function *in the wrong place* (just
two lines above where I should've), and that again created separate contexts,
which the type inferencer took to mean the contexts must be nested. But of
course, you could only figure this out if you've been steeped in monads for
awhile.

And again, *this is all the stuff pure functional programming would make you
think about and have to understand to just do something basic like modifying
structs stored in an array.* This is what it's like working in the world pure
functional programmers want to usher in, where everything is overly rigid and
restrictive out of the paranoid fear of every and any possible thing they might
not have predicted at compile time.

#### Site of grace (II)

This section was a bit less of a mouthful than the last, but I still think it
would be helpful to take a moment to catch our breaths and retrench. In the end,
by trying to purify your codebase by forcing all imperative code to go through a
layer of type-theoretic abstractions in an attempt to "safeguard" it, you have
created a very strange situation. While your code might *look* like regular
imperative code, and will *behave* like it in the end when executed by the
runtime, the appearance of the code is just a thin layer over the much less
intuitive monadic machinery going on underneath, which in turn is only
indirectly and vaguely mapped to the action operations the machine performs:
monadic operations aren't actually pure, and they aren't actually executed the
way Haskell wants you to think of them as being executed --- it's all just a
polite fiction, which makes the task of understanding how, exactly, monads
actually map to what you want to do that much harder. `IO` just represents the
abstract concept of "some side-effecting work was done to produce this type",
and that's all.

So while I don't always find *working* with monads too burdensome myself for
small projects like this, the higher the number of concepts and level of
abstraction that you have to keep in your head to understand the code you're
looking at, the more cognitive load you're there is to understanding the code,
and in a large, complex codebase with its own domain-specific abstractions going
on, I wouldn't be surprised if the extra mental load of pure FP is a significant
hindrance, just as the equivalent deep hierarchies of classes and objects would
be in object-oriented code. This isn't a healthy amount of
compiler-helps-you-out, this is some serious kind of programming OCD that will
go to any lengths to perfect programming, even when it actually makes your code
more confusing, harder to write, and harder to reason about (diminishing returns
adhere in basically all disciplines, and computer science/programming is no
exception).

None of this is to say that Haskell is "impossible to write," or that you can't
get *really good and fast at writing Haskell*. People can get accustomed to, and
extremely good and quick at, even the most hair-raising levels of skill at
things, given enough time, practice, and natural inclination. No, the point I'm
making is that, while this may be learnable, it certainly isn't equivalently
simple to imperative programming. Not at all. This isn't "just a different way
of thinking" that's only harder to understand because I'm used to imperative
programming (a useful accusation to make, since it is never falsifiable,
since the only people discussing these things are going to be people who've
learned to program, and basically all of those will have done so with a
relatively imperative language). We are objectively working at a higher level of
abstraction to get to the exact same operations as an imperative programmer.
Maybe you can get used to it, sure, there are a fair number of practical Haskell
projects around that show that, but that isn't my point. C++ programmers can get
used to all the pitfalls and higher level of cognitive load in their language,
Java programmers can get used to the Enterprise Architecture Astronomy of the
code bases they are encouraged to create and maintain by their language, and so
on. That doesn't really prove anything. My point is that we are having to use
and understand monads and functors and their rules, and the behavior of two
specific ones, and how the syntactic sugar we're operating on top of converts to
those monad rules underneath, in order to perform operations that require no
level of abstraction at all relative to the base language for other languages.
We're having to trawl wikis for the right set of mathematical transformations
that we can compose in order to wrangle the monads into the right format,
because we can't just directly use values. We're having to select between a
bunch of basically identical functions whose only difference is that some are
linearly typed and some are monadic, on the basis of what's easier to wrangle
into the shape the type system will accept. That inherently comes with some
cognitive load and some complexity tax, especially when those abstractions are
so leaky, for instance, that you need two ways of defining a variable (`a <-
val` and `let a = val`) depending on whether `val` was created in a way that
returns a monad or not. And I mean, is this

```haskell
runST (writeSTRef _|_ v >>= f) = _|_
```

really just as easy or hard to reason about as the abstractions involved in the
average procedural program?

So what have we established? First, that sometimes doing things in an imperative
way is easier or just as easy to understand, and sometimes it is necessary, and
that, contrary to dogma, pure functional programming doesn't "just" offer a way
to safely wrap your imperative code and isolate it from its surroundings,
because the means of that wrapping/isolation is very complex and difficult to
use.

<span id="pin"></span>

Now, if you've hung around pure functional circles for long enough, you might be
thinking something like this at this point: *Sure, maybe, but isn't being able
to tag possibly-imperative code in a way that's enforced by the type system a
huge win for the correctness of your program?*

My response to this is that we can mark out and manage the complexity-inducing
kinds of mutable state without having to go all the way to monads, or even to
functional programming *at all*, and even though monads might be more complete,
and more pure, than the alternative approaches I'm going to suggest, the
improvements you'll get to that don't outweight the increase in complexity and
abstraction of using monads in comparison to just being able to do imperative
programming directly. I also think that trying to completely incapsulate mutable
state like this at the cost of more abstraction and complexity is actually
solving entirely the wrong problem. You might be able to see where I'm going
with this, but let's put a [pin](#back) in this discussion and pick that up in a
bit. Before we get to that, I have one last thing to cover, and it's a doozy.

### Performance/systems programming: a blind spot

I keep hammering on about how ideology limits you, preventing you from having
the tools you need to accurately and productively address anything outside of
the narrow confines within which it was invented. Let's do a really big example
of that now, to really drive the point home: an entire area of programming that
pure functional programming isn't very suited for, despite the claims of some of
its proponents.

Systems programming has some important requirements, but one of the most
important ones is predictable, deterministic memory and processor usage, and
general efficiency in the use of memory and the processor. Pure functional
programming fails at both of these typically.

The fundamental problem is that while pure functional programming may map closer
to (some categories of) the abstract algorithms you want to use, it doesn't map
well at all onto the actual architecture of computers. Computers have CPU cores,
which linearly process *instructions* --- that is, imperative commands to do one
sort of thing or another --- one after another that change the state of that
computer's registers and memory over time, and changing states of memory can in
turn produce side-effects, such as causing something to be written to disk, or
to display on the screen, or be sent to the GPU. There are multiple cores
running at the same time, all sequentially executing imperative instructions,
and all accessing the same pool of memory, which can effect the state of the
calculations of the others.

Now, there are exceptions to this. Sometimes, processors can execute one or two
instructions in parallel, or speculatively execute an instruction and then roll
it back when that turns out not to be true. Additionally, each logical core has
its own cache, separate from main memory, and then each physical core has
another layer of cache, and then the CPU die as a whole has another layer of
cache, before main memory is actually reached. But these, too, are fundamentally
mutable, imperative, stateful ideas, and imperative language constructs
therefore tend to map much more clearly to them as well, meaning a clearer map
between the behavior of these things and the way your code looks. For instance,
imperative instructions map more clearly to the instructions the CPU is doing,
and therefore will give you a better picture of whether those instructions can
be parallelized by the CPU or not, because it'll correspond to which
instructions can be conceptually parallelized when you look at the code. That
direct correspondence in processing models just keeps paying dividends, even in
the gotcha cases pure functional programmers like to bring up, because guess
what, parallelism in the CPU is also imperative. Likewise, the state of the
cache is something that changes over time, as a stateful mutable side-effect of
running *any* operation, even one that doesn't seem like it should mutate
anything, like an array access. So viewing code as fundamentally imperative is
useful here.

And believe it or not, at the level of efficiency and determinism that systems
programming requires, being able to have your programs directly map to these
concepts can be extremely important. Being able to understand exactly what the
memory and processor behavior of your code is doing, so for instance you can
avoid constantly invalidating your cache line, can sometimes effect your
performance [to a massive
degree](https://igoro.com/archive/gallery-of-processor-cache-effects/). So even
though one expression or statement in a systems language like C or C++ or Rust
might expand to a couple hundred or thousand or more CPU instructions, it is
still crucial to be able to predict what *sorts* of instructions they expand to
and roughly how many, and therefore the performance properties of what you're
doing in your language.

This means that the way the compiler compiles your language needs to be
deterministic and relatively linear, and most operations you do in your language
needs to be a discrete operation that maps relatively cleanly to a discrete set
of operations under the hood, instead of being intertwined with the expansion
and interpreting of every other expression or statement in your language, so
changing something in one part of your program doesn't completely remap the
performance behavior of everything else.

The other approach, which pure functional programming fanatics, especially
Haskell lovers, like to suggest is just to "leave it up to the compiler." After
all, the compiler is [infinitely "smarter" than
you](http://wiki.c2.com/?SufficientlySmartCompiler), so surely it must be able
to optimize your code better than you can, right? And sure enough, for an
endless sea of local optimizations, and an equally large number of
coarse-grained structural optimizations, this is absolutely true: compilers are
much better at producing performant assembly from C code than you or I or
basically anyone will ever be, most of the time. They can sometimes even do some
pretty clever things.

The problem with this logic is that this only works when the instructions you're
given to your compiler, once all of the abstractions are reduced out to easily
predictable and known-ahead of time lower-level language instructions like a
form of algebra (so classes convert down to structs and v-tables, and iterators
convert to loops, and so on), map cleanly to the operations you actually want
the CPU to perform in a relatively discrete and linear way. When they are simple
expansions of one or the other, largely reversible. That way, the compiler can
piggyback off the inherent (hopefully, anyway) memory and processor efficiency
of the algorithm you've given it, and focus on the tiny nitty gritty
housekeeping details of *fine tuning* that algorithm to be performant. No
compiler can fully fix a shitty, inefficient, over-abstracted algorithm for you.
That's why you can write dog-slow code even in C or C++ or Rust.

And that's why the "leave it up to the compiler" thing doesn't work: when you
write a program in a purely functional, immutable manner, instead of an
imperative one, you can't be sure that the compiler will be able to convert your
higher level resource-hungry algorithm to a faster, better imperative one
transparently under the hood, because that's a much more complicated thing.
Sure, compilers can do that for you with chains of iterators, turning them into
just one single loop with all the classic imperative mutation in play, and
*sometimes* compilers can optimize away things like tail call recursion, but the
more abstract, advanced, complex stuff? That sort of thing isn't what compilers
are actually good at. Pure functional programmers often confuse the compiler
being better than you at granular optimizations (which is why C++ programmers
don't do the `++i` vs `i++` trick anymore) or simple pattern-matching structural
optimizations (like converting iterators to loops) with the much more complex
task of converting an abstract mathematical algorithm to a fundamentally
different, more efficient imperative one under the hood, like perhaps turning
that slow but purely functional quicksort into the imperative one --- which is
what proponents of Haskell seem to think their compiler is capable of. But you
can't really rely on compilers to magically make things fast enough for you,
because while the compiler is much smarter than you for local and small
optimizations, in terms of structuring your algorithms and stuff like that it
simply isn't, because it's a machine doing deterministic pattern matching
operations to make known optimizations, and it can't convert
problem-domain-specific algorithms into entirely different
problem-domain-specific algorithms, because that would require it to be an
Artificial General Intelligence. So the compiler can't make an algorithm that
follows a million pointers and jumps all over memory and blows out the cash a
bunch of times and has no linearity to it into a data oriented algorithm that
works really well.

Additionally, even if you have a compiler that is an awe-inspiring, incredibly
skilled feat of both programming language theory, systems programming, and
software engineering like the OCaml compiler or the GHC, and it really *can* do
some incredibly complex and fancy optimizations, ultimately, it just isn't
enough for a systems programmer, because in the end you just have to rely on the
compiler to be clever enough to essentially take the general *gesture* at the
problem you want solved that you gave it in your high level language like
Haskell, and translate it into a *completely different algorithm* at the machine
code level that somehow is guaranteed to do the right thing in the end. You're
giving up all your control, all your ability to make algorithm or
domain-specific optimizations, and much of your ability to predict ahead of time
what the performance cost of anything might be. The fact that the OCaml and GHC
Haskell compilers are as performant as they are is a miracle, but it is
fundamentally non-deterministicly, and so is difficult to rely on in the systems
programming world. One moment your compiler might translate your high level
purely functional immutable code into a highly efficient imperative mutable
algorithm under the hood, and everything's fine, and the next you might change
something seemingly innocuous about how it works, and everything is a hundred
times slower. Meanwhile, there's really no such fear in the Rust or C or C++
worlds: something is either a slow construct, or a fast one, and will tend to
predictably be one or the other.

It isn't just me saying this, either. Here are a few complaints from a [Reddit
thread](https://www.reddit.com/r/haskell/comments/4f47ou/why_does_haskell_in_your_opinion_suck/)
to illustrate the fact in the Haskell community that show that this is a
well-known problem in Haskell. A sample:

> My major gripe with Haskell was that I could never tell the space/time
> complexity of the my code without serious analysis (that among other things
> involves second-guessing the compiler's ability to optimize). This makes
> writing good quality code harder than it needs to be.
> 
> --- npatil

> Haskell sucks because the performance of a piece of code is often both fragile
> (seemingly unimportant changes have a massive performance impact) and hard to
> predict.
> 
> --- FUZxxl

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
with a list of tips for writing high performance code in Haskell. Here are a few
of them:

> - You want to avoid more complicated language constructs, which are harder to
> convert to machine code: type classes, higher order functions, partial
> application, nested expressions, and so on. It's better to repeat a function
> call a few times in the code than to loop through a repeated list of it.
> ...
> - You want to use simple language constructs, which are easy to convert to
> machine code: if, case, explicit recursion, int, let bindings, tail calls,
> fully saturated function calls, and so on.
> ...
> - You want to avoid operators and combinators when you can express the same
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
like the fact that this benchmark outperforms C disproves my point, this is a
cherry-picked benchmark that was *notable for its performance*. That's why a
blog post was made about it at all. Furthermore, that was compared to an
unoptimized stand-alone C program and FFI from Haskell to C, which is very
slow. If you doubt my assertion that this was a cherrypicked bench mark, I
encourage you to check out [this page](https://github.com/kostya/benchmarks),
which has comparisons of the *same* algorithms (thus avoiding some of the
problems of the [Language Benchmarks
Game](https://benchmarksgame-team.pages.debian.net/benchmarksgame/box-plot-summary-charts.html),
although this benchmark isn't good for Haskell either).

There's also the problem of overhead. To start with, let's look at the most
famous performance optimization of pure functional programming languages,
[persistent immutable](https://en.wikipedia.org/wiki/Persistent_data_structure)
[data
structures](https://hypirion.com/musings/understanding-persistent-vector-pt-1).
When you talk to pure functional programmers about the performance of pure
functional languages, they'll wax lyrical about how "writing higher level code
lets the compiler be more intelligent about optimizations," which is dubious, as
I've outlined above, but they'll also point out stream/iterator fusion and
persistent immutable data structures and their structural sharing attributes as
major advances in programming language theory. They'll accuse you of being stuck
in the 70s, or 80s at best, if you express doubt that these are enough to bring
pure functional programming up to speed.

While stream fusion *is* really cool and immensely useful, something which both
Rust and C++ have actually adopted, allowing code doing series of piped
transformations on stream-like data structures to be translated into the
equivalent imperative code with basically zero cost, this is still just one of
the classic cases I was talking about above of compilers being really good at
simple, rote pattern matching optimizations. It doesn't demonstrate the
intelligence of compilers in the general case --- nor that we should *want* to
have to rely on advanced, difficult to understand compilers taking code written
on piles of abstractions a mile high and transforming it into something
completely different at the other end. So yes, just as compilers can transform a
tail call recursive loop into an imperative loop because even at the code level
they're functionally identical, so too can compilers transform iterators, but
that's because they're a pretty specific and extremely widely used use case, a
pre-existing tool in the toolbox that comes with most languages. But whatever
specific program you're writing to solve some specific problem, whatever
domain-specific algorithm you're writing, or whatever else, isn't going to be a
widely used pattern they can build recognition into the compiler for. So smart
compilers won't be able to save you with arbitrary code.

Likewise, persistent immutable data structures, while indubitably better than
naively copying your data for every modification, are still massively inferior
to in-place mutation and classic data structures like arrays, regular structs,
and vectors: while structural sharing cuts down a on how much memory needs to be
copied, turning it from an $O(n)$ operation to an $O(\log_2 n)$ operation in
both time and (I think?) memory, a significant amount still needs to be copied
compared to an $O(1)$ operation like updating a vector. *Every insertion*
becomes $O(log_2 n)$ in memory and time on top of every previous insertion,
because you need to clone your way down to the place that needs to be modified,
clone *that*, then make the modification, and construct a new structure that
points to the old structure wherever there isn't new clone data. This isn't a
zero-cost abstraction at all, either, because now you need to represent all your
data structures, even ones that should be linear and have exponentially
decreasing memory allocation frequency like vectors, as binary trees with
logarithmic time and space complexity to do *anything*, including reading.

This isn't just speculation on my part either: Microsoft recently implemented
persistent immutable data structures in .NET according to the Haskell/Clojure
model, and did some empirical tests on them. Mind you, Microsoft's runtime is
not known for being slow or unoptimized to my knowledge, and I have no reason to
assume they performed a half-assed implementation. Especially since, as
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

> [!NOTE] It's important to note here that while prepending to mutable arrays
> has bad properties, you could easily use a slightly different mutable data
> structure to fix that problem.

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

> The most significant drawback is the requirement to create new arrays for each
> modification. This can lead to increased memory usage, particularly if the
> original array is large and frequently modified. Furthermore, some algorithms
> and data structures heavily rely on mutable arrays for efficiency, such as
> in-place sorting algorithms. In such cases, using immutable arrays may result
> in reduced performance compared to mutable alternatives.

I also performed a basic benchmarking test of my own using OCaml and Rust. The
basic problem was to take an array of a hundred million randomly populated
position components (structs containing just the three-dimensional position data
of a hypothetical simulation entity) and multiply just one of the properties of
that component by a certain number. I implemented this benchmark in several
different ways:

1. in OCaml in a single-threaded, pure functional manner using record
update syntax,
2. in OCaml in a pure functional manner using its work-stealing job queue
based thread pool library [Domainslib](https://github.com/kostya/benchmarks),
3. in Rust in a single-threaded manner using mutability,
4. in Rust with its own work-stealing job queue thread pool based library,
[rayon](https://github.com/rayon-rs/rayon) in a mutable manner,
5. in OCaml single-threaded using mutable references,
6. in OCaml multithreaded using mutable references,
7. in Haskell single-threaded using purely functional methods.

Note: I didn't implement a multithreaded Haskell version because Haskell
requires you to essentially write in an [separate sister
language](https://wiki.haskell.org/GHC/Data_Parallel_Haskell) to get data
parallelism, and that language is apparently incomplete and subject to
performance issues, so I didn't think it would be fair. Moreover, I just
couldn't stand to write anything more in Haskell. The first iteration of this
program, when I tried to force the list to be strictly evaluated using
[deepseq](https://hackage.haskell.org/package/deepseq) actually caused some kind
of exponential memory leak that resulted in the program using up all 11GB of my
remaining RAM ***and*** all 16GB of swap and then hanging at zero percent CPU
usage for thirty minutes, despite --- as far as I can tell! --- the semantics of
that program compared to the final version being identical. In the end, I just
went with `-XStrict`.

If you're interested in the code for these, you can find that
[here](https://github.com/alexispurslane/dev-blog/tree/main/code), although be
aware I haven't taken any effort to clean up the project directory structure so
there's intermediate compiler artefacts all over the place!

Here are the results:

|                                    | Memory | Execution time      |
| OCaml pure single-threaded         | 7.8GB  | 2.9s  (sd = 0.066)  |
| OCaml pure parallel                | 10.2GB | 5.7s  (sd = 0.22)   |
| OCaml impure single-threaded       | 3.9GB  | 0.25s (sd = 0.02)   |
| OCaml impure parallel              | 3.9GB  | 0.2s  (sd = 0.004)  |
| Rust impure single-threaded        | 2.35GB | 0.15s (sd = 0.0008) |
| Rust pure single-threaded          | 2.35GB | 0.16s (sd = 0.0008) |
| Rust impure parallel               | 2.35GB | 0.14s (sd = 0.003)  |
| Haskell pure single-threaded       | 13.9GB | 11s (sd = NA)       |
| Haskell imperative single-threaded | 26GB   | 61s (sd = NA)       |

![Column C is execution time, Column B is memory](/dev-blog/assets/barchart.png)

Orange is execution time, blue is memory.

Note: if you're wondering why the standard deviation for Haskell is nonexistent,
that's because it took something like 10 minutes for it to construct the array
it needed to run the benchmark on, and I really didn't want to have to repeat
that a bunch of times.

I think that speaks for itself. In order to get remotely reasonable memory and
processor efficiency, you have to drop out of the functional programming world
and use OCaml exactly as if it were Rust, except a bit slower (and it isn't
really surprising that at that level of implementation they're around the same
speed, considering OCaml is a very static, native compiled language and so is
Rust), more awkward to use (in imperative mode), and without any kind of
type-system level enforcement for statically avoiding data races. I was
genuinely, unironically afraid for my computer when the parallel OCaml benchmark
was running, as I watched the memory usage in the system monitor inexorably tick
up towards sixteen gigabytes like some kind of nuclear core meltdown (I usually
have about 5GB of other programs running on my computer at all times), quaking
in my boots that my computer would lock up and I'd lose fifteen minutes of
writing progress.

Also to mirror an earlier statement of mine, I'm sure some Haskell programmer
could breeze through and make my Haskell implementations infinitely faster ---
but that's kind of my point: just translating programs directly between the
languages gives you a much better understanding of the default speed of the
language, because sure you *could* optimize the Haskell implementation, but
you'd have to be deep into Haskell, with a deep and intimate knowledge of how
the GHC does all its incredibly complex optimizations, and of the RTS, and how
it actually runs your program once it's all compiled, to understand how and what
to do to properly optimize Haskell. So while you *can* get "Haskell as fast as
C", it isn't at baseline as fast as even a naive C implementation. Meanwhile, in
a compiled imperative language, you can bang together a reasonably fast
imperative implementation and have a fairly solid understanding of exactly what
the time and space complexity will be like and what the empirical performance
will be just based on the homomorphism between your language's constructs and
how CPUs work, and if you *want* to optimize it further, all you need to do is
have a good understanding of those same things.

And yes, benchmarks aren't the real world --- in an actual game you almost
certainly won't be processing a list of entities that spectacularly huge, but in
real life you'll have much stricter time constraints to compensate for the much
smaller batch sizes. I just did the large batch size for effect, to make the
difference in scales between the OCaml and Rust implementations that much more
comprehensible at human time scales. So while we're dealing with differences of
multiple seconds at large input sizes, and we'll only be dealing with
differences of milliseconds at smaller input sizes, those millisecond
differences will matter just as much, if not more, to the practicality of your
program as multiple-second differences mean in benchmarks. Moreover, keep in
mind that this isn't a constant difference, it's a multiple, and in situations
where differences in just a few milliseconds are gigantic, multiples of
milliseconds can be disastrous: going from, say, 13ms (a nice 75 fps) to (using
the performance multiple between the single-threaded Rust and OCaml programs,
x19) 247ms (an abysmal 4fps), for example, is just not acceptable.

Furthermore, besides their inherent problems, persistent immutable data
structures, and in fact most of the data model of pure functional programming
languages, relies heavily on boxing and allocating *absolutely everything* on
the heap. Hell, even if you want to program imperatively in OCaml and use
mutable variables, [those variables have to actually be allocated on the
heap](https://ocamlbook.org/records-and-references/#references), because you
can't modify anything on the stack in OCaml, only things on the heap, so you
need to allocate even a single, small variable on the heap if you want to be
able to modify it. More relevantly to the above, you can't share structure and
content between old collections and new collections based on modifications from
the old one unless all the individual parts of that old collection exist on the
heap for the new collection to point back to. This obviously has horrendous
memory usage and cache behavior, and to add insult to injury, while these data
structures eliminate a significant amount of the overhead inherent to copying
data structures in order to modify them, they still require a fair amount of
copying to work, and the tricks they use to avoid this --- such as representing
maps and sets and vectors and etc --- as binary trees incur computational
complexity penalties, taking operations that would be $O(1)$ for `std::vec::Vec`
in Rust or `std::vector` in C++ to $O(\log_2 n)$ at best. In OCaml, every member of
an [immutable
record](https://ocamlbook.org/records-and-references/#immutable-records-and-functional-updates_)
(the OCaml equivalent of a C/Rust/C++ struct) has to be individually boxed and
allocated on the heap so that the record can be (***relatively***) cheaply
cloned:

> This is called “functional update”. As we have seen in previous chapters,
> immutable values benefit from structural sharing. Inside the program memory, a
> record value is a collection of pointers to its fields. Since field values are
> known to be immutable, it is safe only copy the pointers rather than actual
> values.

So instead of having things compact and in line --- which is becoming
increasingly *more and more* important as technology gets ambitious and CPUs and
GPUs get so powerful that we are far more memory and cache speed limited than
anything else --- pure functional programming languages would have us allocating
every little thing on the heap, with all the slowness *that* entails, and then
consequently chasing pointers all throughout the heap, which has absolutely
gigantic penalties because you will be invalidating your entire cache every time
you jump to get the value of a property or an element instead of being able to
take advantage of the cache because everything in memory is right next to each
other and in place, not to mention the sheer *fragmentation* of it all! There's
a reason real systems programmers that actually understand how computer
processors work tend to try to avoid keep allocations and pointer interaction at
all costs.

Again, it's worth reiterating that "pure functional" code in Haskell or OCaml
that approaches the speed of C or C++ or Rust tends to begin looking
exponentially more and more like imperative code, unless some canned built-in
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

Even Jane Street themselves say, [when talking about how to get OCaml to compete
with C++ in
performance](https://signalsandthreads.com/performance-engineering-on-hard-mode/#4535),
that they're often force to write in a tiny, garbage-collectorless,
data-structureless, likely mostly imperative subset of the language:

> It’s much less pleasant to use than real OCaml. It’s difficult, and we only do
> this in the places that it matters, but you can do it. This is what I like to
> call a dialect of OCaml. We speak in sometimes and sometimes we gently say
> it’s zero alloc OCaml. And the most notable thing about it, it tries to avoid
> touching the garbage collector, but implied in that zero alloc dialect is also
> a lot of representational things. We have little weird corners of the language
> that are slightly less pleasant to use, but will give you more control over
> layout and more control over not touching the GC and using malloc instead. And
> it works pretty well. It’s harder, but you can do it... Yeah, we fight a
> fundamental disadvantage. We’re working on reducing it. I’m super excited
> about getting more control over the layout of OCaml types. This is like the
> biggest change to me that maybe will ever happen in the compiler. Being able
> to write down a representation of memory that is what I want it to be in a
> real OCaml type that is fun to play with...

He goes on to say that Jane Street tries to use smarter architectures and
algorithms to compensate for fighting at a fundamental disadvantage for using a
high-level functional-first programming language:

> ...but fundamentally we’re kind of at a disadvantage and we just have to work
> hard and we have to think more about, okay, we’re going to have a higher cache
> footprint. What does this mean about our architecture? How can we get cache
> from other places? How can we spread out the job across more steps, more
> processes, pre-process this one place. It gets back to, you don’t want to
> focus on over optimizing this one function. You want to make your overall
> architecture do the right things and just informs infrastructural changes.

But this raises the question --- can't we just do those more clever algorithms
and architectures in the inherently faster languages? And the answer is yes, yes
we can. And in the cases where concepts from pure functional programming like
persistent immutable data structures and such become useful, rather than a
hindrance, for those algorithms and architectures, even then pure functional
languages are at little advantage, because polyglot languages like Rust and C++
can *also* make use of those features handily, if not *quite* as easily. That's
the benefit of not being locked down to one single programming ideology: being
able to pull from any side when it becomes useful! Because the point here is not
to say that pure functional programming concepts are *never* useful, but that
they're just a tool in the toolkit, to be used alongside others, and programming
worldviews that eschew ideology in favor of using the "right tool for the right
job" will *always* be at an advantage here.

It's also worth noting something *else* that the Jane Street podcast interview
brings up: the garbage collector. [The GC is no small thing for real time
software](https://web.archive.org/web/20200205000006/https://blog.discordapp.com/why-discord-is-switching-from-go-to-rust-a190bbca2b1f),
whether hard real time or soft. Garbage collectors try to abstract away needing
to think about memory management at all, making sure that all the memory you
need is magically there whenever you need it and magically cleaned up whenever
you don't, as if the computer had infinite memory, but this is a complex process
that requires a complex algorithm and runtime structure to perform, since it's
trying to take an entire aspect of low-level coding that programmers would
otherwise have to consciously account for under its purview, so a fairly
intelligent algorithm is needed. For example, the nature of garbage collection
--- the need to keep everything in memory that might still be used, and remove
everything that isn't --- means that proper garbage collection typically needs
to have some sort of graph-walking algorithm, which is inherently pretty
nontrivial, especially when we're talking about graphs that aren't necessarily
acyclical, or even directed; and most garbage collectors also supplement their
graph walking algorithm with many other stages, generations, working areas in
memory, and so on. This means that you're spending extra processing power and
memory to run someone else's entire, quite complex, program piggybacking on
yours, but not directly called out to by you. Moreover, because the garbage
collector is *managing all your memory for you*, that means that it *owns* your
memory, which in turn means that when it wants to really properly run, it needs
to pause your program to do it, since your program can't run without access to
memory and running them simultaneously could lead to race conditions that
produce memory safety violations as their failure condition, which is
frightening in the extreme. This means that garbage collectors pretty much have
to act as separate threads, or at least separate programmatic entities, that
interrupt your program's threads in their execution and perform a fairly memory
and CPU-intensive algorithm at sporadic, unpredictable moments.[^1]

And the thing to keep in mind here is that, in reality, garbage collection
really doesn't allow you to stop thinking about memory: you still have to think
about and be careful of when you allocate things for performance reasons and you
still have to worry about memory leaks since you have to remember to eventually
delete an object if you don't need it any more or else it'll hang around in
memory forever. You also still need to worry about data races and stuff like
that. The only thing GCs allow you to ignore are use-after-free and double-free,
which absolutely is nice, but they do so by just keeping everything you might be
able to access in memory, as opposed to a strategy like Rust's, where instead
the language ensures that you only have access to things while that thing must
remain in memory (since memory allocation and deallocation is deterministic
under RAII and thus statically analyzable), so you have to keep in mind the
inherent memory costs of that --- and in a world of Microsoft Teams using 12GB
of memory, that might start to be relevant.

Garbage collection strategies are especially relevant to pure functional
programming languages because their entire concept of persistent immutable data
structures, and in fact most of their preferred data structures in general, such
as linked lists, rely on extensive (I would say excessive) heap allocation and
pointer indirection, which would be a quagmire to deal with manually.
Furthermore, `malloc` and `free` are, shockingly, impure operations, which means
that as a pure functional programmer you *have* to foist that off onto the
runtime.

#### Site of grace (III)

Haskell and OCaml are very fast languages. But often, in order to get that fast,
you have to give up many of their benefits and program something entirely in an
`IO` or `ST` monad in imperative style, since you can't expect a compiler to
magically turn one algorithm into an entirely different algorithm under the
hood, and programming in an imperative style in a purely functional manner is
less ergonomic and much more difficult than just doing it in a language that
supports that style as a first class entity --- this is demonstrated through
Haskell since it is the purest exemplar of pure functional programming qua
ideology, but imperative programming is a bit awkward and certainly inherently
inefficient in OCaml. Moreover, while you can get both languages to be fast,
there is an inherent performance overhead, both as a product of being functional
languages and therefore requiring almost everything to be boxed and on the heap,
and as a product of being garbage collected, and as a product of not just being
"high level" in the sense say C# or Java (both faster than OCaml or Haskell!)
are, but "high level" in the sense that their entire computation model is
totally separated from what computers actually do. Moreover, if you try to avoid
programming these languages in an imperative manner, and go for a pure
functional route, it doesn't matter how 

## Let's talk about tradeoffs

<span id="back"></span>

Okay, remember the [pin](#pin)? Let's talk about that a bit. Not my own
preferred solution, mind you, but the pure functional programming solution.

At this point, most programmers of the purely functional persuasion will be
absolutely screaming at the screen that they *understand* that side effects and
imperative programming are sometimes necessary, that's *why* Haskell has the
`IO` and `ST` monads and so on in the first place. And yes, that incurs a
certain level of cognitive load, they'll say, but it pays dividends in terms of
isolating and clearly marking out your side effecting (`IO`), imperative (`ST`)
and stateful (`State`) code. And sure, the type system will force you to
restructure your problems in weird ways in order to do the mathematical kungfu
needed to get things into the shape you want, because of all the monads and
linear types in the way, but so what? What you get out of that is safer, more
correct-from-the-start code!

Hell, you could say a lot of the same things I'm saying about pure functional
programming also can be said about Rust by C++ programmers or any statically
typed language by dynamically typed programmers! And in a sense you'd be right.
The difference is, it's all about tradeoffs. Is the penalty worth the benefit?

I don't think it is in this case.

Across the entire industry, [decades of tracked issues show that around 65-70%
of all high severity security vulnerabilities can be chalked up to memory safety
violations.](https://alexgaynor.net/2020/may/27/science-on-memory-unsafety-and-security/)
Additionally, when memory safety violations happen, you typically (best case
scenario) get a segmentation fault, or you can get completely garbage data or
undefined behavior. In comparison, having to deal with the borrow checker
imposes very little actual cognitive load compared to programming in C++,
because the borrow checker merely enforces the *exact* same single
ownership/move semantics/borrowing/RAII principles C++ programmers already use,
except now having to remember all that is offloaded onto the compiler instead of
the programmer, so it can remind you when you fail to follow those rules.
Furthermore, it doesn't force you to operate at a higher level of abstraction or
do anything confusing or weird when you need to drop down to `unsafe`. You just
need to remember the invariants you need to uphold --- just like you would in
C++. So yes you have more up front *frustration*, but very little extranious
cognitive load or added complexity in most cases. I think this is a clear win.
There *is a tradeoff*, and for low-security industries like perhaps video games,
if you're not already familiar it, adding a borrow checker alone (so ignoring
all the other ways Rust is better) may not be worth the extra annoyance (this is
Jonathan Blow's opinion), depending on how you feel having some up front
frustration with the borrow checker compares to not having to take on the
constant cognitive overhead of remembering to keep everything correct yourself,
when often, the borrow checker can lead you blindly down the *correct* path for
program structure.

Now, what about static versus dynamic languages? At least
[two](https://ieeexplore.ieee.org/document/9436020)
[studies](https://ieeexplore.ieee.org/document/7985711) that I could find found
that static type systems typically tend to find 15% of type errors. For more
advanced type systems like Rust's and OCaml's that let you encode more
invariants into your system, it might be more. As for effects on productivity,
the empirical results are inconclusive overall, but the
[one](https://www.researchgate.net/publication/254007140_Static_vs_Dynamic_Type_Systems_An_Empirical_Study_About_the_Relationship_between_Type_Casts_and_Development_Time)
very small study I was able to find that seemed to have reasonable methodology
seems to suggest that for less type-complex programs, it is far more productive
to use a dynamically typed language, while for more type-complex workloads,
those advantages melt away. This aligns in general with my personal experience
with the matter: after a certain level of complexity, the effort you have to go
to in order to manually type check your data and document what shape it should
be outweighs the convenience of dynamic typing, and if the study had extended to
projects larger than ten lines of code, based on my personal experience, I think
maybe we would have seen the relative development time switch, as more and more
developer time is taken up debugging the issues of dynamic typing which only
really rear their heads in large scale programs, manually specifying and keeping
track of the shape of the data, and manually inserting type checks for things.
But I don't really have any emperical evidence for this, and neither does anyone
else. Just their particular experiences and subjective preferences weighed
against those of others. So for smaller programs, or less type complex ones,
dynamic typing is empirically a win, and afterwards we don't really know --- I
may have my opinion, as may others, but there's no clear cut truth of it, and it
probably varies depending on team and problem. This is precisely why I'm not a
static typing ideologue. *I* like static typing, especially ML-family static
typing, because I don't like having to keep track of my data's shape, I want the
compiler to do it for me, and I like being able to cleverly encode invariants
into my type system, but I wouldn't scoff at someone who did a project in a
dynamically typed language, and certainly not when it was small.

Let's compare this with pure functional programming. Compared a hybrid
structured/data-oriented/functional approach that takes the critiques and ideas
of pure functional programming seriously, but doesn't go all out with monads and
pure functions to get that last 10% of perfection, can we really point to any
obvious tangible benefits? Pure functional programmers spend their time
comparing pure functional programming to the worst case of OO or imperative
programming, and assuming that proves we have to go all the way, but as we
approach more purely functional programming, we have to think in more
unintuitive ways --- ***mathematical thinking may be intuitive for academics,
but not for your average person, or even your average programmer, since you can
be bad at math but good at programming*** --- deal with more complex and strange
abstractions, fight with the compiler more, and all for the benefit of just a
*little* extra explicit labeling and encapsulation compared to just normal
immutable-by-default languages like OCaml, Rust, Scala, and Clojure. Do we know
if that extra benefit is worth the tradeoff? Especially in industries like
systems programming where pure functional programming is weaker? I'm not sure
that conclusion is at all obvious.

I think this is a classic case of binary thinking. Pure functional programmers
view the world as *either* horrible webs of OO or imperative nonsense, *or* all
the way pure functional, because they don't see purely functional programming as
having any drawbacks at all, and so they think that if *some* functional
programming adjustments to traditional programming are good, then *maximal*
functional programming adjustments must be maximally good! Or, alternatively,
they think that there's a sort of slippery slope here, where either your
program/programming language is purely functional, or it might as well not be
functional at all in the context of the benefits they're championing, because
even a little bit of accessible mutability or use of imperative programming at
different scales of your program will just instantly collapse back into a mess.
They fail to see things in terms of engineering tradeoffs: almost everything has
drawbacks, almost everything has benefits, even if they're to widely varying
degrees, and almost everything can be applied partially to optimize your
position on that scale. And indeed, it varies from project to project where that
curve is and how it looks.

Let's return to that [James Hague](https://prog21.dadgum.com/55.html) quote:

> My real position is this: 100% pure functional programing
> [doesn't](https://prog21.dadgum.com/54.html)
> [work](https://prog21.dadgum.com/3.html). Even 98% pure functional programming
> doesn't work. But if the slider between functional purity and 1980s
> BASIC-style imperative messiness is kicked down a few notches--say to
> 85%--then it really does work. You get all the advantages of functional
> programming, but without the extreme mental effort and unmaintainability that
> increases as you get closer and closer to perfectly pure [links inserted to
> give context to what he's saying].

This is coming from a man who chooses to write arcade games and bit-slinging
benchmarks in Erlang. And yet, here I am agreeing with him. Why? Because I think
this is, in the end, the reasonable take.

Pure functional programming has a lot to offer us, but it also has drawbacks
that grow exponentially the closer you get to purity, and they will eventually
outweigh what it has to offer. So choose the right place on that slider for
you/your project/your team.

## Why functional programming solves the wrong problem

Why is there this curve though? Why does pure functional programming even *have*
tradeoffs? Why isn't the binary thinking of the pure functional programmers,
that if a little PFP is good, then more PFP would be more gooder, correct?

Fundamentally, I think that's because they're paying attention to the wrong
problem.

Fundamentally, though, the problem with this isn't even just over-abstraction or
boilerplate. The problem with this is that it really isn't any better than
structured programming. Yes, you're forced to clearly mark which functions
involve state and which don't, and enforce some invariants at the boundaries,
but the backbone of your application, and probably many intermediate operations,
are going to involve manipulation of state one way or another. And whether you
encapsulate it in a state monad or fold over events to accumulate state or tail
call recurse it or call a function with your world state and have it pass a new
world state out, or call functions with just what they need and have them pass
diffs to the world state out, in the end, you really haven't changed the fact
that the backbone of your application is going to be stateful in *some* sense,
you've just moved it around in comparison to a structured imperative program.

The way proponents of pure functional programming avoid this unpleasant fact is
by comparing good, usually toy, best-case pure functional programming examples,
or abstract concepts like referential transparency that typically don't actually
apply at the scale of software architecture as a whole, to entire applications
programmed in a worst-case way --- i.e. using global mutable state and object
oriented programming. They're not comparing like kinds at all, because any
useful pure functional program is going to have to engage in state, mutation,
and side-effects on some level, so the comparison is moot. You're comparing the
beauty of the `qsort` function to the complexity of an entire imperative
application and from this concluding that pure functional programming is better.
In the end, though, you'll still have complex intermingling state dependency
graphs, and you'll still have imperative algorithms on the small scale, and the
possibility of accidental side effects or whatever. As long as you don't have
global state or webs of always-implicitly-mutable objects, I don't see how
structured programming is any worse than pure functional programming.

## Solving the right problem: PFP without PFP

It's possible to have imperative algorithms in functions that maintain
referential transparency. Why should those have to be marked out as unclean and
only callable from the central dispatch part of the code that's already marked
out as unclean, when you can just make sure that functions can't access any data
but what's passed into them, and make side effects governed by stack-allocated
structs that have single ownership, so that in order to perform side effects,
any function would have to have the struct governing that side-effect passed in.

It's also possible to have the central data and control flow of your program be
very clean and straightforward, so that it still largely follows the data
pipeline model that functional programming introduced, but still uses mutation
along the way for performance or convenience reasons. Just ensure that data has
a single owner, and only owners of that data, or those who already have a
mutable reference, can possibly hand mutable references out to anyone else. Then
make the central control flow path of your program the owner of your data, and
have it only hand out mutable references when necessary, to functions that
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
mechanism is the fact that that would be a code smell to your co-workers in both
cases. Additionally, if you enforce rules like Rust's "one mutable reference XOR
many immutable ones" model, this would quickly become basically impossible to
actually do!

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
thanks to being less ideological, as we've covered, while at the same time being
just as enforcible when it comes to the things that ***actually matter*** when
it comes to mutable state. And don't confuse the fact that it's *less strict
about mutable state* with it being less strict about what matters when it comes
to mutable state! Strictness toward mutable state isn't an end in itself, it's a
means to achieving maintainable programs. So the two are separable. A pure
functional programmer might look at a language that's less strict about mutable
state and feel less safe, like there are fewer guarantees, but that's the entire
confusion at the heart of the pure functional programming ideology: assuming
that because mutable state is often bad, it is always bad, and therefore must be
maximally minimized.

## Conclusion


[^1]: These drawbacks aren't *strictly* universal, though. The JVM in recent
    years has done some incredibly impressive, cutting-edge work with the
    [ZGC](https://www.baeldung.com/jvm-zgc-garbage-collector) to bring
    stop-the-world GC pauses down to (theoretically at least) a sub-1ms
    threshold. However, they achieve this by essentially requiring every pointer
    access to participate in garbage collection, thereby amortizing the costs of
    garbage collection over time, and otherwise spreading all the computational
    work of an *immensely* complex garbage collector throughout your program and
    over time. Meaning that yes, latency spikes are basically solved, but you
    pay a penalty in processing overhead throughout your entire program for that
    privilege. More than that, its memory footprint is probably around twice as
    much as a regular Java program (also, because of the way it maps memory to
    deal with colored pointers, it will *look* at least 3x that without actually
    being three times larger) and garbage collector pauses increase in time and
    frequency exponentially as you approach the limit of the size of the heap
    you've specified, which can significantly slow your program down.
