Names: Reed Rosenbluth and Jonathan Dubin
Pennkeys: reedros and dubinj

# Djembe

### What was the goal of your project?

The goal of the project was to create an embedded DSL for composing
and playing drum beats.


### What was the most significant challenge that you faced in completing the project?

The most significant challenge was figuring out to best represent a
drum beat in Haskell so that they can easily be combined to create new
beats.

Another significant challenge was figuring out how to test our
code. At first, we were testing the project by simply listening to the
beat and making sure it sounded like we expected. After getting
feedback from the project checkpoint we realized we could use
quickcheck to test properties of the underlying Haskell representation
of our beats. To that end, we had to refactor the code a bit to make
it amenable to testing.


### What is the most impressive aspect of what you built?

I think the most impressive aspect of our project is how we combined a
broad set of abstract Haskell constructs (eDSL, Monad, Monoid, etc.)
to achieve our goal of making drum beats. Additionally, we implemented
a DSL called [DSEQ](http://www.csounds.com/journal/issue8/dseq.html) (**d**rum **seq**encer) on top of our DSL to make it even easier to
compose beats.


### What was the most interesting thing you learned?

After using Haskell for a semester, it's tempting to draw the
conclusion that Haskell's powerful type system and the more
"algebraic" typeclasses are great for academic exercises, but would
get in the way of more practical work. However, throughout the project
it became very clear that Haskell made what could have been extremely
awkward and difficult in other programming languages, much simpler.


### What grade do you feel you deserve for this project, and why?

We feel that we deserve an A for this project, namely because we
really leveraged the power of Haskell to create a cool and useful
library. We really tried to take advantage of the Haskell language,
doing everything from creating our own data types, to "arbitrary"
instances of those types, and instances of the monad, applicative,
functor, and monoid typeclasses. And we were able to use these to
create a project that is a lot of fun to play with and use.


### Any other comments?

We really had a lot of fun in this class and especially working on
this project!


### What resources did you use in completing the project?

- [Bang](https://hackage.haskell.org/package/Bang)
- [DSEQ](http://www.csounds.com/journal/issue8/dseq.html)
- [Haskell School of Music](http://haskell.cs.yale.edu/?post_type=publication&p=112)

===========================================================================

# A summary of the files

### src/Types.hs

Types.hs contains the fundamental data types that we use throughout
the project. The main types are Sound, Hit, Beat, Composition, and
Song. In addition to defining these types, we also create a number of
typeclass instances, including the arbitrary instances for Sound, Hit,
Beat, and Song that we use for testing. We also define (for some of
our types) typeclass instances for Functor, Applicative, Ord, Monad,
and Monoid.


### src/Play.hs

Play.hs is where we actually convert from lists of Hits to listenable
MIDI. The `play` function is the principle function here: it's what
users will typically use to play their compositions. The other
functions in this file are called by play to transform the composition
into MIDI events and finally to play the MIDI events on the user's
computer.


### src/Drums.hs

The elements and functions defined in Drums.hs will help a user get
started making beats. We include definitions for several basic hits,
like bass drum and high hat hits. We also include functions that the
user can use to easily transform hits to songs (a specialized
composition), to change the velocity of their song, and to loop their
songs.

### src/Dseq.hs The dseq function let's users use dseq notation to
create beats. For examples of dseq notation, check out Example.hs.


### src/Example.hs

Example.hs contains several beats that have already been defined that
the user can play around with and use as inspiration for their own
beats. We make liberal use of dseq notation here to construct our
beats.


### src/Interpret.hs

Interpret contains a number of functions related to interpreting
beats. Two notable functions here are mergeHits and toHits, which we
use to merge two lists of hits and to take a composition and turn it
into a list of hits, respectively.


### test/DjembeSpec.hs

DjembeSpec is home to our test cases. We are using the HSpec library to
organize our quickcheck and unit tests. The quickcheck tests confirm
that layering beats (using the mappend operator) is associative and
commutative, and that sequencing beats (using bind) is
associative. Because of how the code is organized, these tests also
test other portions of the codebase, including song construction.


# Usage:
First, you'll need to download and install
[SimpleSynth](http://notahat.com/simplesynth/). Next, you'll need to
set up a MIDI IAC driver. Open Audio MIDI Setup (in `Applications ->
Utilities`) and press ⌘2 (or go `Window -> Show MIDI Window`). You
should see a slightly greyed out **IAC Driver** icon. Double click it,
then check the box labeled "Device is online."  Launch SimpleSynth and
set the MIDI Source to "**IAC Driver Bus 1**" (or whatever you named
your IAC driver) using the drop-down box at the top of the
window. Audio from `Djembe` should now feed into and play through
SimpleSynth. Alternatively, you can now use Garageband or any other
software to can play MIDI to play your drum beats.

Now clone and install the dependencies with Stack

    git clone https://github.com/reedrosenbluth/Djembe
    brew install stack
    stack build

# Usage:
1. Open the repl: `stack ghci`
2. Load the example beats: `:l Example`
3. Play a beat: `play funky 210`

To Test run `stack test`.