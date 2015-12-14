# Djembe

Djembe is a Haskell embedded domain-specific language for composing drum beats.

## Example

Let's define a few quarter note length drum hits
```haskell
bass  = n4 $ hsong (Hit BassDrum1 0 100)
snare = n4 $ hsong (Hit SnareDrum2 0 100)
hihat = n4 $ hsong (Hit ClosedHihat 0 100)
```

Now let's create a simple composition
```haskell
beat = bass <> snare <> bass <> hihat
```

Finally we can play it at a specific tempo
```haskell
> play beat 200
```

We can also compose beats using [dseq](http://www.csounds.com/journal/issue8/dseq.html) notation
``` haskell
funky = dseq OpenHihat   8 ".... ...7 .... .7.."
     <> dseq ClosedHihat 8 "7777 777. 7777 7.77"
     <> dseq SnareDrum1  8 ".... 7..7 .7.7 7..7"
     <> dseq BassDrum1   8 "7.7. ..7. ..7. .7.."
```

## Inspiration
- [Bang](https://github.com/5outh/Bang)
- [The Haskell School of Music / Paul Hudak](http://haskell.cs.yale.edu/?post_type=publication&p=112)
- [dseq](http://www.csounds.com/journal/issue8/dseq.html)
