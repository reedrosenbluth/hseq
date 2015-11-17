# Djembe

Djembe is a Haskell embedded domain-specific language for composing drum beats.

It is currently a work in progress.

## Example

Let's define a few quarter note length drum hits
```haskell
> bass  = n4 $ Hit BassDrum1 0 100
> snare = n4 $ Hit SnareDrum2 0 100
> hihat = n4 $ Hit ClosedHihat 0 100
```

Now let's create a simple composition
```haskell
> beat = mkComposition [bass, snare, bass, hihat]
```

Finally we can play it
```haskell
> play beat
```

## Inspiration
- [Bang](https://github.com/5outh/Bang)
- [The Haskell School of Music / Paul Hudak](http://haskell.cs.yale.edu/?post_type=publication&p=112)
