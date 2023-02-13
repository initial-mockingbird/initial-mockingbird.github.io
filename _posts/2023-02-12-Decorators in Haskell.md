---
title: "Decorators in Haskell: Curry Away!"
date: 2023-02-12 23:30:00 -0400
categories: [Haskell]
tags: [haskell,functional programming, decorator, decorators, decorator pattern]
---

## Motivation

This one is straight out of the Haskell reddit. If you would like to read it from the source, you can do so [here](https://www.reddit.com/r/haskell/comments/10z07n7/comment/j81uhuq/?utm_source=share&utm_medium=web2x&context=3). But since we like our posts to be as self-contained as possible. Here goes the story!

Suppose we have a lot of functions that might fail, and let us assume that the way they fail is encoded in a particular Monad, say `Maybe` or `Parser`.

What we want to achieve is a way to provide default values for each of these functions. This might be useful if we are applying a numeric method, and we need to map the "failures" or `NaNs` to 0. 

In code, we would like to have something like this:

```haskell

-- List of functions that we want to have a default.
f0 :: Maybe Int
f0 = undefined

f1 :: Double -> Maybe String
f1 = undefined

f2 :: Int -> String -> Maybe Bool
f2 = undefined

f3 :: String -> Bool -> Int -> Maybe Double
f3 = undefined

-- Function that we want to achieve
withDefault = undefined

-- Comfortable interface
f0' = f0 `withDefault` 0
f1' = f1 `withDefault` "Error!"
f2' = f2 `withDefault` False
f3' = f3 `withDefault` 3.14
```

So, how do we proceed?

## A Naive attempt: The functor method

Something fun about functions, is that they are themselves functors, And thus allow us to map over them. 

The way this functor instance works, is by "skipping" or "lifting over" the arguments of the function:

```haskell
f :: Int -> Double
f = fromIntegral

transformation :: Double -> String
transformation = show

over :: Int -> String
over = transformation <$> f
{-
Notice how it skips the variable
f                    :: Int -> Double
transformation       ::        Double -> String
f <$> transformation :: Int      ->      String
-}
```

Moreover, if we have a nested functor structure, we can compose `fmap` in order to target the inner layers:

```haskell
-- 2 layered structure
f2 :: Int -> Bool -> Double
f2 x b 
  | b         = fromIntegral x * 2
  | otherwise = fromIntegral x * 2 + 1

-- 3 layered structure
f3 :: Int -> Bool -> String -> Double
f3 x b _
  | b         = fromIntegral x * 2
  | otherwise = fromIntegral x * 2 + 1

transformation :: Double -> String
transformation = show

-- we use 2 fmaps in order to penetrate 2 layers
over1 :: Int -> Bool -> String
over1 = fmap transformation <$> f2

-- we use 3 fmaps in order to penetrate 3 laters
over2 :: Int -> Bool -> String -> String
over2 = (fmap . fmap) transformation <$> f3
```

So, in a way, we notice a pattern, we can just compose as many `fmap`s as we need in order to reach the base type. If we additionally use the [Alternative](https://hackage.haskell.org/package/base-4.17.0.0/docs/Control-Applicative.html#g:2) typeclass to handle errors, we could write customs `withDefault` functions for each arity:

```haskell
withDefault' :: (Applicative f, Alternative f) => f a -> a -> f a
withDefault' computation defaultValue = computation <|> pure defaultValue

-- Notice the pattern:

f0' = f0 `withDefault'` 0
  where
    withDefault f defaultValue = id (`withDefault` defaultValue) f
{-                                ^
                              f0 has 0 arguments thus
                                0 fmaps composition
-}

f1' = f1 `withDefault` "Error!"
  where 
    withDefault f defaultValue = fmap (`withDefault'` defaultValue) f
{-                                ^
                              f1 has 1 argument thus
                                1 fmaps composition
-}

f2' = f2 `withDefault` False
  where 
    withDefault f defaultValue = (fmap . fmap) (`withDefault'` defaultValue) f
{-                                ^
                              f2 has 2 argument thus
                                2 fmaps composition
-}

f3' = f3 `withDefault` 0.0
  where 
    withDefault f defaultValue = (fmap . fmap . fmap) (`withDefault'` defaultValue) f
{-                                ^
                              f3 has 3 argument thus
                                3 fmaps composition
-}
```

And one might naively try to generalize this result as follows:

```haskell
withDefault arity defaultValue f = solution
  where
    fmapList      = replicate arity fmap
    composedFmaps = foldl1 (.) fmapList -- This fails because fmaps have different types!
    solution      = composedFmaps (`withDefault'` defaultValue) f
```

Nevertheless, this won't compile! This is due to the fact that lists are homogeneous: they hold exactly one type:


```haskell
-- What type has fmapList?
-- let's say: 
-- fmapList :: Functor f => [(a -> b) -> f a -> f b]
fmapList = [fmap,fmap]

composedFmaps = foldl1 (.) fmapList
{- this will yield:
 (fmap . fmap)
 but this types don't match!

Let's write a more simple equivalent version:

foo transform = fmap (\layer1 -> fmap transform layer1)
                 ^                ^       
              This one maps the   ^
              outer functor f1    ^
                           This one maps the
                           inner functor f2

f1 does not need to unify with f2. Not even in our simple
case, an example of this would be:

f1 ~ (->) Int
f2 ~ (->) Bool

They are different! we cannot have a list that holds:

[Int -> a, Bool -> a] <- Illegal!
-}
```

Thus, we are at an impasse, we could generate a `withDefault` function for each possible arity, either by hand or doing some template haskell. Nevertheless, this feels unsatisfactory.

## A better abstraction

Let's remember what we would like to do:

- Take a function that might fail
- Execute it
- Check if it fails, and if it does return a default value
- Otherwise return the result.

This is awfully close to what a decorator does in languages like python or java. In fact, this is exactly what a decorator does!

So, in essence, we would like to model the following behavior:

```python
def withDefault(default):
  def defaulted(func):
    def wrapper(*args, **kwargs):
      result = func(*args, **kwargs)
      if result is None: return default
      return result
    return wrapper
  return defaulted

@withDefault("Defaulted")
def f(i : int, j : str) -> str | None:
  if (i == 0): return None 
  return str(i) + ", " + j

# Prints "Defaulted"
print(f(0,"Test"))
# Prints "1, Test"
print(f(1,"Test"))
```

If we translate this to haskell, we would end up having
a function that looks like this:

```haskell
withDefault :: Alternative f => a -> (*args -> f a) -> *args -> f a
withDefault defaultValue function args = function args <|> defaultValue 
```

And there we have the problem, the way python and other languages model functions are as tuples that return a result, in essence all the functions in python are single input functions that accept an `n-tuple`, and on top of that, we have a way to refer to arbitrarily long tuples: `*args`. If we combine these two features it allows us to have easy to define decorators.

Haskell, on the other hands works with curried functions, and does not provide a way to easily talk about arbitrarily long tuples. Which makes decorators in haskell a bit more difficult do define.

Thankfully, [Tim Sheard and Nathan Collins already made a paper about it!](http://web.cecs.pdx.edu/~ntc2/haskell-decorator-paper.pdf) and the implementation turns out to be pretty straight forward.

If the issue is handling curried functions due to the arbitrary nesting that have to get to the result, then we can transform them to uncurried functions, which posses a single layer, `fmap` the transformation, and curry the function back.

The way we describe the process of `Uncurry` is as follows:

```haskell
-- Dummy datatype for Nats that has the property:
-- The smaller the number the smaller the stack of Succs.
-- Perfect for Induction!
data Nat = Zero | Succ Nat


class Uncurry (n :: Nat) (t :: Type) where
  --            ^
  -- We need to adquire the arity of the function
  -- Since this will help us to generate the tuple.
  -- We do so by providing it explicitely.


  -- We must also model the *args construct,
  -- we do so using a type family. Since
  -- curried functions are inductive,
  -- we can construct the tuple inductively too
  -- one argument at a time.
  type Args n t :: Type
  
  -- We will also need to point out the return
  -- type, since this is usually "hidden" deep
  -- down the stacked structure.
  type Ret  n t :: Type

  -- Finally, an uncurry!
  uncurry :: t -> Uncurried n t

-- The ideal type for decorating.
type Uncurried n t = Args n t -> Ret n t
```

Now, we only must provide instances for functions when the numbers of arguments are greater than zero (inductive step), and when they are zero (base case).

But before proceeding, we must point out a caveat. Since haskell doesn't provide an easy way of extending a tuple (which in our context is adding arguments). We will have to use nested tuples instead. A minor drawback considering how easy it is to implement the instances:

```haskell

-- Inductive case
instance (Uncurry n b) => Uncurry (Succ n) (a -> b) where
  -- we have something of the form: a -> (b -> c -> .... -> result)
  -- thus we transform it into a tuple: 
  --   (a, Args (b -> c -> .... -> result))
  -- = (a,  (b, Args c -> .... -> result))
  -- = (a,  (b, c, (.... -> result)))
  type Args (Succ n) (a -> b) = (a , Args n b)
  -- the return value is just the return value of
  -- the "tail"
  type Ret  (Succ n) (a -> b) = Ret n b

  uncurry f (x , xs) = uncurry @n (f x) xs

-- Base case
instance Uncurry Zero b where
  -- When we have no more args, we can return ()
  -- Since 0 argument functions are isomorphic to
  -- functions that recieve the Unit type.
  type Args Zero b = ()
  type Ret  Zero b = b
  uncurry f () = f
```

Awesome, now, defining the `Curry` class is somewhat similar:

```haskell
class Curry (args :: *) (b :: *) where
  -- A way to transform (a,b,c,d) -> res
  -- to a -> b -> c -> d -> res
  -- a ->* b can be read as, from 'a', you can retrieve
  -- 'b' after 0 or more applications.
  type args ->* b :: Type
  
  curry :: (args -> b) -> (as ->* b)
```

And the instances also look similar:

```haskell
-- Inductive case
instance Curry as b => Curry (a , as) b where
  --                            ^
  --                      Remember we use
  --                      nested tuples!
  --                      so as ~ (b,bs)

  -- And we transform the nested tuples
  -- one layer at a time
  type (a , as) ->* b = a -> (as ->* b)
  curry f x = curry (\ xs -> f (x , xs))

-- Base case
instance Curry () b where
  type () ->* b = b
  curry f = f ()
```

Finally, We can define our `withDefault` function as follows:

```haskell
f2 :: Int -> String -> Maybe Bool
f2 0 _ = Nothing
f2 _ _ = Just True


withDefault :: forall n t f a. 
 (Ret n t ~ f a
 , Alternative f
 , Uncurry n t
 , Curry (Args n t) (f a)
 ) =>
  t -> a -> Args n t ->* f a
withDefault f att = curry ((<|> pure att) <$> uncurry @n f)


f2' :: Int -> String -> Maybe Bool
f2' = withDefault @(Succ (Succ Zero)) f2 False
```

And thus, we have found a way of both: decorating functions and provide
a default value to our computations!

## Complete code

```haskell
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

import           Control.Applicative (Alternative ((<|>)), Applicative (pure),
                                      (<$>))
import           Data.Kind           (Type)
import           Prelude             hiding (curry, uncurry)


data Nat = Zero | Succ Nat

class Curry (as :: *) (b :: *) where
  type as ->* b :: Type
  curry :: (as -> b) -> (as ->* b)

instance Curry as b => Curry (a , as) b where
  type (a , as) ->* b = a -> (as ->* b)
  curry f x = curry (\ xs -> f (x , xs))

instance Curry () b where
  type () ->* b = b
  curry f = f ()


class Uncurry (n :: Nat) (t :: Type) where
  type Args n t :: Type
  type Ret  n t :: Type
  uncurry :: t -> Uncurried n t

type Uncurried n t = Args n t -> Ret n t

instance (Uncurry n b) => Uncurry (Succ n) (a -> b) where
  type Args (Succ n) (a -> b) = (a , Args n b)
  type Ret  (Succ n) (a -> b) = Ret n b

  uncurry f (x , xs) = uncurry @n (f x) xs

instance Uncurry Zero b where
  type Args Zero b = ()
  type Ret  Zero b = b
  uncurry f () = f

type CurryUncurry (n :: Nat) (t :: *) =
  ( Uncurry n t
  , Curry (Args n t) (Ret n t)
  , (Args n t ->* Ret n t) ~ t 
  )

withDefault' :: (Applicative f, Alternative f) => f a -> a -> f a
withDefault' computation defaultValue = computation <|> pure defaultValue


f2 :: Int -> String -> Maybe Bool
f2 0 _ = Nothing
f2 _ _ = Just True


withDefault :: forall n t f a. 
 (Ret n t ~ f a
 , Alternative f
 , Uncurry n t
 , Curry (Args n t) (f a)
 ) =>
  t -> a -> Args n t ->* f a
withDefault f att = curry ((<|> pure att) <$> uncurry @n f)


f2' :: Int -> String -> Maybe Bool
f2' = withDefault @(Succ (Succ Zero)) f2 False
```

## What we learned

- How to model the Decorator Pattern in haskell.
- How to apply the Decorator Pattern to the problem of defaulting a function.

## What can be improved

- Sadly, since we rolled our own version of `Nat`, it's not overloaded for writing them with digits. I saw that such a thing can be done with the extension `Rebindable Syntax`, or maybe Overloading `fromInt` of `Num` but I have yet to test this out.
- A way to just skip passing the arity. This probably should be done using template haskell.

## Finally

Although I believe I summarized well how to implement the decorator pattern, be sure to give [the paper](http://web.cecs.pdx.edu/~ntc2/haskell-decorator-paper.pdf) a read. It's short, well written and touches other topics, such as memoization and monads.