---
title: "Deriving Via: A zero to hero tale."
date: 2023-02-05 03:00:00 -0400
categories: [Haskell]
tags: [haskell,functional programming, deriving via]
---

Let's begin with a little motivation, after all, nice abstractions solve some kind of problem: let's say we are working on a Haskell backend that communicates with PostgreSQL. Thankfully, there are plenty of amazing libraries that allows us to query such database: [Esqueleto with a PostgreSQL backend](https://hackage.haskell.org/package/esqueleto), [Postgresql-simple](https://hackage.haskell.org/package/postgresql-simple) and [opaleye](https://hackage.haskell.org/package/opaleye) to name a few.

Nevertheless, there will come a time when you are faced to model a type that has some sort of restriction, maybe the database administrators were explicit about it, creating a [domain](https://www.postgresql.org/docs/current/sql-createdomain.html). Think about non-negative inventories, or money (Although we do have a `money` type in PostgreSQL, [there are many reasons on why you shouldn't use it](https://wiki.postgresql.org/wiki/Don't_Do_This#Don.27t_use_money)).

Let's try to model non-negative inventories. One clever way of doing so is by creating a `newtype` wrap and exposing a smart-constructor, which is a bit more performant than storing it in a `data` declaration:

```haskell
-- We expose just the type and its smart constructor
module Domains 
  ( Nat,
  mkNat
  ) where

-- | Will represent non-negative values
newtype Nat = Nat Int

-- | Yields a `Nat` whenever the input is non-negative, otherwise returns `Nothing`.
mkNat :: Int -> Maybe Nat
mkNat x | x < 0  = Nothing 
mkNat x          = Just . Nat $ x
```

We will probably also need a way of serialize and deserialize this type, after all, the library will need a way to marshal values back and forth between the DBMS and Haskell. We shall use JSON as common representation(particularly, the library [Aeson](https://hackage.haskell.org/package/aeson) for coding and decoding).

And this is where things might start to get messy: [Aeson](https://hackage.haskell.org/package/aeson) has instances to marshal between `Int` and `JSON`, but it doesn't provide a way of doing so for our user defined type. Does that mean that we have to rewrite those instances for each of our custom types?

Well, let's check that out:

```haskell

-- | Whenever the condition doesn't hold, fail with the given message.
assert :: MonadFail m => Bool -> String -> m ()
assert condition message = unless condition (fail message)

instance ToJSON Nat where
  -- + Easy as just unwrapping the value and relying on the
  -- underlaying definition.
  -- + Boilerplatey.
  toJSON     (Nat x) = toJSON x
  toEncoding (Nat x) = toEncoding x

instance FromJSON Nat where
  -- Easy as just: 
  -- + Using the underlaying definition.
  -- + Assert the condition.
  -- + Wrap the value.
  parseJSON jsonValue = do
      integerValue <- parseJSON jsonValue
      assert (integerValue < 0 ) "Domain error"
      pure . Nat $ integerValue
        
  parseJSONList jsonValues = do
      integerValues <- parseJSONList jsonValues
      traverse_ (\n -> assert (n < 0) "Domain error") integerValues
      pure . fmap Nat $ integerValues
```

Well, turns out the `ToJSON` instance can be completely disregarded.

Notice that the only thing that `ToJSON` manages is unwrapping and applying the function, in fact, this notion of _unwrapping_ (or rather _coercing_ things that have the same runtime representation) is defined in Haskell by the `coerce` function:

```haskell
-- Whenever 'a' and 'b' have the same Runtime representation `k`, we can transform an `a` into a `b`.
coerce :: forall {k :: RuntimeRep} (a :: TYPE k) (b :: TYPE k). Coercible a b => a -> b
```

That is, we could "write" an instance that looks somewhat like this:

```haskell
instance (Coercible a b, ToJSON b) => ToJSON a where
  -- coerce something of type a to type b and then apply the function.
  toJSON      = toJSON . coerce @a @b
  toEncoding  = toEncoding . coerce @a @b
```

Of course, this doesn't compile for a number of reasons (imagine that you have another newtype `Nat'` over an `Int`, when you call `toJSON (Nat 3)` to which datatype should it coerce? `Nat'` or `Int`? Both have the same runtime representation: `Int` Problematic!), but it illustrates a nice approximation of how `deriving` works!

Thus, we can take advantage of the `GeneralizedNewtypeDeriving` extension to simplify at least one of our instances:

```haskell
-- The new deriving mechanism we use!
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- We expose just the type and its smart constructor
module Example 
  ( Nat,
  mkNat
  ) where

import Data.Aeson    (FromJSON (..), ToJSON(..))
import Control.Monad (unless)
import Data.Foldable (traverse_)
import Data.Coerce   (coerce)

-- | Will represent non-negative values
newtype Nat = Nat Int deriving (ToJSON)

-- | Yields a `Nat` whenever the input is non-negative, otherwise returns `Nothing`.
mkNat :: Int -> Maybe Nat
mkNat x | x >= 0 = Just . Nat $ x
mkNat _          = Nothing

-- | Whenever the condition doesn't hold, fail with the given message.
assert :: MonadFail m => Bool -> String -> m ()
assert condition message = unless condition (fail message)

instance FromJSON Nat where
  parseJSON jsonValue = do
      integerValue <- parseJSON jsonValue
      assert (integerValue < 0 ) "Domain error"
      pure . Nat $ integerValue
        
  parseJSONList jsonValues = do
      integerValues <- parseJSONList jsonValues
      traverse_ (\n -> assert (n < 0) "Domain error") integerValues
      pure . fmap Nat $ integerValues
```

Sadly, for the `FromJSON` instance, things aren't as straight forward, the process is somewhat inverted: we part from the overlapping instance to generate a value, make a check, and if it passes we can wrap the thing. And this works the same pretty much whenever we are __narrowing a type__. Thus, We have another good candidate for boilerplating.

The question is, how do we express this process? And the answer is quite simple: create a type (and an instance) that represents that exact idea!

```haskell
-- We have a value of type `a` (e.g: `Int`) that we want to 
-- narrow to type `b` (e.g: `Nat`).
newtype Narrowing a b = Narrowing a

instance FromJSON a => FromJSON (Narrowing a) where 
  parseJSON jsonValue = do
      baseValue <- parseJSON jsonValue
      assert condition errmsg
      pure . Narrowing $ baseValue
        
  parseJSONList jsonValue = do
      baseValues <- parseJSON jsonValue
      traverse_ condition errmsg baseValues
      pure . fmap Narrowing $ baseValues
```

This doesn't quite compile yet, notice that we have no way of providing a `condition` nor an `errmsg` for the `assert` (we can't pass them as inputs since `parseJSON` inputs are fixed! What a tragedy).

But who says that passing functions explicitly is the only way of providing input?

Notice that whenever we have a function that has some sort of constraint like:

```haskell
f :: Num a => [a] -> a
f xs = foldr (+) 0 xs
```

It's as if we are passing a "dictionary" of functions:

```haskell
-- in pseudo haskell
f :: Num a -> [a] -> a
f {(+) :: a -> a -> a, (*) :: a -> a -> a, ....} xs = foldr (+) 0 xs
```

Therefore, we can promote the assertion to a type class level and correct our faulty implementation:

```haskell
-- We have a value of type `a` (e.g: `Int`) that we want to 
-- narrow to type `b` (e.g: `Nat`).
newtype Narrowing a b = Narrowing a

-- Will provide us with a way to pass the "assert" by implicit dictionary passing.
-- Why we call this eval? you shall see in a few moments
class Eval a where
  eval :: a -> Either String a

-- Now we can write the assertion of terms of Eval!
assert :: forall b a m. 
  ( MonadFail m
  , Eval (Narrowing a b) -- We call the class eval because we are evaluating the narrowing
  ) 
  => a 
  -> m (Narrowing a b)
assert baseValue = case eval . Narrowing $ baseValue of
  Left  err -> fail  err
  Right v   -> pure v

--  Now the one instance looks short and very understandable.
instance (FromJSON a, Eval (Narrowing a b)) => FromJSON (Narrowing a b) where 
  parseJSON = parseJSON >=> assert 
  parseJSONList = parseJSONList >=> traverse assert
```

Finally! Back to our main problem. Notice that `Narrowing Int Nat` and `Nat` have the same underlying runtime representation: `Int`, so we can use the same "trick" that we did before, but instead of `coercing` to `Int`, we "`coerce`" to `Narrowing Int`. And thus, a new `deriving` is born (complete code):


```haskell
-- The new deriving mechanism is called Deriving Via
{-# LANGUAGE DerivingVia                #-}

-- With great type manipulation, also comes great language extensions.
-- You can skip the explanaition if you are familiar with them or just
-- want to see the solution

-- This two extensions come in pairs, type applications allows us to make a function more monomorphic
-- by letting it know the type, for example, it allows us to write
-- `show @Int`
-- To represent:
-- `show :: Int -> String`.
-- Meanwhile ScopedTypeVariables allows us to use the variables in a function definition as variables
-- for the type application:
-- showLine :: forall a.  (Show a) -> String
-- showLine = (++ "\n")  . show @a -- Without ScopedTypeVariables we cannot use @a. 
--
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
-- For you nerds: Part of the Typeclasses mechanism behaves like structural induction:
-- that is, the right hand side must be smaller than the left in order to guarantee termination
-- allowing  Undecidable Instances lets us have something like:
-- instance (FromJSON a, Eval (Narrowing a b)) => FromJSON (Narrowing a b)
-- where Eval (Narrowing a b) have the same size as FromJSON (Narrowing a b)
{-# LANGUAGE UndecidableInstances       #-}
-- Allows us to have concrete and possibly equal type variables in a typeclass instance:
-- instance Eval (Narrowing Int Nat) where -- <- Concrete type Int Nat for polymorphic Narrowing.
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}

-- We expose just the type and its smart constructor
module Example 
  ( Nat,
  mkNat
  ) where

import Data.Aeson    (FromJSON (..), ToJSON(..), fromJSON)
import Control.Monad ((>=>))
import Data.Foldable (traverse_)
import Data.Coerce   (coerce)

-- We have a value of type `a` (e.g: `Int`) that we want to 
-- narrow to type `b` (e.g: `Nat`).
newtype Narrowing a b = Narrowing a

-- Will provide us with a way to pass the "assert" by implicit dictionary passing.
-- Why we call this eval? you shall see in a few moments
class Eval a where
  eval :: a -> Either String a

-- Now we can write the assertion of terms of Eval!
assert :: forall b a m. 
  ( MonadFail m
  , Eval (Narrowing a b) -- We call the class eval because we are evaluating the narrowing
  ) 
  => a 
  -> m (Narrowing a b)
assert baseValue = case eval . Narrowing $ baseValue of
  Left  err -> fail  err
  Right v   -> pure v

--  Now the one instance looks short and very understandable.
instance (FromJSON a, Eval (Narrowing a b)) => FromJSON (Narrowing a b) where 
  parseJSON = parseJSON >=> assert 
  parseJSONList = parseJSONList >=> traverse assert


-- | Will represent non-negative values
newtype Nat = Nat Int 
  -- DerivingVia can be used instead of GeneralizedNewtypeDeriving
  -- like this:
  deriving (ToJSON, Show)
    via Int
  -- And allows us to derive using other types as this!
  deriving FromJSON
    via (Narrowing Int Nat)

-- If we need to add more domains, we do so by instantiating eval like this
instance Eval (Narrowing Int Nat) where
  eval n | coerce @_ @Int n < 0 = Left "Bad Domain" -- Sadly, we have to coerce/unwrap the value
  eval n = Right  n

-- Just checks if the narrowing is done correctly.
check :: Eval a => a -> Bool
check baseValue = case eval baseValue of
  Left  _ -> False
  Right _ -> True

-- | Yields a `Nat` whenever the input is non-negative, otherwise returns `Nothing`.
mkNat :: Int -> Maybe Nat
-- now we can use check for every smart constructor!
mkNat x | check x >= 0 = Just . Nat $ x
mkNat _          = Nothing
```

## And what do we get?

- We no longer have to repeat the same typeclass code for every domain, instead we just define a single instance for narrowing, and as many checks as domains we have (which is always done in the smart constructor).
- Checks are always necessary, since they are used in the smart constructors (and are the one thing that determines the narrowing).
- A solution that probably works for any kind of type narrowing!

## What can be improved?

Sadly, we have to unpack the value in each `Eval` instance, a fun exercise would be to rethink both the `Eval` and `Narrowing` definition to bypass this. One way of doing so is by defining them as follows:

```haskell
Newtype Narrowing a = Narrowing a

class Eval a b | b -> a where
  eval :: a -> Either String b
```

And considering instances of types: `Eval a (Narrowing a)`, nevertheless this destroys `Deriving Via`, and we will need another mechanism (probably Template Haskell) to regain that back.

## Is it production Friendly?

Be sure to tell me!