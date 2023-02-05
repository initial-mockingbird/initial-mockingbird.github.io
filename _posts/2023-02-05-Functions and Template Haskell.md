---
title: "Functions and Template Haskell: an alternative."
date: 2023-02-05 16:40:00 -0400
categories: [Haskell]
tags: [haskell,functional programming, template haskell]
---

## Borrowing Motivation

We shall use the same motivation that we had in the [Deriving Via]({% post_url 2023-02-05-Deriving Via %}), if you haven't read that one, don't worry! Here is a short description:

Suppose that we are working with Haskell in the backend, and we need to communicate with PostgreSQL. So we start modeling our tables until we find out that the database administrators use very narrow types for certain actions: they created [domains](https://www.postgresql.org/docs/current/sql-createdomain.html) for non-negative inventory, money, and possibly much else.

Haskell, being the type safe language that it is, encourages us to make types that matches the domain, so for a non-negative inventory, we could have a type that looks like this:

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
mkNat x | x >= 0 = Just . Nat $ x
mkNat _          = Nothing
```

Nevertheless, in [Deriving Via]({% post_url 2023-02-05-Deriving Via %}) we saw that this type of narrowing can come with some boilerplate:

- We will possibly need to have some type class instances for `Nat` that matches the one for it's underlying representation. For example, both `Nat` and `Int` are represented as JSON numbers, so we would like to `derive` a `ToJSON` instance directly from `Int`, which can be done either by using the `GeneralizedNewtypeDeriving` extension, or a clever `DerivingVia` extension.
- We will also need to have some type class instances for `Nat` that _almost_ matches the ones for `Int`. An example of that would be the `FromJSON` class. We can deserialize a `JSON` object into an `Int`, but we need an additional check in order to narrow it down to a `Nat`.

We want to study how to do this narrowing in a better way.

## Real motivation: Template Haskell

Reality is that we already have a nice and simple way of solving that problem, what this post really means to do is showing you a technique that can make Template Haskell a bit more flexible.

## Hands on!

So, let's take a look on how we could define a `FromJSON` instance of `Nat`:

```haskell
-- | Whenever the condition doesn't hold, fail with the given message.
assert :: MonadFail m => Bool -> String -> m ()
assert condition message = unless condition (fail message)

instance FromJSON Nat where
  -- Easy as just: 
  -- + Using the underlaying definition.
  -- + Assert the condition.
  -- + Wrap the value.
  parseJSON jsonValue = do
      integerValue <- parseJSON jsonValue
      assert (integerValue >= 0 ) "Domain error"
      pure . Nat $ integerValue
        
  parseJSONList jsonValues = do
      integerValues <- parseJSONList jsonValues
      traverse_ (\n -> assert (n >= 0) "Domain error") integerValues
      pure . fmap Nat $ integerValues
```

Right off the bat we notice that this code is pretty general: any narrowing will have this exact structure:

- Pull the value using the underlying instance
- Make the check.
- Wrap the result.

So, our goal is to define a function that given a type, and it's underlying representation, yields a `FromJSON` instance:

```haskell

-- template-haskellish for what we want to achieve. This won't compile yet
genFromJSONNarrowed :: forall t runtimeRep. (FromJSON runtimeRep) => Q [Dec]
genFromJSONNarrowed = [d|   
  instance FromJSON t where
    parseJSON jsonValue = do
      parsedValue <- parseJSON @runtimeRep jsonValue
      assert condition errmsg
      pure . newtypeConstructor $ parsedValue

    parseJSONList jsonValues = do
        parsedValues <- parseJSONList @runtimeRep jsonValues
        traverse_ (\n -> condition errmsg) parsedValues
        pure . fmap newtypeConstructor $ integerValues
  |]
```

Nevertheless, this code won't compile for a number of reasons, the first one is that the condition in the code is fixed, we need to write a condition that works for **any** type `t`! That's impossible, imagine trying to "pattern match" each possible type to give a condition, they are infinite!.

So, since we are creating a function, we could _try_ to pass the condition as a parameter:

```haskell

-- template-haskellish for what we want to achieve. This won't compile yet
genFromJSONNarrowed :: forall t runtimeRep.
 (FromJSON runtimeRep)
 => (runtimeRep -> Bool) -> String -> Q [Dec]
genFromJSONNarrowed condition errMsg = [d|   
  instance FromJSON t where
    parseJSON jsonValue = do
      parsedValue <- parseJSON @runtimeRep jsonValue
      assert condition errmsg
      pure . newtypeConstructor $ parsedValue

    parseJSONList jsonValues = do
        parsedValues <- parseJSONList @runtimeRep jsonValues
        traverse_ (\n -> condition errmsg) parsedValues
        pure . fmap newtypeConstructor $ integerValues
  |]
```

But sadly that won't work, the good old compiler will yell at us saying that: `runtimeRep -> Bool` doesn't have a `Lift` instance. The reason this happens is that template haskell isn't able to "inject" or rather _Lift_ just any function, in particular it can't lift local functions such as:

```haskell
g x = let f y = x + y in f [|f|]
```

Template Haskell needs to be able to expand `f` at compile time, nevertheless `f` is dependent on a runtime parameter `x`. What will the generated code look like? What is instantiated instead of `x`? TH doesn't quite know how to do.

So, if the problem is that we need to know about everything at compile time, and we need a way to overload the `assert` function in order to work on arbitrary types, it seems our best bet is typeclasses!

But we are going to make a twist here, instead of encoding the assert like we did in [Deriving Via]({% post_url 2023-02-05-Deriving Via %}), we are going to actually "promote" the function as follows:

```haskell
-- `Symbol` can be thought of as a type level string.
class Eval (name :: Symbol) a b where
  eval :: a -> b
```

Notice that this class encodes functions almost verbatim: each function has a name (`Symbol`), an input type (`a`) and an output type (`b`). Moreover, we don't impose any other restrictions in order to allow overloading.

So, we can encode a narrowing function like this:

```haskell

type NarrowResult b = (Either String b)
instance Eval "Narrow" Int (NarrowResult Nat) where
  eval n | n >= 0 = Right . Nat $ n 
  eval _          = Left "Bad Domain"
```

Allowing us to rewrite our TH funcion as follows:

```haskell
-- Now we can write the assertion of terms of Eval!
assert :: forall a b m. 
  ( MonadFail m
  , Eval "Narrow" a b -- We call the class eval because we are evaluating the narrowing
  ) 
  => a 
  -> m b
assert baseValue = case eval baseValue of
  Left  err -> fail  err
  Right v   -> pure v


-- Closer!
genFromJSONNarrowed :: forall t runtimeRep. 
  ( Eval "Narrow" runtimeRep (NarrowResult t)
  , FromJSON runtimeRep
  ) 
  => Q [Dec]
genFromJSONNarrowed = [d|   
  instance FromJSON t where
    parseJSON = parseJSON @runtimeRep >=> assert @Int @Nat
    parseJSONList = parseJSONList @runtimeRep >=> traverse assert
  |]
```

And although this compiles, when we try to generate the TH splice in another file, we shall get an issue, because we cannot pass the types `t,runtimeRep` to the slice as is, we must transform them into TH names.

Thankfully, there is a package that allows us to extract type representations in runtime! So, by using `Typeable`, `mkName` from TH and assuming that the show instance of the type representation is the same as the type representation itself, we can make an instance that atcually works:

```haskell
-- in main file
genFromJSONNarrowed :: forall t runtimeRep. 
  (Eval "Narrow" runtimeRep (NarrowResult t)
  , Typeable runtimeRep
  , Typeable t
  , FromJSON runtimeRep
  ) 
  => Q [Dec]
genFromJSONNarrowed = [d|   
  instance FromJSON $t' where
    parseJSON = parseJSON @($runtimeRep') >=> assert @($runtimeRep') @($t')
    parseJSONList = parseJSONList @($runtimeRep') >=> traverse (assert @($runtimeRep') @($t'))
  |]

  where
    t'            = conT . mkName . show . typeRep $ Proxy @t
    runtimeRep'   = conT . mkName . show . typeRep $ Proxy @runtimeRep


```

And that's it! We now have a mechanism that derives instances for narrowing types based on template Haskell, we just need to hop on another file and call the slice:

```haskell
-- in another TH exclusive file
$(genFromJSONNarrowed @Nat @Int)
```

## Final solution

```haskell
-- One of those Yes But moments
-- https://twitter.com/ChShersh/status/1575460560161980418
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE FlexibleContexts      #-}


-- We expose just the type and its smart constructor
module Domains.Internal.Example where

import           Control.Monad       ((>=>))
import           Data.Aeson          (FromJSON (..), ToJSON (..), fromJSON)
import           Data.Coerce         (coerce)
import           Data.Foldable       (traverse_)
import           Data.Typeable       (Proxy (Proxy), Typeable, typeRep)
import           GHC.TypeLits        (Symbol)
import           Language.Haskell.TH (Dec, Q, conT, mkName)


-- | Will represent non-negative values
newtype Nat = Nat Int 

mkNat :: Int -> Maybe Nat
mkNat x | x >= 0 = Just . Nat $ x
mkNat _          = Nothing

class Eval (name :: Symbol) a b where
  eval :: a -> b

type NarrowResult b = (Either String b)
instance Eval "Narrow" Int (Either String Nat) where
  eval n | n >= 0 = Right . Nat $ n 
  eval _          = Left "Bad Domain"


-- Now we can write the assertion of terms of Eval!
assert :: forall a b m. 
  ( MonadFail m
  , Eval "Narrow" a (NarrowResult b) -- We call the class eval because we are evaluating the narrowing
  ) 
  => a 
  -> m b
assert baseValue = case eval @"Narrow" @a @(NarrowResult b) baseValue of
  Left  err -> fail  err
  Right v   -> pure v


-- template-haskellish for what we want to achieve. This won't compile yet
genFromJSONNarrowed :: forall t runtimeRep. 
  (Eval "Narrow" runtimeRep (NarrowResult t)
  , Typeable runtimeRep
  , Typeable t
  , FromJSON runtimeRep
  ) 
  => Q [Dec]
genFromJSONNarrowed = [d|   
  instance FromJSON $t' where
    parseJSON = parseJSON @($runtimeRep') >=> assert @($runtimeRep') @($t')
    parseJSONList = parseJSONList @($runtimeRep') >=> traverse (assert @($runtimeRep') @($t'))
  |]

  where
    t'            = conT . mkName . show . typeRep $ Proxy @t
    runtimeRep'   = conT . mkName . show . typeRep $ Proxy @runtimeRep
```

## What we learned

- Another way of narrowing types.
- A simple way of passing functions to Template Haskell code.

## Drawbacks of this solution

Sadly, due to TH stage restriction, our hierarchy will end up looking like this:

```
- Public Module that reexports most things from Internals
- Internals 
  - Module that exposes everything and holds TH definitions in case of circular dependencies
  - TH file with slice applicatio.
```

This means that the TH file will be full of orphaned instances, and therefore should be put on an Internal module. But also will mean that we will have to manually reexport most of the things from the non-TH module since it will surely have non-public constructors.