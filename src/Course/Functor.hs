{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Functor where

import Course.Core
import Course.Id
import Course.Optional
import Course.List
import qualified Prelude as P

class Functor f where
  -- Pronounced, eff-map.  This is just a more generic map function (we have seen it where f = List)
  (<$>) ::
    (a -> b)
    -> f a
    -> f b

infixl 4 <$>

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Course.Core
-- >>> import qualified Prelude as P(return, (>>))

-- | Maps a function on the Id functor.
--
-- >>> (+1) <$> Id 2
-- Id 3
instance Functor Id where
  -- data Id a = Id a
  -- (<$>) :: (a->b) -> f a -> f b
  -- (<$>) :: (a->b) -> Id a -> Id b
  (<$>) f (Id a) = Id(f a)

-- | Maps a function on the List functor.
--
-- >>> (+1) <$> Nil
-- []
--
-- >>> (+1) <$> (1 :. 2 :. 3 :. Nil)
-- [2,3,4]
instance Functor List where
  -- (<$>) :: (a->b) -> f a -> f b
  -- (<$>) :: (a->b) -> List a -> List b
  (<$>) _ Nil = Nil
  -- (<$>) f (h :. t) = f h :. (f <$> t) -- this is just map!
  (<$>) f (h :. t) = f h :. (<$>) f t -- this is just map! in prefix position
  -- so you can also write it as MAP!
  -- (<$>) f list = map f list 


-- | Maps a function on the Optional functor.
--
-- >>> (+1) <$> Empty
-- Empty
--
-- >>> (+1) <$> Full 2
-- Full 3
instance Functor Optional where
  -- (<$>) :: (a->b) -> f a -> f b
  -- (<$>) :: (a->b) -> Optional a -> Optional b
  (<$>) _ Empty = Empty
  (<$>) f (Full a) = Full (f a)

  -- you can also write this using mapOptional



-- | Maps a function on the reader ((->) t) functor.
--
-- >>> ((+1) <$> (*2)) 8
-- 17
instance Functor ((->) t) where
  -- (<$>) :: (a->b) -> f a -> f b
  -- (<$>) :: (a->b) -> ((->) t) a -> ((->) t) b -- replace with operator
  -- (<$>) :: (a->b) -> (->) t a -> (->) t b -- refactor to remove implied brackeets
  -- (<$>) :: (a->b) -> (t -> a) -> (t -> b) -- change from prefix to infix
  -- (<$>) :: (a->b) -> (t -> a) -> t -> b -- remove implied brackets

  -- NOTE that this is the same function signature as function composition (.)
  -- similar structure to mapping across a list. 
  -- (.) :: (b -> c) -> (a -> b) -> a -> c

  -- (<$>) ::   f    ->   g      -> t 
      -- f :: a -> b
      -- g :: t -> a
      -- t :: t

  -- ** this works! 
  -- (<$>) f fb = (\t -> f       (fb t))
  --               t is the supplied input from command line 
  --                  -> (a -> b)((t -> a))

  -- refactor this using (f g)(t) = f (g (t)) rule, effectively getting rid of lambda argument
  -- http://learnyouahaskell.com/higher-order-functions#composition
  (<$>) f fb = f . fb

-- | Anonymous map. Maps a constant value on a functor.
--
-- >>> 7 <$ [1,2,3]
-- [7,7,7]
--
-- prop> x <$ [a,b,c] == [x,x,x]
--
-- prop> x <$ Full q == Full x
(<$) ::
  Functor f =>
  a
  -> f b
  -> f a
(<$) =
  error "todo"

-- | Anonymous map producing unit value.
--
-- >>> void [1,2,3]
-- [(),(),()]
--
-- >>> void (Full 7)
-- Full ()
--
-- >>> void Empty
-- Empty
--
-- >>> void (+10) 5
-- ()
void ::
  Functor f =>
  f a
  -> f ()
void =
  error "todo"

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

-- | Maps a function on an IO program.
--
-- >>> reverse <$> (putStr "hi" P.>> P.return ("abc" :: List Char))
-- hi"cba"
instance Functor IO where
  (<$>) =
    P.fmap

instance Functor [] where
  (<$>) =
    P.fmap

instance Functor P.Maybe where
  (<$>) =
    P.fmap
