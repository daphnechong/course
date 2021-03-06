{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Optional where

import Course.Core
import qualified Prelude as P

--  class Optional<A> {
--    Optional(A a) {} // Full
--    Optional() {} // Empty
--  }
data Optional a = Full a | Empty deriving (Eq, Show)

mapOptional :: (a -> b) -> Optional a -> Optional b
mapOptional _ Empty    = Empty
mapOptional f (Full a) = Full (f a)

bindOptional :: (a -> Optional b) -> Optional a -> Optional b
bindOptional _ Empty    = Empty
bindOptional f (Full a) = f a

fbindOptional ::  Optional a -> (a -> Optional b) -> Optional b
fbindOptional = flip bindOptional

(??) :: Optional a -> a -> a
Empty ?? d  = d
Full a ?? _ = a

(<+>) :: Optional a -> Optional a -> Optional a
Empty <+> o = o
k <+> _     = k

twiceOptional :: (a -> b -> c) -> Optional a -> Optional b -> Optional c
twiceOptional f a b = bindOptional (\aa -> mapOptional (f aa) b) a

contains :: Eq a => a -> Optional a -> Bool
contains _ Empty = False
contains a (Full z) = a == z

instance P.Monad Optional where
  (>>=) =
    flip bindOptional
  return =
    Full
