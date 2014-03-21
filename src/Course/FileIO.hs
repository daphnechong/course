{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Apply
import Course.Bind
import Course.Functor
import Course.List

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: Chars -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure


Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

$ runhaskell io.hs "files.txt"
============ a.txt
the contents of a

============ b.txt
the contents of b

============ c.txt
the contents of c

-}

-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
main = 
  do 
     args <- getArgs
     void (sequenceIO (map run args))

type FilePath = Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run ::
  Chars
  -> IO ()
run filepath =
  do 
    content <- readFile filepath
    files <- getFiles (lines content)
    printFiles files

getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars)) -- return type will be List(FilePath, Chars) and IO is just signalling to the compiler that there are side effects and so you need to keep those in mind when reshuffling arguments
getFiles files = sequenceIO (getFile <$> files)
-- my attempt:
-- foldRight (\fp _ -> getFile fp) (void()) files

-- but you can define it using fmap, i.e. map a funtion on to each item in the list


getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile path = readFile path >>= \contents -> pure (path, contents)

printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles files = void (foldRight (\a r -> uncurry printFile a >>= \_ -> r) (pure Nil) files)
  -- void (foldRight (\a r -> uncurry printFile a >>= \_ -> r) (pure Nil) files)

-- officially:
-- void (sequenceIO (map (\(path, ct) -> printFile path ct)x))

-- but to refactor, examine the signature of the innermost function
-- (a -> b -> c) -> ((a,b) -> c)
-- you can check hoogle (haskell.org/hoogle) and put in the signature above into the search, 
-- it will return you something that matches the signature... uncurry
-- void (sequenceIO (map (uncurry printFile) x))

-- and then use function composition
-- void. sequenceIO . map. uncurry printFile

printFile ::
  FilePath
  -> Chars
  -> IO ()

-- path :: FilePath
-- contents :: Chars
-- Chars -> IO()
printFile path contents =  
  putStrLn path  >>= \_ -> putStrLn contents

-- putStrLn ("====== " ++ path ++ "\n" ++ contents)

-- you can also write as do
  --do 
  --  putStrLn path 
  --  putStrLn contents


sequenceIO :: List (IO a) -> IO (List a)
-- sequenceIO using foldRight
--sequenceIO = foldRight (twiceIO (:.)) (pure Nil)
 
 -- sequenceIO using pattern matching
sequenceIO Nil = pure Nil
sequenceIO (h:.t) = 
  h >>= \a -> 
  sequenceIO t >>= \r -> 
  pure (a:.r)


-- this is the definition of lift2 but for IO

twiceIO :: (a -> b -> c) -> IO a -> IO b -> IO c
twiceIO f a b = 
  a >>= \aa -> b >>= \bb -> pure (f aa bb)

-- you can rewrite the (Bind f, Applicative f) as Monad f
-- the only reason why we can't is that we don't have monad defined in this set of files... :)
-- in reality the signature should be Monad f => (a -> b -> c) -> f a -> f b -> f c
-- and this says, anything of type f needs to be a monad, i.e. have Bind and Applicative functions
mylift2 :: (Bind f, Applicative f) => (a -> b -> c) -> f a -> f b -> f c
mylift2 f a b = 
  a >>= \aa -> b >>= \bb -> pure (f aa bb)

--do 
--  aa <- a
--  bb <- b
--  pure (f aa bb)

-- I have no idea what this function does :/
listAnything :: Bind f => f a -> f (List a) -> f (List a)
listAnything h t = 
  (:.) <$> h <*> t

-- mylift2 (:.) h t

-- Course for learning Haskell 
-- http://www.seas.upenn.edu/~cis194/lectures.html

-- nicta fp google group
-- lambda ladies google group

-- Tic Tac Toe haskell

-- to get better on applicative finish file Functor, then Apply, then Applicative
