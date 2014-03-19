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
  error "todo"

type FilePath =
  Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run ::
  Chars
  -> IO ()
run =
  error "todo"

getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars)) -- return type will be List(FilePath, Chars) and IO is just signalling to the compiler that there are side effects and so you need to keep those in mind when reshuffling arguments
getFiles files = undefined
 -- foldRight (\fp _ -> getFile fp) (void()) files

getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile path = readFile path >>= \contents -> pure (path, contents)

printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles files = 
  foldRight (\(fp, contents) _ -> printFile fp contents) (pure()) files

printFile ::
  FilePath
  -> Chars
  -> IO ()

-- path :: FilePath
-- contents :: Chars
-- Chars -> IO()
printFile path contents =  
  putStrLn path  >>= \_ -> putStrLn contents

--sequenceIO :: List (IO a) -> IO (List a)
--sequenceIO (h:.t) = twiceAnything (pure h) (sequenceIO t)

twiceAnything :: Bind f => f a -> f (List a) -> f (List a)
twiceAnything h t = 
  (:.) <$> h <*> t

-- case listy of (x,y):.t -> x
