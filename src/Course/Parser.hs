{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.Parser where

import Course.Core
import Course.Person
import Course.Functor
import Course.Apply
import Course.Applicative
import Course.Bind
import Course.Monad
import Course.List
import Course.Optional
import Data.Char

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Char(isUpper)

type Input = Chars

data ParseError =
  UnexpectedEof
  | ExpectedEof Input
  | UnexpectedChar Char
  | Failed
  deriving Eq


instance Show ParseError where
  show UnexpectedEof =
    "Unexpected end of stream"
  show (ExpectedEof i) =
    stringconcat ["Expected end of stream, but got >", show i, "<"]
  show (UnexpectedChar c) =
    stringconcat ["Unexpected character", show [c]]
  show Failed =
    "Parse failed"

data ParseResult a =
  ErrorResult ParseError
  | Result Input a
  deriving Eq

instance Show a => Show (ParseResult a) where
  show (ErrorResult e) =
    show e
  show (Result i a) =
    stringconcat ["Result >", hlist i, "< ", show a]

-- Function to determine is a parse result is an error.
isErrorResult ::
  ParseResult a
  -> Bool
isErrorResult (ErrorResult _) =
  True
isErrorResult (Result _ _) =
  False

data Parser a = P {
  parse :: Input -> ParseResult a
}

-- Function to produce a parser with the given result.
result ::
  ParseResult a
  -> Parser a
result =
  P . const

-- | Return a parser that always succeeds with the given value and consumes no input.
--
-- >>> parse (valueParser 3) "abc"
-- Result >abc< 3
valueParser ::
  a
  -> Parser a
valueParser x = P(\input -> Result input x)

-- | Return a parser that always fails with the given error.
--
-- >>> isErrorResult (parse failed "abc")
-- True
failed ::
  Parser a
failed = P (\_ -> ErrorResult Failed)

-- | Return a parser that succeeds with a character off the input or fails with an error if the input is empty.
--
-- >>> parse character "abc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse character "")
-- True
character :: Parser Char
character = P (\input -> 
  case input of
    Nil -> ErrorResult UnexpectedEof
    h:.t -> Result t h
  )

-- | Return a parser that puts its input into the given parser and
--
--   * if that parser succeeds with a value (a), put that value into the given function
--     then put in the remaining input in the resulting parser.
--
--   * if that parser fails with an error the returned parser fails with that error.
--
-- >>> parse (bindParser (\c -> if c == 'x' then character else valueParser 'v') character) "abc"
-- Result >bc< 'v'
--
-- >>> parse (bindParser (\c -> if c == 'x' then character else valueParser 'v') character) "a"
-- Result >< 'v'
--
-- >>> parse (bindParser (\c -> if c == 'x' then character else valueParser 'v') character) "xabc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse (bindParser (\c -> if c == 'x' then character else valueParser 'v') character) "")
-- True
--
-- >>> isErrorResult (parse (bindParser (\c -> if c == 'x' then character else valueParser 'v') character) "x")
-- True
bindParser ::
  (q -> Parser y)
  -> Parser q
  -> Parser y
bindParser f p =
  -- f :: q -> Parser y
  -- p :: Parser q (aka an (Input -> ParseResult a))
  -- parse p :: Input -> ParseResult q

  -- rest :: Input
  -- q :: q
  -- f q :: Parser y


  -- ??? :: ParseResult y
  P (\input -> case parse p input of 
            ErrorResult e -> ErrorResult e
            Result rest q -> parse (f q) rest)




  -- rest :: Input
  -- a :: a
  -- f a :: Parser b
  -- Parse


pairparser :: Parser (Char, Char)
pairparser =
  -- undefined :: char -> Parser (char, char)
  fbindParser character (\char1 -> 
    fbindParser character (\char2 -> 
      valueParser(char1, char2)))



fbindParser ::
  Parser a
  -> (a -> Parser b)
  -> Parser b
fbindParser =
  flip bindParser

-- | Return a parser that puts its input into the given parser and
--
--   * if that parser succeeds with a value (a), ignore that value
--     but put the remaining input into the second given parser.
--
--   * if that parser fails with an error the returned parser fails with that error.
--
-- /Tip:/ Use @bindParser@ or @fbindParser@.
--
-- >>> parse (character >>> valueParser 'v') "abc"
-- Result >bc< 'v'
--
-- >>> isErrorResult (parse (character >>> valueParser 'v') "")
-- True
(>>>) ::
  Parser a
  -> Parser b
  -> Parser b
(>>>) pa pb =
  fbindParser pa (\_ -> 
    fbindParser pb (\b -> 
      valueParser(b)))

-- | Return a parser that tries the first parser for a successful value.
--
--   * If the first parser succeeds then use this parser.
--
--   * If the first parser fails, try the second parser.
--
-- >>> parse (character ||| valueParser 'v') ""
-- Result >< 'v'
--
-- >>> parse (failed ||| valueParser 'v') ""
-- Result >< 'v'
--
-- >>> parse (character ||| valueParser 'v') "abc"
-- Result >bc< 'a'
--
-- >>> parse (failed ||| valueParser 'v') "abc"
-- Result >abc< 'v'
(|||) ::
  Parser a
  -> Parser a
  -> Parser a
(|||) pa pb =
  -- Input -> ParseResult a
  P (\input -> case parse pa input of 
      Result rest a   -> Result rest a
      ErrorResult _   -> parse pb input)


-- ** This works!
-- this is an alternative solution where you match the parameters, which extracts the 'parse' 
-- function out to variables pa and pb. 
-- if you don't match, then pa and pb equal P objects, which contain a 'parse' method (therefore you have to call it)
--(|||)' (P pa) (P pb) =
--  P (\input -> case pa input of 
--      Result rest a   -> Result rest a
--      ErrorResult _   -> pb input)



  --P (\input -> case parse p input of 
  --          ErrorResult e -> ErrorResult e
  --          Result rest q -> parse (f q) rest)

infixl 3 |||

-- | Return a parser that continues producing a list of values from the given parser.
--
-- /Tip:/ Use @many1@, @valueParser@ and @(|||)@.
--
-- >>> parse (list (character)) ""
-- Result >< ""
--
-- >>> parse (list (digit)) "123abc"
-- Result >abc< "123"
--
-- >>> parse (list digit) "abc"
-- Result >abc< ""
--
-- >>> parse (list (character)) "abc"
-- Result >< "abc"
--
-- >>> parse (list (character *> valueParser 'v')) "abc"
-- Result >< "vvv"
--
-- >>> parse (list (character *> valueParser 'v')) ""
-- Result >< ""
list ::
  Parser a
  -> Parser (List a)
-- valueParser :: a -> Parser a

list pa =  many1 pa ||| valueParser Nil



-- | Return a parser that produces at least one value from the given parser then
-- continues producing a list of values from the given parser (to ultimately produce a non-empty list).
-- The returned parser fails if The input is empty.
--
-- /Tip:/ Use @bindParser@, @list@ and @value@.
--
-- >>> parse (many1 (character)) "abc"
-- Result >< "abc"
--
-- >>> parse (many1 (character *> valueParser 'v')) "abc"
-- Result >< "vvv"
--
-- >>> isErrorResult (parse (many1 (character *> valueParser 'v')) "")
-- True
many1 ::
  Parser a
  -> Parser (List a)

-- A 1 or many parser is a parser that runs a parser
-- producing a value (call it a)
-- bi.THEN.ndPar.RUN.ser a 0 or many parser
-- producing a list (call it listofa)
-- bi.THEN.ndPar.RUN.ser cons a to a list of a


many1 pa = fbindParser pa (\h ->
  fbindParser (list pa) (\t ->
    valueParser(h :. t)))
--many1 _ = P(\_ -> ErrorResult UnexpectedEof)

-- | Return a parser that produces a character but fails if
--
--   * The input is empty.
--
--   * The character does not satisfy the given predicate.
--
-- /Tip:/ The @bindParser@ and @character@ functions will be helpful here.
--
-- >>> parse (satisfy isUpper) "Abc"
-- Result >bc< 'A'
--
-- >>> isErrorResult (parse (satisfy isUpper) "abc")
-- True
satisfy ::
  (Char -> Bool)
  -> Parser Char
satisfy f = fbindParser character (\c -> if f c then valueParser c else unexpectedCharP c)
    
    
unexpectedCharP :: Char -> Parser a
unexpectedCharP = result . ErrorResult . UnexpectedChar




-- | Return a parser that produces the given character but fails if
--
--   * The input is empty.
--
--   * The produced character is not equal to the given character.
--
-- /Tip:/ Use the @satisfy@ function.
is ::
  Char -> Parser Char
--is c = satisfy ( == c) 
is = satisfy . (==)

-- | Return a parser that produces a character between '0' and '9' but fails if
--
--   * The input is empty.
--
--   * The produced character is not a digit.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char.isDigit@ functions.
digit ::
  Parser Char
digit = satisfy isDigit

-- | Return a parser that produces zero or a positive integer but fails if
--
--   * The input is empty.
--
--   * The input does not produce a value series of digits
--
-- /Tip:/ Use the @bindParser@, @valueParser@, @list@, @read@ and @digit@
-- functions.

-- (list digit) returns a stream of digits from the input 
-- if you call fbindParser you end up with the stream of digits, 
-- you can then read the digits and evaluate the result (which is an optional, hence the case statement)
natural ::
  Parser Int
natural = 
  fbindParser (list digit) (\digits -> 
    case read digits of 
      Empty -> failed
      Full n -> valueParser n)


 

-- read "1234" :: Optional Int

--
-- | Return a parser that produces a space character but fails if
--
--   * The input is empty.
--
--   * The produced character is not a space.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char.isSpace@ functions.
space ::
  Parser Char
space = satisfy isSpace


-- | Return a parser that produces one or more space characters
-- (consuming until the first non-space) but fails if
--
--   * The input is empty.
--
--   * The first produced character is not a space.
--
-- /Tip:/ Use the @many1@ and @space@ functions.
spaces1 ::
  Parser Chars
spaces1 = many1 space

-- | Return a parser that produces a lower-case character but fails if
--
--   * The input is empty.
--
--   * The produced character is not lower-case.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char.isLower@ functions.
lower ::
  Parser Char
lower = satisfy isLower

-- | Return a parser that produces an upper-case character but fails if
--
--   * The input is empty.
--
--   * The produced character is not upper-case.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char.isUpper@ functions.
upper ::
  Parser Char
upper = satisfy isUpper

-- | Return a parser that produces an alpha character but fails if
--
--   * The input is empty.
--
--   * The produced character is not alpha.
--
-- /Tip:/ Use the @satisfy@ and @Data.Char.isAlpha@ functions.
alpha ::
  Parser Char
alpha = satisfy isAlpha

-- | Return a parser that sequences the given list of parsers by producing all their results
-- but fails on the first failing parser of the list.
--
-- /Tip:/ Use @bindParser@ and @value@.
-- /Tip:/ Optionally use @List#foldRight@. If not, an explicit recursive call.
--
-- >>> parse (sequenceParser (character :. is 'x' :. upper :. Nil)) "axCdef"
-- Result >def< "axC"
--
-- >>> isErrorResult (parse (sequenceParser (character :. is 'x' :. upper :. Nil)) "abCdef")
-- True

--fbindParser ::
--  Parser a
--  -> (a -> Parser b)
--  -> Parser b


sequenceParser ::
  List (Parser a)
  -> Parser (List a)
sequenceParser Nil = valueParser Nil


-- h :: Parser a
-- t :: List (Parser a)
-- sequenceParser t :: Parser (List a)
-- 
-- sequenceParser (h:.t) = undefined

sequenceParser (h:.t) = twiceParser (:.) h (sequenceParser t)


-- "this is not actually how you do twice"

twiceParser :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
twiceParser f pa pb = 
  fbindParser pa (\a ->
    fbindParser pb (\b -> 
      valueParser (f a b)))


twiceOptionalagain :: (a -> b -> c) -> Optional a -> Optional b -> Optional c
twiceOptionalagain f oa ob = 
  fbindOptional oa (\a ->
    fbindOptional ob (\b -> 
      Full (f a b)))


--sequenceParser = foldRight (\h  -> bindParser h ) valueParser(Nil)

-- | Return a parser that produces the given number of values off the given parser.
-- This parser fails if the given parser fails in the attempt to produce the given number of values.
--
-- /Tip:/ Use @sequenceParser@ and @List.replicate@.
--
-- >>> parse (thisMany 4 upper) "ABCDef"
-- Result >ef< "ABCD"
--
-- >>> isErrorResult (parse (thisMany 4 upper) "ABcDef")
-- True
thisMany ::
  Int
  -> Parser a
  -> Parser (List a)

thisMany num pa = sequenceParser (replicate num pa)

-- thisMany num pa = (sequenceParser . replicate num) p 

-- can also be refactored as
-- thisMany num = sequenceParser . replicate num

-- you can also even refactor futher as (but this is too far)
-- thisMany = (sequenceParser .) . replicate



-- | Write a parser for Person.age.
--
-- /Age: positive integer/
--
-- /Tip:/ Equivalent to @natural@.
--
-- >>> parse ageParser "120"
-- Result >< 120
--
-- >>> isErrorResult (parse ageParser "abc")
-- True
--
-- >>> isErrorResult (parse ageParser "-120")
-- True
ageParser ::
  Parser Int
ageParser = natural

-- | Write a parser for Person.firstName.
-- /First Name: non-empty string that starts with a capital letter/
--
-- /Tip:/ Use @bindParser@, @valueParser@, @upper@, @list@ and @lower@.
--
-- >>> parse firstNameParser "Abc"
-- Result >< "Abc"
--
-- >>> isErrorResult (parse firstNameParser "abc")
-- True
firstNameParser ::
  Parser Chars
firstNameParser =  twiceParser (:.) upper (list lower)

--(sequenceParser (upper :. lower :. Nil))
 

-- replicate (length ("abc" :: List Char)) (parse (list (lower)) "abc") 


--sequenceParser ( upper :.  lower) :. Nil)

-- find the length of the string
-- that satisfies no space
-- replicate the number of lowercase parsers (-1)
-- add to a list
-- sequence it



-- | Write a parser for Person.surname.
--
-- /Surname: string that starts with a capital letter and is followed by 5 or more lower-case letters./
--
-- /Tip:/ Use @bindParser@, @valueParser@, @upper@, @thisMany@, @lower@ and @list@.
--
-- >>> parse surnameParser "Abcdef"
-- Result >< "Abcdef"
--
-- >>> isErrorResult (parse surnameParser "Abc")
-- True
--
-- >>> isErrorResult (parse surnameParser "abc")
-- True
surnameParser ::
  Parser Chars
surnameParser = twiceParser (++) (twiceParser (:.) upper (thisMany 5 lower)) (list lower)

-- could you potentially twiceParser the twiceParser???

thriceParser :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
thriceParser = error "todo"

--surnameParser ::
--  Parser Chars
--surnameParser =
--    fbindParser upper (\u ->
--    fbindParser (thisMany 5 lower) (\r ->
--    fbindParser (list lower) (\s ->
--    valueParser (u :. r ++ s))))

-- | Write a parser for Person.smoker.
--
-- /Smoker: character that must be @'y'@ or @'n'@/
--
-- /Tip:/ Use @is@ and @(|||)@./
--
-- >>> parse smokerParser "yabc"
-- Result >abc< 'y'
--
-- >>> parse smokerParser "nabc"
-- Result >abc< 'n'
--
-- >>> isErrorResult (parse smokerParser "abc")
-- True
smokerParser ::
  Parser Char
smokerParser = is 'y' ||| is 'n'

-- | Write part of a parser for Person.phoneBody.
-- This parser will only produce a string of digits, dots or hyphens.
-- It will ignore the overall requirement of a phone number to
-- start with a digit and end with a hash (#).
--
-- /Phone: string of digits, dots or hyphens .../
--
-- /Tip:/ Use @list@, @digit@, @(|||)@ and @is@.
--
-- >>> parse phoneBodyParser "123-456"
-- Result >< "123-456"
--
-- >>> parse phoneBodyParser "123-4a56"
-- Result >a56< "123-4"
--
-- >>> parse phoneBodyParser "a123-456"
-- Result >a123-456< ""
phoneBodyParser ::
  Parser Chars
phoneBodyParser = list(digit ||| is '.' ||| is '-')

-- | Write a parser for Person.phone.
--
-- /Phone: ... but must start with a digit and end with a hash (#)./
--
-- /Tip:/ Use @bindParser@, @valueParser@, @digit@, @phoneBodyParser@ and @is@.
--
-- >>> parse phoneParser "123-456#"
-- Result >< "123-456"
--
-- >>> parse phoneParser "123-456#abc"
-- Result >abc< "123-456"
--
-- >>> isErrorResult (parse phoneParser "123-456")
-- True
--
-- >>> isErrorResult (parse phoneParser "a123-456")
-- True
phoneParser ::
  Parser Chars
--phoneParser = twiceParser (:.) digit phoneBodyParser

--phoneParser = 
--  fbindParser digit (\d ->
--    fbindParser phoneBodyParser (\b ->
--      fbindParser (is '#') (\_ ->
--        valueParser (d:.b))) )

-- you can make use of the >>> operator which discards the effect of input a and returns input b instead.
phoneParser = 
  fbindParser digit (\d ->
    fbindParser phoneBodyParser (\b ->
      is '#' >>> valueParser (d:.b))) 

-- parse (sequenceParser (replicate 5 (digit ||| is '.' ||| is '-'))) "23adfasdf"
-- Unexpected character"a"

-- you can pattern match the input?
-- d <- digit
-- b <- phoneBodyParser
-- _ <- is #
-- value (d:.b)




-- this works!
-- parse (list(digit ||| is '.' ||| is '-')) "23adfaf"


-- | Write a parser for Person.
--
-- /Tip:/ Use @bindParser@,
--            @valueParser@,
--            @(>>>)@,
--            @ageParser@,
--            @firstNameParser@,
--            @surnameParser@,
--            @smokerParser@,
--            @phoneParser@.
--
-- >>> isErrorResult (parse personParser "")
-- True
--
-- >>> isErrorResult (parse personParser "12x Fred Clarkson y 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 fred Clarkson y 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Cla y 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred clarkson y 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Clarkson x 123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Clarkson y 1x3-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Clarkson y -123-456.789#")
-- True
--
-- >>> isErrorResult (parse personParser "123 Fred Clarkson y 123-456.789")
-- True
--
-- >>> parse personParser "123 Fred Clarkson y 123-456.789#"
-- Result >< Person {age = 123, firstName = "Fred", surname = "Clarkson", smoker = 'y', phone = "123-456.789"}
--
-- >>> parse personParser "123 Fred Clarkson y 123-456.789# rest"
-- Result > rest< Person {age = 123, firstName = "Fred", surname = "Clarkson", smoker = 'y', phone = "123-456.789"}
personParser ::
  Parser Person
personParser = 
  --fbindParser ageParser (\pAge ->
  --  fbindParser spaces1 (\_ ->
  --    fbindParser firstNameParser (\pFname -> 
  --      fbindParser spaces1 (\_ ->
  --        fbindParser surnameParser (\pSname -> 
  --          fbindParser spaces1 (\_ ->
  --            fbindParser smokerParser (\pSmoker -> 
  --              fbindParser spaces1 (\_ ->
  --                fbindParser phoneParser (\pPhone -> 
  --                  valueParser(Person pAge pFname pSname pSmoker pPhone)))))))))) 

  do 
    pAge <- ageParser
    _ <- spaces1 
    pFname <- firstNameParser
    _ <- spaces1 
    pSname <- surnameParser
    _ <- spaces1 
    pSmoker <- smokerParser
    _ <- spaces1
    pPhone <- phoneParser
    valueParser(Person pAge pFname pSname pSmoker pPhone)

--personParser = undefined

-- Make sure all the tests pass!


-- | Write a Functor instance for a @Parser@.
-- /Tip:/ Use @bindParser@ and @valueParser@.
instance Functor Parser where
  -- (<$>) :: (a -> b) -> Parser a -> Parser b
  (<$>) f pa = do
    a <- pa
    valueParser (f a)
  -- (<$>) :: (a -> b) -> Parser a -> Parser b


-- | Write a Apply instance for a @Parser@.
-- /Tip:/ Use @bindParser@ and @valueParser@.
instance Apply Parser where
  -- (<$>) ::        (a -> b) -> Parser a -> Parser b
  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  -- (=<<) :: (a -> Parser b) -> Parser a -> Parser b
  (<*>) pf pa =
    do f <- pf
       a <- pa
       valueParser (f a)
    

-- | Write an Applicative functor instance for a @Parser@.
instance Applicative Parser where
  pure =
    error "todo"

-- | Write a Bind instance for a @Parser@.
instance Bind Parser where
  (=<<) =
    bindParser

instance Monad Parser where
