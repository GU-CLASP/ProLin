{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}


module TransportationParser (parseTop) where

import Text.ParserCombinators.Parsek
import Data.Char
import Data.Function
import Data.Maybe
import HaskToProlin
import GHC.Generics

type P = Parser Char


integer :: P Int
integer = read <$> munch1 isDigit
-- integer = read <$> ((:) <$> digit <*> manyGreedy digit)

vehicle :: P String
vehicle = string "bus" <|> string "tram"

place :: P String
place =   string "science park"
      <|> string "central station"
      <|> string "airport"
      <|> string "university"

anything :: P ()
anything = skipMany anySymbol

thanks :: P ()
thanks = do
  anything
  _ <- string "thank"
  return ()

data Clarification = To String | From String | With Int deriving (Show,Generic)
instance Encode Clarification

clarification :: Parser Char Clarification
clarification =  (To <$> (string "to " >> place))
              <|> (From <$> (string "from " >> place))
              <|> (With <$> (vehicle >> munch1 (not . isDigit) >> integer))

instance Encode Request
data Request = Request {reqTo :: Maybe String
                       ,reqFrom :: Maybe String
                       ,reqWith :: Maybe Int} deriving (Show,Generic)

emptyRequest :: Request
emptyRequest = Request Nothing Nothing Nothing
addToRequest :: Clarification -> Request -> Request
addToRequest (To x) Request{..} = Request{reqTo = Just x,..}
addToRequest (From x) Request{..} = Request{reqFrom = Just x,..}
addToRequest (With x) Request{..} = Request{reqWith = Just x,..}

makeRequest :: [Clarification] -> Request
makeRequest = foldr addToRequest emptyRequest

request :: Parser Char Request
request = do
  anything
  _ <- string "when" <|> string "what"
  anything
  cs <- sepBy1 clarification anything
  return (makeRequest cs)

data Message = Thanks | Req Request | Clarify Clarification deriving (Show,Generic)
instance Encode Message

reqScore :: Request -> Int
reqScore (Request a b c) = length $ filter id $ [isJust a, isJust b, isJust c]

bestReq :: Request -> Request -> Request
bestReq x y = case (compare `on` reqScore) x y of
  LT -> y
  GT -> x
  _ -> error ("Cannot decide best request: " ++ show x ++ show y)

best :: Message -> Message -> Message
Thanks `best` x = x
Clarify _ `best` x = x
Req x `best` Req y = Req (x `bestReq` y) -- FIXME: HACK!
Req x `best` _ = Req x

bests :: [Message] -> Message
bests = foldr1 best

message :: Parser Char Message
message = (Req <$> request) <|>
          (pure Thanks <* thanks) <|>
          (Clarify <$> (anything *> clarification) <* anything)

parseTop :: [Char] -> ParseResult Char Message
parseTop msg = bests <$> parse message allResults (map toLower msg) 


-- >>> parseTest "thank you!!!"
-- Right Thanks

-- >>> parseTest "hum, to central station please"
-- Right (Clarify (To "central station"))


-- >>> parseTest "Hey! When is the next fucking bus 55 from Science park, you stupid machine?"
-- Right (Req (Request {reqTo = Nothing, reqFrom = Just "science park", reqWith = Just 55}))


errToMaybe :: Either a b -> Maybe b
errToMaybe (Left _) = Nothing
errToMaybe (Right a) = Just a

-- >>> encode (errToMaybe (parseTest "Hey! When is the next fucking bus 55 from Science park, you stupid machine?"))
-- App [Con "Just",App [Con "Req",App [Con "Request",App [Con "Nothing"],App [Con "Just",Con "science park"],App [Con "Just",Con "55"]]]]
