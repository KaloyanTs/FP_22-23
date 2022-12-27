{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use when" #-}
{-# HLINT ignore "Use bimap" #-}

import Datatypes
import Conversions
import Checkers
import Identities
import Unification
import Tools

consult :: String -> (Bool, [String])
consult contents = (truth, if truth then [] else filter (\x -> not (isFact x || isRule x || isComment x)) (extractData contents))
  where
    truth = all (\x -> isFact x || isRule x || isComment x) (extractData contents)

interpreteCode :: [String] -> Database
interpreteCode c = (rules, facts)
  where
    rules = map toRule (filter isRule c)
    facts = map toFact (filter isFact c)

interpreteInput :: String -> Database -> Bool
interpreteInput input (r, f)
  | isFact input = any (\x->not (null (toBeUnified (factToTerm x ,readyTerm)))) f
  | isRule input = error "not done yet"
  --todo must be finished; now telling only true or false
  | otherwise = not (null (toBeUnified (toEquality input)))
  where readyTerm = MakeTermAtom $ toAtom (init input)

check :: String -> Database -> IO ()
check input database = do
  if input == "quit"
    then return ()
    else
      if not (isFact input) && not (isRule input) && not (isEquality input)
        then do
          print "You are allowed to input only facts, queries and equalities!"
          userInteract database
        else do
          print $ if interpreteInput input database then "true." else "false."
          userInteract database

userInteract :: Database -> IO ()
userInteract database = do
  factInput <- getLine
  let fact = removeWhiteSpacesAfterComma factInput
  check fact database

workWithFile :: String -> IO ()
workWithFile path = do
  contents <- readFile ("prolog/" ++ path)
  let truth = consult contents
  print $ if fst truth then "true." else "false.\n" ++ unlines (snd truth)
  let realCode = [removeWhiteSpacesAfterComma x | x <- lines contents, (not . isComment) x, (not . null) x]
  let interpretedCode = interpreteCode realCode
  userInteract interpretedCode

loop :: IO ()
loop = do
  putStr "Which file to consult from the directory \"prolog/\"?\n> "
  file <- getLine
  workWithFile file
  putStrLn "Consult another file? ( y | [n] )"
  response <- getLine
  if (not . null) response && head response == 'y' then loop else return ()

main :: IO ()
main = do
  loop
  putStrLn "Closing..."
  response <- getLine
  return ()