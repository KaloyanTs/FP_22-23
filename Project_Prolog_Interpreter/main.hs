{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use when" #-}
{-# HLINT ignore "Use bimap" #-}

import Checkers
import Conversions
import Datatypes
import Identities
import Tools
import Unification
import Resolutions

consult :: String -> (Bool, [String])
consult contents = (truth, if truth then [] else filter (\x -> not (isFact x || isRule x || isComment x)) (extractData contents))
  where
    truth = all (\x -> isFact x || isRule x || isComment x) (extractData contents)

interpreteCode :: [String] -> Database
interpreteCode c = (rules, facts)
  where
    rules = map toRule (filter isRule c)
    facts = map toFact (filter isFact c)

showQRs :: [QueryResult] -> IO ()
showQRs [] = do
    print "false."
showQRs [qr] = do
    showQR qr
showQRs (x@(EndQR _):xs) = do
  showQR x
  showQRs xs
showQRs (x : xs) = do
  showQR x
  response <- getLine
  if (not.null) response
    then return ()
    else showQRs xs

showQR :: QueryResult -> IO ()
showQR (EndQR True) = do
  print "true."
showQR (EndQR False) = do
  print "false."
showQR (MakeQR (var, id) (EndQR _)) = do
  print $ showVariable var ++ " = " ++ showIdentifier id ++ "."
showQR (MakeQR (var, id) qr) = do
  print $ showVariable var ++ " = " ++ showIdentifier id ++ "."
  showQR qr

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
          showQRs $ interpreteInput input database
          userInteract database

userInteract :: Database -> IO ()
userInteract database = do
  putStr "> "
  factInput <- getLine
  let fact = removeWhiteSpacesAroundComma factInput
  check fact database

workWithFile :: String -> IO ()
workWithFile path = do
  contents <- readFile ("prolog/" ++ path)
  let truth = consult contents
  print $ if fst truth then "true." else "false.\n" ++ unlines (snd truth)
  let realCode =
        [ removeWhiteSpacesAroundComma x
          | x <- lines contents,
            (not . isComment) x,
            (not . null) x
        ]
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