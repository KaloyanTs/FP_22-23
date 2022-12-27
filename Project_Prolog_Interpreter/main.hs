{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use when" #-}
{-# HLINT ignore "Use bimap" #-}

import Checkers
import Conversions
import Datatypes
import Identities
import Tools
import Unification

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
showQRs [] = return ()
showQRs (x@(EndQR _) : xs) = do
  showQR x
  showQRs xs
showQRs (x : xs) = do
  showQR x
  response <- getLine
  showQRs xs

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

interpreteInput :: String -> Database -> [QueryResult]
interpreteInput input (r, f)
  | isFact input = reverse $ search f []
  | isRule input = error "not done yet"
  --todo must be finished; now telling only true or false
  | otherwise = [toBeUnified (toEquality input)]
  where
    readyTerm = MakeTermAtom $ toAtom (init input)
    search [] qrs = EndQR False : qrs
    search (fact : fs) qrs
      | good res = [res]
      | notBad res = search fs (res : qrs)
      | otherwise = search fs qrs
      where
        res = toBeUnified (factToTerm fact, readyTerm)
        good (EndQR True) = True
        good _ = False
        notBad (EndQR False) = False
        notBad _ = True

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