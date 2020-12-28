-- |
-- Module      :  EndOfExe
-- Copyright   :  (c) OleksandrZhabenko 2019-2020
-- License     :  MIT
-- Maintainer  :  olexandr543@yahoo.com
--
-- A small library to deal with executable endings. Uses a Maybe data representation inside an IO monad.

module EndOfExe where

import qualified System.Directory as D (findExecutable)
import Data.Maybe (isJust,isNothing)
import System.IO.Unsafe (unsafePerformIO)

-- | Can be used instead of 'System.Info.os' to check whether the executable ends in \".exe\". The function returns 'IO' 'Nothing' if there is neither 
-- @ys@ nor @(ys ++ ".exe")@ names for executables in the variable @PATH@.
endOfExecutable :: String -> IO (Maybe String)
endOfExecutable ys = do
  xs <- D.findExecutable ys
  if isJust xs 
    then return $ fmap (ys ++) (Just "")
    else do
      zs <- D.findExecutable (ys ++ ".exe")
      if isJust zs
        then return $ fmap (ys ++) (Just ".exe")
        else error ("Please, install the executable " ++ ys ++ " into the directory in the PATH variable!")
                                  
-- | Gets the proper name of the executable in the system (it must be seen in the directories in the @PATH@ variable). 
-- You can use 'showE' \"nameOfExecutable\" to get 'Just' \"nameOfExecutable\"@ if it is present on the system. Further you can adopt it to be used 
-- inside the 'System.Process.callCommand' as the name of the executable
showE :: String -> Maybe String
showE xs 
  | null xs = error "No executable specified!"
  | otherwise = unsafePerformIO . endOfExecutable $ xs

-- | Being given a list of names of executables (without an \".exe\" suffix) looks up for them in the specified list order 
-- till the first existing occurrence. If there is no such occurrence (the specified executables are not installed in the directories 
-- mentioned in the variable @PATH@) then the function returns 'Nothing'.
findSysExes :: [String] -> Maybe String
findSysExes xss 
  | all (isNothing . showE) xss = Nothing
  | otherwise = Just (head . dropWhile (isNothing . showE) $ xss)
