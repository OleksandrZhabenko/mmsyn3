module EndOfExe where

import qualified System.Directory as D (findExecutable)
import Data.Maybe (isJust)
import System.IO.Unsafe (unsafePerformIO)

-- | Function that is used instead of System.Info.os to check whether the executable ends in .exe
endOfExecutable :: String -> IO String                              
endOfExecutable ys = do
                       xs <- D.findExecutable ys
                       if isJust xs 
                         then return ""
                         else do
                                zs <- D.findExecutable (ys ++ ".exe")
                                if isJust zs
                                  then return ".exe"
                                  else error ("Please, install the executable " ++ ys ++ " into the directory in the PATH variable!")
                                  
-- | Function that is used to concat the endOfExecutable to the name of the program. You can use showE \"nameOfExecutable\" e. g. inside the System.Process.callCommand as the name of the executable
showE :: String -> String
showE xs | null xs = []
         | otherwise = xs ++ unsafePerformIO (endOfExecutable xs)
