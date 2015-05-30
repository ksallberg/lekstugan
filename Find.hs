import Control.Monad (filterM, forM)
import Data.Time.Clock
import Data.Maybe (fromJust)
import System.FilePath.Posix
import System.Directory
import System.Time (ClockTime(..))
import System.FilePath (takeExtension)
import Control.Exception (bracket, handle, SomeException(..))
import System.IO (IOMode(..), hClose, hFileSize, openFile)

-- (a) 12p

type InfoP a =  FilePath    -- path to directory entry
             -> Permissions -- permissions
             -> Integer     -- file size
             -> UTCTime     -- last modified
             -> a

constP :: a -> InfoP a
constP x = (\fp per filesize time -> x)

liftPc :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftPc fun infa b = (\fp per filesize time ->
                        let a = (infa fp per filesize time)
                        in fun a b)

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 fun infa infb = (\fp per filesize time ->
                         let a = (infa fp per filesize time)
                             b = (infb fp per filesize time)
                         in fun a b)

liftPath :: (FilePath -> a) -> InfoP a
liftPath fpfun = (\fp _ _ _ ->
                     (fpfun fp))

(&&?) :: InfoP Bool -> InfoP Bool -> InfoP Bool
x &&? y = (\fp per filesize time ->
              (x fp per filesize time) && (y fp per filesize time))

(||?) :: InfoP Bool -> InfoP Bool -> InfoP Bool
x ||? y = (\fp per filesize time ->
              (x fp per filesize time) || (y fp per filesize time))

pathP :: InfoP FilePath
pathP = (\fp _ _ _ -> fp)

sizeP :: InfoP Integer
sizeP = (\_ _ filesize _ -> filesize)

(==?) :: (Eq a) => InfoP a -> a -> InfoP Bool
x ==? y = (\fp per filesize time -> (x fp per filesize time) == y)

(>?) :: (Ord a) => InfoP a -> a -> InfoP Bool
x >? y = (\fp per filesize time -> (x fp per filesize time) > y)

type Predicate = InfoP Bool

find :: FilePath -> Predicate -> IO [FilePath]
find path p = getRecursiveContents path >>= filterM check
    where check name = do perms    <- getPermissions name
                          size     <- getFileSize name
                          modified <- getModificationTime name
                          return (p name perms (fromJust size) modified)

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
    names <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", ".."]) names
    paths <- forM properNames $ \name -> do
      let path = topdir </> name
      isDirectory <- doesDirectoryExist path
      if isDirectory
        then getRecursiveContents path
        else return [path]
    return (concat paths)

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle handler
                   $ bracket (openFile path ReadMode)
                             (hClose)
                             (\h -> do size <- hFileSize h
                                       return $ Just size)
    where handler :: SomeException -> IO (Maybe Integer)
          handler _ = return Nothing

myTest :: InfoP Bool
myTest = (liftPath takeExtension ==? ".hs") &&? (sizeP >? 10000)

test :: IO [FilePath]
test = find "/home/kristian/Documents/haskell/Haskerl" myTest

-- (b) 8p

-- implement filterM and forM using only return, (>>=), liftM, foldr

-- type of first argument to foldr: a -> m [a] -> m [a]
-- example: filterM' (\a -> return  (a == 329)) [1,2,3,329,34,234,329]
filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' filtfun xs = foldr (\elem acc -> acc >>=
                                 \unacc -> filtfun elem >>=
                                     \unbool -> case unbool of
                                                  True -> return ([elem] ++ unacc)
                                                  False -> acc)
                            (return [])
                            xs
                      
-- type of first argument to foldr: a -> m [b] -> m [b]
-- example: forM' [1,2,3,4] (\a -> return (a+1))
forM' :: Monad m => [a] -> (a -> m b) -> m [b]
forM' xs liftf = foldr (\elem acc ->
                         acc >>=
                             \unacc -> liftf elem >>=
                                 \elemmod -> return ([elemmod] ++ unacc))
                       (return [])
                       xs
