{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeOperators    #-}
module Utils (
               buildDicts
             , compareDicts
             , replaceDupes
  )

where

import           Control.Monad         (forM_, forM)
import           Data.Bifunctor        (second)
import           Data.List             (intersect)
import           Data.Map.Strict       (Map, filterWithKey, keys)
import qualified Data.Map.Strict       as M (elems, insert)
import           Data.Set              (Set, fromList, notMember)
import qualified Data.Set              as S (elemAt, filter, map, null, size)
import           Effectful             (Eff, (:>))
import           Effectful.Unix        (FileID, FileOffset, Unix, fileID,
                                        fileSize, isDirectory, listDirectory, removeLink
                                       , createLink)
import           System.FilePath.Posix (isExtensionOf, (</>))

getAllPaths :: Unix :> es => FilePath -> [String] -> Eff es [FilePath]
getAllPaths fp ex = do
  isDir <- isDirectory fp
  if isDir then listDirectory fp >>= (concat <$>) . mapM (flip getAllPaths ex . (fp </>))
           else if any (`isExtensionOf` fp) ex then pure [fp]
            else pure []

getDetails :: Unix :> es => FilePath -> Eff es (FileID, FilePath, FileOffset)
getDetails fp = do
  iNode <- fileID fp
  fsize <- fileSize fp
  pure (iNode, fp, fsize)

buildDicts :: Unix :> es => FilePath -> [String] -> Eff es (Map FileID (FilePath, FileOffset))
buildDicts fp ex = do
  paths <- getAllPaths fp ex
  details <- forM paths getDetails
  pure . foldr (\(a, b, c) d -> M.insert a (b, c) d) mempty $ details

compareDicts :: Map FileID (FilePath, FileOffset) -> Map FileID (FilePath, FileOffset) -> (Set FileID, Set (FilePath, FilePath), Set (FilePath, Set FilePath), Set FilePath)
compareDicts fd td = (linked, dupes, uncertain, noMatch)
  where
    linked = fromList $ keys fd `intersect` keys td
    fu = fromList $ M.elems . filterWithKey (\k _ -> k `notMember` linked) $ fd
    tu = fromList $ M.elems . filterWithKey (\k _ -> k `notMember` linked) $ td
    matchedFiles = S.map (`matchBySize` fu) tu
    dupes = S.map (second (S.elemAt 0)) . S.filter ((== 1) . S.size . snd) $ matchedFiles
    uncertain = S.filter ((>1) . S.size . snd) matchedFiles
    noMatch = S.map fst . S.filter (S.null . snd) $ matchedFiles

matchBySize :: (FilePath, FileOffset) -> Set (FilePath, FileOffset) -> (FilePath, Set FilePath)
matchBySize (tfp, tfo) = (tfp, ) . S.map fst . S.filter ((== tfo) . snd)

replaceDupes :: Unix :> es => [(FilePath, FilePath)] -> Eff es ()
replaceDupes dupes = forM_ dupes replaceDupe

replaceDupe :: Unix :> es => (FilePath, FilePath) -> Eff es ()
replaceDupe (to, from) = do
  removeLink to
  createLink from to
