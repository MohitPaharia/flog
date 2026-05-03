module Client.Tui.Util where

import           Type.General

safeInit :: [a] -> [a]
safeInit [] = []
safeInit xs = init xs

safeHead :: [String] -> String
safeHead [] = ""
safeHead xs = head xs

normalize :: (a -> a) -> Zipper a -> Zipper a
normalize f z@(Zipper _ x _) = replace (f x) $ rightEnd z

