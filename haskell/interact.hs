main = interact hello

hello :: String -> String
hello cs = unlines $ takeWhile (/= "hello, ") $ map ("hello, " ++) $ lines cs