-- PCode : TEST

main = interact (unlines . takeWhile (/= "42") . words)