module AnsiConsole
(
  cls,
  goto
)
where

----------------------------------------------------------------------
--- See: https://en.wikipedia.org/wiki/ANSI_escape_code
----------------------------------------------------------------------

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: (Int, Int) -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show x ++ ";" ++ show y ++ "H")
