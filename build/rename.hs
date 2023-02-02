-- renaming gwern.net pages while not breaking links is fairly difficult... This spits out a multi-step script to rename 'foo.page' to 'bar.page'
import System.Environment (getArgs)

main :: IO ()
main = do [arg1, arg2] <- getArgs
          foo arg1 arg2

foo :: String -> String -> IO ()
foo arg1 arg2 = putStrLn $ ("git mv '." ++ arg1 ++ ".page' " ++ "'." ++ arg2 ++ ".page' && ") ++
                ("gwsed ' " ++ arg1 ++ "' " ++ "' " ++ arg2 ++ "' && ") ++
                ("gwsed '](" ++ arg1 ++ "' " ++ "'](" ++ arg2 ++ "' && ") ++
                ("gwsed 'href=\"" ++ arg1 ++ "\"' " ++ "'href=\"" ++ arg2 ++ "\"' && ") ++
                ("gwsed 'href=\\\"" ++ arg1 ++ "\\\"' " ++ "'href=\\\"" ++ arg2 ++ "\\\"' && ") ++
                ("echo '\"~^" ++ arg1 ++ "$\" \"" ++ arg2 ++ "\";' >> ~/wiki/static/redirect/nginx.conf")
