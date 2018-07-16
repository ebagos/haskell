module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = do
    id <- getids "Language" languages
    if id == 9 then return () else do
        id2 <- getids "message" (messages !! (id - 1))
        if id2 == 9 then return () else do
            let mid = messages !! (id - 1)
            let mid2 = mid !! (id2 - 1)
            putStrLn mid2
            someFunc
                where
                    languages = ["日本語", "English", "Français", "Deutsch"]
                    messages = [ja, en, fr, gr]
                        where
                            ja = ["おはよう", "こんにちは", "こんばんは", "さようなら"]
                            en = ["Good moring", "Hello", "Good evening", "Good-by"]
                            fr = ["Bonjour", "Bonjour", "Bonsoir", "Au revoir"]
                            gr = ["Guten Morgen", "Guten Tag", "Guten Abend", "Auf Wiedersehen"]
                

menuitem :: Int -> [String] -> String
menuitem _ [] = ""
menuitem n (x:xs) = x ++ " = " ++ show(n) ++ ", " ++ menuitem (n + 1) xs

menu :: String -> [String] -> String
menu title disp = "select " ++ title ++ "\n" ++ menuitem 1 disp ++ "end = 9"

getids :: String -> [String] -> IO Int
getids title disp = do
    putStrLn $ menu title disp
    ids <- getLine
    let id = read ids :: Int
    if (id > 0) && (id <= length disp) 
        then return id
        else if id == 9 then return id else getids title disp

