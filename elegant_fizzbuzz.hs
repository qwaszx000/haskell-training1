module ElegantFizzbuzz where

-- https://themonadreader.wordpress.com/wp-content/uploads/2014/04/fizzbuzz.pdf

fizzbuzz :: Int -> String
fizzbuzz n = (test 3 "fizz" . test 5 "buzz") id $ show n
  where
    test :: Int -> String -> (String -> String) -> (String -> String)
    test m str cont
        | n `mod` m == 0 = const $ str ++ cont ""
        | otherwise = cont

main :: IO ()
main = do
    let inputs = [0 .. 20]
    mapM_ (putStrLn . fizzbuzz) inputs
