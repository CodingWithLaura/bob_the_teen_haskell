--  in  -> String -> f(??????) -> Sting "Meh"
-- f(String bla) =  [a(bla),b(bla),c(bla),d(bla)]  -> [true,false,false,true]
                                   --              -> ["calm","sure","whoa","fin"]
                                  --     curry_zip -> [(true,"calm"),(false,"sure"),...]
--             ["calm,fine"]
--erstma mappen
-- dann folden : )
import Data.List
import Data.Char

--isAnyUpper list = foldl (isUpper  "" list
--regel_a :: [Char] -> Bool
--regel_a test = ( isUpper test ) && (isSuffixOf ['?'] test )                       
regel_a sentence = isSuffixOf ['!'] sentence
regel_b sentence = isSuffixOf ['?'] sentence  
--regel_c sentence = isUpper sentence
regel_c sentence = isSuffixOf ['.'] sentence
regel_d sentence = (sentence == "")

regelwerk = [regel_a, regel_b, regel_c, regel_d]


response_a = "Calm down, I know what I'm doing!"
response_b = "Sure."
response_c = "Whoa, chill out!"
response_d = "Fine. Be that way!"

responses_list = [response_a,response_b,response_c,response_d]

curry_zip :: [a] -> [b] -> [(a,b)]
curry_zip (x:[]) (y:[]) = [(x,y)]
curry_zip (x:xs) (y:ys) = (x,y) : (curry_zip xs ys)

eignes_map :: a -> [(a->b)] -> [b]
eignes_map s [] = []
eignes_map s (fx:fxs) = (fx s): (eignes_map s fxs)

-- brauche funktion wie add
-- true "calm" , False ->

react :: String -> (Bool,String) -> String
react _   (True ,new) = new
react old (False,new) = old

bob_response sentence = foldl react "whatever" (curry_zip (eignes_map sentence regelwerk) responses_list) 

