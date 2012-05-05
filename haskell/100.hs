--http://www.haskell.org/haskellwiki/99_questions
myLast :: [a] -> a
myLast (s:[]) = s
myLast (s:xs) = myLast xs

------------------------------------------
myButLast  :: [a] -> a
myButLast [a,b] = a
myButLast  (a:b:c) = myButLast  (b:c)

------------------------------------------
elementAt :: [a] -> Int -> a
elementAt (x:xs) 1 = x
elementAt [] _ = error "Out of bound"
elementAt (x:xs) a
  | a < 1 = error "Out of bound"
  | otherwise = elementAt xs (a-1)

------------------------------------------
myLength :: [a] -> Int
myLength x = myLength' x 0
myLength' [] a = a
myLength' (x:xs) a = myLength' xs $ a+1

myLength'' [] = 0
myLength'' (x:xs) = 1 + myLength'' xs

------------------------------------------
reverse'' :: [a] -> [a]
reverse'' []= []
reverse'' (x:xs) = (reverse'' xs) ++ [x]

------------------------------------------
isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == reverse'' x

------------------------------------------
data List' a = Elem a | List [List' a]
flatten :: List' a -> [a]
flatten (Elem a   )   = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List [])     = []

------------------------------------------
compress (x1:(x2:xs))
  | x1 == x2 = compress (x2:xs)
  | otherwise = x1 : compress (x2:xs)
compress x = x

------------------------------------------
pack (x:xs) = let (y,ys) = span (==x) xs
               in (x:y) : pack ys
pack [] = []

------------------------------------------
encode x = encode' $ pack x
encode' [] = []
encode' (x:xs) = (length x, head x) : encode' (xs) 

