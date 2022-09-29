
------ Part 1 ----
{-

1a)
Sigma is an application designed by Facebook written in Haskell which detects and removes spam and malicious actions on their popular social media site.  This was put into 
place as Haskell was faster in areas as it can abstract information that others are not able to. Sigma can respond quickly to attempts to release spam, malware and other 
negative content so that it does not end up on the user’s feed by applying functions to check for malicious content. These checks are incredibly quick and happen between the 
time that the information is sent from the user and to the time at which it is posted/delivered.

The Glasgow Haskell compiler is a program written in Haskell that translates Haskell code to C, assembly code and other formats for the code to be used. It is the most common
complier for Haskell and allows programs to run quickly as a result of being written in Haskell. The program compiles the entire program before running it meaning that each 
time that the program is run it will check for errors and if saved after compiled it will execute quickly  as it does not need to be translated into assembly code again.

Cardano uses Haskell as a basis for Plutus which runs on the blockchain for Cardano when certain criteria is met. Plutus allows for conversion of crypto/digital currencies 
into great British pounds, Euros and other currencies. It is important for transitions of currencies to be done quickly because in the matter of seconds the exchange rate 
between them can change significantly which is why haskell is one of the best languages for Plutus to run on due to its speed.

Sources:
https://serokell.io/blog/functional-programming-in-fintech
https://serokell.io/blog/top-software-written-in-haskell
https://engineering.fb.com/2015/06/26/security/fighting-spam-with-haskell/
https://medium.com/@cardano.foundation/why-cardano-chose-haskell-and-why-you-should-care-why-cardano-chose-haskell-and-why-you-should-f97052db2951

1b)
-	Functional code tends to be self-isolated between functions meaning that it is easier to identify where problems are in the code and multiple people can program multiple 
	different functions simultaneously as they will understand what their inputs and outputs should be.

-	It is easier to test as each function simply maps a set of inputs to a set of outputs meaning that each module is easier to test if you understand what the domain and 
	range of outputs should be for the function

-	Functional programs such as Haskell or SML are High level programming languages meaning that they are close to human language in their commands which makes the program 
	easier to read, understand or write different functions

-	It is beneficial for programmers as it reduces a large complex problem into a number of sub problems which can each be coded as individual functions which will perform the 
	specified task. When a problem is broken down like so, it becomes easier to program and more robust as each function can be tested easily.


Sources:
https://alvinalexander.com/scala/fp-book/benefits-of-functional-programming/#debugging-is-easier

1c)
A mathematical function is one that manipulates values (numbers or algebra) to another set of numbers or algebra. Eg: (below this paragraph). The mathematical function below will square 
any number that is inputted into the function or will square any algebra inputted. Haskell functions are also able to replicate functions done in math however they operate in a 
different way for different inputs. Haskell functions are able to take values other than floats or integers and manipulate them to produce any range of outputs from Strings to 
lists to user defined data types such as the “Dog” type taken from Part 2 exercise a and b (and written below this paragraph). From this it can create a list of data from strings and 
integers.

Mathematical function:
f(x) = x^2 

Haskell Function:
type Dog =(String,Int)
create_dog_list :: [String] -> [Int] -> [Dog] 
create_dog_list xs ys = zip xs ys

-}

------ Part 2 ----

------ Part 2a ----
type Dog =(String,Int)
--Creates a tuple of strings and integers ("Poodle",111)

------ Part 2b ----
create_dog_list :: [String] -> [Int] -> [Dog] 
create_dog_list xs ys = zip xs ys
--Matches an inputted list of strings to an inputted list of integers to form the tuple list [Dog]

------ Part 2c ----
merge ::[Dog] -> [Dog] -> [Dog]
merge [] ys = ys
merge xs [] = xs
merge ((x,y):xs) ((a,b):ys) 
	|y < b = (x,y) : merge xs ((a,b):ys)
	|otherwise =  (a,b): merge ((x,y):xs) ys 
{-Compares Integers in the heads of each tuple list to see which one is larger than the other and places them infront of the other depending on wether or not the value held in the tuple is
 larger or less than the value of the integer in the second-}

sort_dog_list ::[Dog] -> [Dog]
sort_dog_list [] = []
sort_dog_list [(x,y)] = [(x,y)]
sort_dog_list xs = merge (sort_dog_list ys) (sort_dog_list zs)
 where
 h = (length xs)`div` 2
 ys = take h xs
 zs = drop h xs
{-Recusively splits up a list into a number of sub lists until each list is composed of one or no elements and then uses the merge function to combine them until all lists are merged into 
1 list-}

------ Part 2d ----
remove_smallest_dogs :: Int -> [Dog] -> [Dog]
remove_smallest_dogs k ys = drop k (sort_dog_list (ys))
--Sorts the list of Dog using function in 2c then removes the first k elements

------ Part 2e ----
remove_tall_dogs :: [Dog] ->[Dog]
remove_tall_dogs xs = [(x,y)|(x,y)<-xs,(80>y)]
--List comprehension to remove all the values in a list of dogs that are larger than 80cm


------ Part 3a ----


theStars:: Int -> String
theStars x
 |x == 0 = ""
 |otherwise = "*"++theStars(x-1)
--Creates a string composed of x number of stars

thelinesofstar:: Int -> Int  -> Int ->String
thelinesofstar x y z
 |x == 0 = ""
 |otherwise =theSpaces (z) ++ theStars(y) ++ "\n" ++ thelinesofstar (x-1) y z
--Creates a string with z number of spaces and y number of stars and indents to create x number of identical additional lines

theSpaces:: Int -> String
theSpaces x
 |x == 0 = ""
 |otherwise = " "++theSpaces(x-1)
--Creates a string composed of x number of spaces

steps1::Int->Int->Int->String
steps1 x y 0 = []
steps1 x y z = thelinesofstar (x) (y*z) (z*(y)) ++ steps1 x y (z-1)
{-Function which recursively calls itself and thelinesofstar to coordinate for the steps to go up, as the amount of spaces in each line decreases in a different way to the 
decrease of stars in which is managed by the z variable decreasing-}

steps2::Int->Int->Int->Int ->String
steps2 w x y z
 |y==z = []
 |otherwise = steps2 w x (y-1) (z)++thelinesofstar (w) (x*y) (y*(x))
{-Function which recursively calls itself for the steps to go up, as the amount of spaces in each line increases in a different way to the increase of stars in each line each 
time it goes down. The additional variable w is always needs to be zero when called as it counts up to the number of steps (z) that the code needs to make-}


steps::Int->Int->Int->String
steps x y z = steps2 x y z 0 ++ steps1 x y z 
{-Final Function which combines Steps2 which counts up to z from 0 and Steps1 which counts down from z to 0 to create steps going up and down as the final function and 
intended pattern of output based on inputs-}

------ Part 3b ----


thestars:: Int -> String
thestars x
 |x == 0 = ""
 |otherwise = "*"++thestars(x-1)
--Creates a string of length x of stars at the start and end of each flagmade

thelines:: Int-> (Int,Int)-> String
thelines x (y,z)= "\n"++"*"++(linescontent (spaces(x-2)) (y,z))++"*" 
{-makes sure every line called starts and ends with a star and calls linescontent and spaces in order to create the text betweeen them. Calls spaces as x-2 as the lines content plus the 
two characters at the start and at the end total to an x wide line. In total it creates a string which starts a new line from the line before each time it is called -}

thelinesrecursively:: Int -> [(Int,Int)] -> String
thelinesrecursively x [] = ""
thelinesrecursively x (y:ys) = thelines x y ++ thelinesrecursively x ys
{-takes the integer from when the main function is called and recursively calls the lines to be made by the function thelines until the all the lines have the diagonal pattern made 
from the tuples in the list created by the pluses function-}

spaces:: Int -> String
spaces 0 =""
spaces x = " "++spaces (x-1)
--creates a string of length x purely consisting of spaces

pluses:: Int -> [(Int,Int)]
pluses x = zip [1..(x-2)] [(x-2),(x-3)..1]
{-A formula which creates a tuple of two elements that create the pattern of the diagonals assigning where in each line the pluses should go for example (1,(x-2)) will tell the 
linescontent function that the program needs to add spaces at the first and last element in the string of spaces and will continue to create the list of where the pluses should 
go ending with ((x-2),1) as the final spaces-}

addplustospaces:: String -> Int -> String
addplustospaces xs y = (take (y-1) xs) ++recursiveformula xs (y)
--Takes the string of spaces and adds a plus into the string at that position using the recursiveformula function

recursiveformula :: String -> Int -> String
recursiveformula xs 0 = "+"++xs
recursiveformula (x:xs) y = recursiveformula xs (y-1)
--calls itself until it creates a string with + at the position that it has chosen

linescontent :: String -> (Int,Int) -> String
linescontent x (y,z) = addplustospaces (addplustospaces x y) z
--makes sure that the plus is added for the first and second element in a tuple when the function is called

flagpattern1:: Int->Int->String
flagpattern1 x 0 = ""
flagpattern1 x y = thestars x ++ thelinesrecursively x (pluses x)++"\n"++thestars x ++"\n" ++ flagpattern x (y-1)
{-recursively creates the flag pattern until y=0. It Starts and ends with lines of stars and adding the lines inbetween through the function thelinesrecursively and indents between lines 
in the process-}

flagpattern::Int->Int->String
flagpattern n m
	|n<5 = ""
	|m<1 = ""
	|otherwise = flagpattern1 n m
--ensures that the function only returns the pattern for the correct inputs

------ Part 4 ----


crossouttheletters :: String -> Char -> String
crossouttheletters [] y = ""
crossouttheletters (x:xs) y
	|x == y = "*" ++ crossouttheletters xs y
	|otherwise = [x] ++ crossouttheletters xs y
--Function that takes a char and then loops through a string replacing all occurences of itself with a star

recursivelycallstar :: String -> String -> String
recursivelycallstar xs [] = xs
recursivelycallstar xs (y:ys) =  crossouttheletters (recursivelycallstar xs ys) y
--Function that calls crossouttheletters recursively until every char in the string is checked against the other string

getridofstar :: String -> String
getridofstar xs = [x |x<-xs,(x/='*')]
--Gets rid of the star so that each of the non star values can be mapped to the l,a,h,i sequence

zippingvalues :: Int -> [Int]
zippingvalues x = [i`mod`4 |i<-[1..x]]
--Function that creates a repeating list of [0,1,2,3,0,1,2...] which will be the length of the string

zippingvalues2 :: [Int] -> String
zippingvalues2 [] = ""
zippingvalues2 (x:xs)
	|x == 1 = "l" ++ zippingvalues2 xs
	|x == 2 = "a" ++ zippingvalues2 xs
	|x == 3 = "h" ++ zippingvalues2 xs
	|x == 0 = "i" ++ zippingvalues2 xs
--replaces the list created in zippingvalues function with a list of chars with the repeating pattern lahi instead of the repeating pattern of 0,1,2,3

matchingletterstovalues:: String -> String -> [(Char,Char)]
matchingletterstovalues xs ys = zip (zippingvalues2 (zippingvalues (length(xs)))) (getridofstar (recursivelycallstar  xs ys))
{-maps the letters that are not shared between the two inputted strings and outputs a list of tuples of each char in the lahi sequence to each char from teh recursively call star function 
that is not a star-}

compatibility :: String->String->String
compatibility xs ys = xs ++ a ++ ys++ " and "++ ys ++ b ++xs
	where
	a = matchletterstostrings (last (map (fst) (matchingletterstovalues xs ys)))
	b = matchletterstostrings (last (map (fst) (matchingletterstovalues ys xs)))
--takes the first element of the tuple in the assignment of the last character and creates an output string as a result of the function for each inputted strings

matchletterstostrings:: Char -> String
matchletterstostrings x
	|x =='l' = " likes "
	|x =='a' = " admires "
	|x =='h' = " hates "
	|x =='i' = " is indifferent to "
--maps the character which is chosen by the function before to a string for the final output

------ Part 5 ----

thecounter :: Eq a => [a] -> a -> Int
thecounter xs x = length(takeWhile (/=x ) xs )
--Counts the elements up to the first occurence of a in [a]

checknotempty:: Eq a=> [a] -> [a]
checknotempty [] = []
checknotempty xs = tail xs
--Stops for the recursion of the last function as the function will continue forever but allows empty lists to be called for nsplit if it is in the middle of the list

nsplit :: Eq a =>[a] ->a -> [Int]
nsplit[] y = []
nsplit xs y =  filter (/=0) ( [z]++ nsplit(checknotempty(drop z xs)) y)
	where
	z = length(take(thecounter xs y) xs)
--Recursive function that calls thecounter and makes sure that gaps of zero are not included in the output of the final function and adds a number to the final list each time that it runs