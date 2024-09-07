-- We define a new data type using the data keyword.
-- BookInfo is the ```type constructor and Book is the value constructor```
data BookInfo = Book Int String [String]
                deriving (Show) 

-- BookInfo after the data keyword is the name of our new type. We call BookInfo a 
-- ```type constructor```. Once we define a type, we will use its type constructor
-- to refer to it. It must start with a capital letter.

-- The Book that follows is the name of the ```value constructor``` (sometimes called 
-- a `data constructor`). We use this to create a value of the BookInfo type.
-- A ```value constructor's name``` must also start with a capital letter.

-- After Book, the Int, String, [String] that follow are the ```components```
-- of the type. A component serves the same purpose in Haskell as a `field` in 
-- a structure or class would in another language: it's a "slot" where we keep
-- a value. (We'll often refer to components as fields).

-- In this example, the Int represents a book's identifier, String represents its title,
-- ans [String] represents the names of its authors. 

-- We can create a new value of type BookInfo by treating Book as a functin and applying 
-- it with arguments of types Int, String and [String].
myInfo = Book 9787483748 "Algebra of Programming"
         ["Richard Bird", "Oege de Moor"]

data MagazineInfo = Magazine Int String [String]
                    deriving (Show)
-- Even though this MagazineInfo type has the same structure as our BookInfo type,
-- Haskell treats the type as distinct because their type and value constructures have 
-- different names. 


-- Once we define a type, we can experiment with it in ghci. We begin by using the 
-- :load our source file:
-- `ghci> :load BookStore.hs
-- `ghci> myInfo
-- We can constuct new values interactively in ghci, too:
-- ghci> book 0 "Title" ["Author-1", Author-2"]


type CustomerID = Int
type ReviewBody = String
data BeterReview = BeterReview BookInfo CustomerID ReviewBody

type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                   | CashOnDelivery
                   | Invoice CustomerID
                     deriving (Show)
