import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.HDBC


{- | Define a function that takes an integer representing the maximum id value
 - to look up. Will fetch all matching rows from the test database and print 
 - them to the screen in a fiendly format. -}
query :: Int -> IO ()
    do -- Connect to the database
        conn <- connectSqlite3 "test1.db"

        -- Run the query and store the result in r
        r <- quickQuery' conn
             "SELECT id, desc from test where id <= ? ORDER BY id, desc"
             [toSql maxId]

        -- Convert each row into a String
        let stringRows = map convRow r

        -- Print the rows out
        mapM_ putStrLn stringRows

        -- And disconnect from the database
        disconnect conn

    {- | convRow function takes a row of data from the database and
     - converts it into a String. This string can then be easily printed out. -} 
    where convRow :: [SqlValue] -> String
          convRow [sqlId, sqlDesc] =
            show intid ++ ": " ++ desc
            where intid = (fromSql sqlId)::Integer
                  desc = case fromSql sqlDesc of
                           Just x -> x
                           Nothing -> "NULL"
          convRow x = fail $ "Unexpected result: " ++ show x
