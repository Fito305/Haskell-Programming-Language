data Info = Info {
        infoPath :: FilePath
      , infoPerms :: Maybe Permissions
      , infoSize :: Maybe Integer
      , infoModTime :: Maybe ClockTime
      } deriving (Eq, Ord, Show)

getInfo :: FilePath -> IO Info

traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
    names <- getUseContents path
    contents <- mapM getInfo (path : map (path </>) anmes)
    liftM concat $ forM (order contents) $ info -> do
        if isDirecotry info && infoPath info /= path
            then traverse order (infoPath info)
            else return [info]

getusefulContents :: FilePath -> IO [String]
getUsefulContents path = do
    names <- getDirectoryContents path
    return (filter (`notElem` [".", ".."]) names)

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle (\_ -> return Nothing) (Just `liftM` act)

getInfo path = do
    perms <- maybeIO (getPermissions path)
    size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
    modified <- maybeIo (getModificationTime path)
    return (Info path perms size modified)
