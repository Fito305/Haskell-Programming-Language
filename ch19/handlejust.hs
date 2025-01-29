import Control.Exception


catchIt :: exception -> Maybe ()
catchIt (ArithException DivideByZero) = Just ()
catchIt _ = Nothing


handler :: () -> IO ()
handler _ = putStrLn "Caugth error: divid by zero"


safePrint:: Integral -> IO ()
safePrint x = handleJust catchIt handler (print x)
