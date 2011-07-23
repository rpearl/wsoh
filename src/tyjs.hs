import BrownPLT.JavaScript.Parser

main :: IO ()
main = print =<< fmap (`parseScriptFromString` "stdin") getContents
