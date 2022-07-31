{-# LANGUAGE LambdaCase, NumericUnderscores #-}

import Data.Decimal (Decimal)
import Data.Function (on)
import Data.List (intercalate, groupBy)

data Direction = Dr | Cr deriving (Eq)
data Entry = Entry  { direction :: Direction
                    , account   :: String
                    , amount    :: Decimal }

showEntries :: [Entry] -> String
showEntries = 
    intercalate "\n"
    . map (intercalate "\n" . convertToStringList)
    . groupBy ((==) `on` direction)
    where   convertToStringList :: [Entry] -> [String]
            convertToStringList = \case
                (entry@(Entry Dr _ _) : xs) -> showEntry "借：" entry   : map (showEntry "  \t") xs
                (entry@(Entry Cr _ _) : xs) -> showEntry "\t贷：" entry : map (("  " ++ ) . showEntry "\t") xs
                []                          -> []
            showEntry :: String -> Entry -> String
            showEntry dir (Entry _ acc amt) = intercalate "\t" [dir, acc, show amt]


main :: IO ()
main = 
    putStrLn "\n一、取得长期股权投资时："
    >> (putStrLn . showEntries)  [ Entry Dr "长期股权投资——成本" 7_940.00
                                 , Entry Cr "银行存款" 7_900.00 
                                 , Entry Cr "投资收益" 40.00]
    >> putStrLn "\n二、实现净利润时："
    >> (putStrLn . showEntries)  [ Entry Dr "长期股权投资——损益调整" 260.00
                                 , Entry Cr "投资收益" 260.00]
    >> putStrLn "\n三、（1）宣告发放现金股利时："
    >> (putStrLn . showEntries)  [ Entry Dr "应收股利" 160.00
                                 , Entry Cr "长期股权投资——损益调整" 160.00]
    >> putStrLn "\n三、（2）实际发放现金股利时："
    >> (putStrLn . showEntries)  [ Entry Dr "应收股利" 160.00
                                 , Entry Cr "长期股权投资——损益调整" 160.00]
    >> putStrLn "\n四、发生亏损时："
    >> (putStrLn . showEntries)  [ Entry Dr "投资收益" 8_000.00
                                 , Entry Cr "长期股权投资——损益调整" 8_000.00]
    >> putStrLn "乙公司确认投资收益为：借方7,700.00万元。"
