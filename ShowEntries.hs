{-# LANGUAGE LambdaCase, NumericUnderscores, QuasiQuotes #-}
module Main where

import Data.Decimal (Decimal)
import Data.Function (on)
import Data.List (intercalate, groupBy)
import Text.RawString.QQ (r)

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
                (entry@(Entry Dr _ _) : xs) -> showEntry "借：" entry   : map (showEntry "    ") xs
                (entry@(Entry Cr _ _) : xs) -> showEntry "    贷：" entry : map (("      " ++ ) . showEntry "\t") xs
                []                          -> []
            showEntry :: String -> Entry -> String
            showEntry dir (Entry _ acc amt) = intercalate "\t" [dir ++ acc, show amt]

main :: IO ()
main = 
    putStrLn [r|8.3附加题:
        3月1日，甲公司向乙公司销售一批商品200件，单位销售价格为1万元，单位成本为0.8万元，开出的增值税专用发票上注明的销售价格为200万元，
        增值税税额为26万元。协议约定，乙公司在2×19年6月30日之前有权退回商品。商品已经发出，款项已经收到。甲公司根据过去的经验，
        估计该批商品退货率约为8%。3月31日，甲公司对退货率进行了重新估计，将该批商品的退货率调整为10%。根据资料，
        判断甲公司向乙公司销售商品是否确认收入，如果确认，说明收入确认时点，并编制甲公司2×19年向乙公司销售商品相关的会计分录。|]
    >> putStrLn "\n应当确认收入，收入确认时点为：3月1日\n3月1日分录："
    >> (putStrLn . showEntries)  [ Entry Dr "应收账款" 226.00
                                 , Entry Cr "主营业务收入" 184.00
                                 , Entry Cr "应交税费——应交增值税（销项税额）" 26.0
                                 , Entry Cr "预计负债——应付退货款" 16.0]
    >> putStrLn ""
    >> (putStrLn . showEntries)  [ Entry Dr "主营业务成本" 147.2
                                 , Entry Cr "应收退货成本" 12.8
                                 , Entry Cr "库存商品" 160]

    >> putStrLn "\n3月31日调整时："
    >> (putStrLn . showEntries)  [ Entry Dr "主营业务收入" 4
                                 , Entry Cr "预计负债——应付退货款" 4
                                 , Entry Dr "应收退货成本" 3.2
                                 , Entry Cr "主营业务成本" 3.2]
    
    >> putStrLn ""
    >> putStrLn [r|8.4附加题：
        2×22年3月2日，甲公司以一台A设备（生产经营用）和一项无形资产（商标权）换入乙公司一项固定资产（管理用办公楼）。
        甲公司换出的A设备账面原值为70万元，已计提累计折旧20万元，公允价值为80万元；无形资产的原价为150万元，累计摊销为45万元，
        公允价值为160万元。乙公司换出的固定资产，已使用2年，账面原值为270万元，已计提折旧18万元，当日的公允价值为300万元。
        甲公司另向乙公司支付银行存款67万元。甲公司为换入办公楼支付相关手续费7万元。乙公司为换入A设备支付运杂费等5万元，
        为换入无形资产发生手续费等2万元。甲、乙公司均为增值税一般纳税人，设备适用的增值税税率均为13%，商标权适用的增值税税率为6%，
        办公楼适用的增值税税率为9%。以上资产公允价值均等于计税价格。交换之后，双方均保持资产原确认状态不变。
        该项资产交换具有商业实质且换入、换出资产的公允价值均能可靠计量。不考虑其他因素。
        要求：分别计算甲、乙公司换入资产的入账价值，并分别编制相关会计分录。|]
    
    >> putStrLn [r|
    甲公司向乙公司支付的不含税补价＝300－（80＋160）＝60（万元）（0.5分）；
    甲公司换入办公楼的入账价值＝80＋160＋支付的补价60＋7＝307（万元）；
    乙公司换入A设备的入账价值＝（300－收到的补价60）×[80/（80＋160）]＋5＝85（万元）；
    乙公司换入无形资产的入账价值＝（300－收到的补价60）×[160/（80＋160）]＋2＝162（万元）（0.5分）|]
    >> putStrLn "\n甲换入办公楼的分录："
    >>  (putStrLn . showEntries) [ Entry Dr "固定资产清理" 50.0
                                 , Entry Dr "累计折旧" 20.0
                                 , Entry Cr "固定资产——A设备" 70.0
                                 
                                 
                                 , Entry Dr "固定资产——办公楼" 307
                                 , Entry Dr "应交税费——应交增值税（进项税额）" 27
                                 , Entry Dr "累计摊销" 45

                                 , Entry Cr "资产处置损益" 85
                                 , Entry Cr "应交税费——应交增值税（销项税额）" 20
                                 , Entry Cr "银行存款" 74
                                 , Entry Cr "无形资产" 150
                                 , Entry Cr "固定资产清理" 50 ]

    >> putStrLn "\n乙换入设备及无形资产的分录："
    >>  (putStrLn . showEntries) [ Entry Dr "固定资产清理" 252
                                 , Entry Cr "累计折旧" 18.0
                                 , Entry Cr "固定资产——办公楼" 270.0

                                 , Entry Dr "固定资产——A设备" 85
                                 , Entry Dr "无形资产" 162
                                 , Entry Dr "银行存款" 60

                                 , Entry Cr "资产处置损益" 48.0
                                 , Entry Cr "应交收费——应交增值税（销项税额）" 27
                                 , Entry Cr "固定资产清理" 252.0 ]
    >> putStrLn ""
    >> putStrLn [r| 8.5附加题
            甲公司以土地使用权自丙公司手中换入乙公司100万股普通股，占乙公司股份比例1％，甲公司将其指定为其他权益工具投资，
            交易当天，乙公司股份的公允市价为每股2元。甲公司土地使用权的账面原价为300万元，累计摊销120万元，已提减值准备50万元，
            公允价为190万元，增值税率为9％，双方约定，由甲公司另行支付银行存款3万元。丙公司所持乙公司股份一直按交易性金融资产核算，
            交易当天账面余额130万元，假定不考虑相关交易费用。
            ------
            问题：甲和丙的相关账务处理~ |]
    >> putStrLn "\n甲公司分录："
    >> (putStrLn . showEntries) [ Entry Dr "其他权益工具投资" 200
                                , Entry Dr "应交税费——应交增值税（进项税额）" 18
                                , Entry Dr "资产处置损益" 67.9
                                , Entry Dr "无形资产减值准备" 50
                                , Entry Dr "累计摊销" 120

                                , Entry Cr "银行存款" 3
                                , Entry Cr "无形资产" 300
                                , Entry Cr "应交税费——应交增值税（销项税额）" 17.1]
    >> putStrLn "\n乙公司分录："
    >> (putStrLn . showEntries) [ Entry Dr "无形资产" 190
                                , Entry Dr "银行存款" 3
                                , Entry Dr "应交税费——应交增值税（进项税额）" 17.1
                                , Entry Dr "投资收益" 68.4

                                , Entry Cr "交易性金融资产" 130
                                , Entry Cr "应交税费——应交增值税（销项税额）" 11.7]
    >> putStrLn ""
