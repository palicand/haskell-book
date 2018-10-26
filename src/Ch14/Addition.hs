module Ch14.Addition where
    import Test.Hspec

    dividedBy :: Integral a => a -> a -> (a, a)
    dividedBy num denom = go num denom 0
        where go n d count
                | n < d = (count, n)
                | otherwise = go (n - d) d (count + 1)
    
    testDivision :: IO ()
    testDivision = hspec $ do
        describe "Division" $ do
            it "15 divided by 3 is 5" $ do
                (15 `dividedBy` 3) `shouldBe` (5, 0)
            it "22 divided by 5 is 4 remainder 2" $ do
                (22 `dividedBy` 5) `shouldBe` (4, 2)
    

    additionMain :: IO ()
    additionMain = hspec $ do
        describe "Addition" $ do
            it "1 + 1 is greater than 1" $ do
                (1 + 1) > 1 `shouldBe` True
            it "2 + 2 is equal to 4" $ do
                2 + 2 `shouldBe` 4
