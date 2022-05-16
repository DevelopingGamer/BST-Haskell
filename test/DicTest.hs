module DicTest where
import Test.HUnit(Test(..), runTestTT)
-- import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
-- https://hackage.haskell.org/package/HUnit

import Prelude hiding (lookup, insert)
import Dictionary(Dictionary, dicCreate, dicLookup, dicListEntries, dicInsert, dicRemove, dicRemoveIf)

main :: IO ()
main = do
    results <- runTestTT allTests
    print results

--Create Dictionary Tests

testCreateDicList1 = [(2, "Josh"), (5, "Jade"), (9, "Jamie"), (10, "James")]
testCreateDicList2 = [("G", 36), ("M", 6), ("P", 270), ("S", 6), ("W", 90)]

testCreateDic1 :: Test
testCreateDic1 = TestCase (assertEqual "CreateNewDictionary1_ListEntriesOfTheDic" (testCreateDicList1) (dicListEntries (dicCreate testCreateDicList1)))

testCreateDic2 :: Test
testCreateDic2 = TestCase (assertEqual "CreateNewDictionary2_ListEntriesOfTheDic_Polymorphic" (testCreateDicList2) (dicListEntries (dicCreate testCreateDicList2)))

--Lookup


testLookupList1 = [(2, "Josh"), (5, "Jade"), (9, "Jamie"), (10, "James")]
testLookupList2 = [("G", 36), ("M", 6), ("P", 270), ("S", 6), ("W", 90)]

testLookup1 :: Test
testLookup1 = TestCase (assertEqual "Lookup1_7_ShouldReturnNothing" (Nothing) (dicLookup 7 (dicCreate testLookupList1)))

testLookup2 :: Test
testLookup2 = TestCase (assertEqual "Lookup2_5_ShouldReturnJade" (Just "Jade") (dicLookup 5 (dicCreate testLookupList1)))

testLookup3 :: Test
testLookup3 = TestCase (assertEqual "Lookup3_p_ShouldReturnNothing_Polymorphic" (Nothing) (dicLookup "p" (dicCreate testLookupList2)))

testLookup4 :: Test
testLookup4 = TestCase (assertEqual "Lookup4_P_ShouldReturn270_Polymorphic" (Just 270) (dicLookup "P" (dicCreate testLookupList2)))


--Insert


testInsertList1 = [(2, "Josh"), (5, "Jade"), (9, "Jamie"), (10, "James")]
testInsertList2 = [(2, "Josh"), (5, "Jade"), (9, "Jamie"), (10, "James"), (12, "Jamal")]

testInsertList3 = [(2, "Josh"), (5, "Jade"), (10, "James"), (12, "Jamal")]
testInsertList4 = [(2, "Josh"), (5, "Jade"), (9, "Jamie"), (10, "James"), (12, "Jamal")]


testInsertList5 = [("G", 36), ("M", 6), ("S", 6), ("W", 90)]
testInsertList6 = [("G", 36), ("M", 6), ("P", 270), ("S", 6), ("W", 90)]

testInsert1 :: Test
testInsert1 = TestCase (assertEqual "Insert1_12Jamal_IncludedInList" testInsertList2 (dicListEntries(dicInsert 12 "Jamal" (dicCreate testInsertList1))))

testInsert2 :: Test
testInsert2 = TestCase (assertEqual "Insert2_9Jamie_IncludedInList" testInsertList4 (dicListEntries(dicInsert 9 "Jamie" (dicCreate testInsertList3))))

testInsert3 :: Test
testInsert3 = TestCase (assertEqual "Insert3_P270_IncludedInList_Polymorphic" testInsertList6 (dicListEntries(dicInsert "P" 270 (dicCreate testInsertList5))))

--List Entries Tests

testListEntriesList1 = [(2, "Josh"), (5, "Jade"), (9, "Jamie"), (10, "James")]
testListEntriesList2 = [("G", 36), ("M", 6), ("P", 270), ("S", 6), ("W", 90)]

testlistEntriesDic1 :: Test
testlistEntriesDic1 = TestCase (assertEqual "ListEntriesInCreatedDictionary" (testListEntriesList1) (dicListEntries (dicCreate testListEntriesList1)))

testlistEntriesDic2 :: Test
testlistEntriesDic2 = TestCase (assertEqual "ListEntriesInCreatedDictionary_Polymorphic" (testListEntriesList2) (dicListEntries (dicCreate testListEntriesList2)))

--Remove Tests

testRemoveList1 = [(2, "Josh"), (5, "Jade"), (9, "Jamie"), (10, "James"), (12, "Jamal")]
testRemoveList2 = [(2, "Josh"), (5, "Jade"), (9, "Jamie"), (10, "James")]

testRemoveList3 = [(2, "Josh"), (5, "Jade"), (9, "Jamie"), (10, "James"), (12, "Jamal")]
testRemoveList4 = [(2, "Josh"), (5, "Jade"), (10, "James"), (12, "Jamal")]

testRemoveList5 = [("G", 36), ("M", 6), ("P", 270), ("S", 6), ("W", 90)]
testRemoveList6 = [("G", 36), ("M", 6), ("S", 6), ("W", 90)]


testRemove1 :: Test
testRemove1 = TestCase (assertEqual "Remove1_12Jamal_IncludedInList" testRemoveList2 (dicListEntries(dicRemove 12 (dicCreate testRemoveList1))))

testRemove2 :: Test
testRemove2 = TestCase (assertEqual "Remove2_9Jamie_IncludedInList" testRemoveList4 (dicListEntries(dicRemove 9 (dicCreate testRemoveList3))))

testRemove3 :: Test
testRemove3 = TestCase (assertEqual "Remove3_P270_IncludedInList_Polymorphic" testRemoveList6 (dicListEntries(dicRemove "P" (dicCreate testRemoveList5))))


--RemoveIf Tests

testRemoveIfList1 = [(2, "Josh"), (5, "Jade"), (9, "Jamie"), (10, "James"), (12, "Jamal")]
testRemoveIfList3 = [(5, "Jade"), (9, "Jamie")]
testRemoveIfList2 = [(2, "Josh"), (10, "James"), (12, "Jamal")]
testRemoveIfList4 = [("G", 36), ("M", 6), ("P", 270), ("S", 6), ("W", 90)]
testRemoveIfList6 = [("P", 270), ("S", 6), ("W", 90)]
testRemoveIfList5 = [("G", 36), ("M", 6)]


testRemoveIf1 :: Test
testRemoveIf1 = TestCase (assertEqual "RemoveIf1_OddKeys" testRemoveIfList2 (dicListEntries(dicRemoveIf odd (dicCreate testRemoveList1))))

testRemoveIf2 :: Test
testRemoveIf2 = TestCase (assertEqual "RemoveIf2_EvenKeys" testRemoveIfList3 (dicListEntries(dicRemoveIf even (dicCreate testRemoveList1))))

testRemoveIf3 :: Test
testRemoveIf3 = TestCase (assertEqual "RemoveIf3_PolymorphicData_EvenKeys" testRemoveIfList5 (dicListEntries(dicRemoveIf (\a -> a > "N") (dicCreate testRemoveIfList4))))

testRemoveIf4 :: Test
testRemoveIf4 = TestCase (assertEqual "RemoveIf4_PolymorphicData_OddKeys" testRemoveIfList6 (dicListEntries(dicRemoveIf (\a -> a < "N") (dicCreate testRemoveIfList4))))

-- All tests

allTests :: Test
allTests = TestList [testCreateDic1, testCreateDic2, testLookup1, testLookup2, testLookup3, testLookup4, testInsert1, testInsert2, testInsert3, testRemove1, testRemove2, testRemove3, testlistEntriesDic1,testlistEntriesDic2, testRemoveIf1, testRemoveIf2, testRemoveIf3, testRemoveIf4]
