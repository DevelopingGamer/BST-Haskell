import Test.HUnit(Test(..), runTestTT)
-- import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
-- https://hackage.haskell.org/package/HUnit

import Prelude hiding (lookup, insert)
import BST(BST (InternalNode, Leaf), isGreaterThan, lookup, insert, listEntries, isNotLeaf, remove, isLeaf, findMaxLeftNode, removeIf)

main :: IO ()
main = do
    results <- runTestTT allTests
    print results


--IsGreater Tests
intIsGreater1 = 10
intIsGreater2 = 20
intIsGreater3 = 30

stringIsGreater1 = "as"
stringIsGreater2 = "in"
stringIsGreater3 = "to"

testIntIsGreater1 :: Test
testIntIsGreater1 = TestCase (assertEqual "testIsGreater | 10_20" False (isGreaterThan intIsGreater1 intIsGreater2))

testIntIsGreater2 :: Test
testIntIsGreater2 = TestCase (assertEqual "testIsGreater | 20_10" True (isGreaterThan intIsGreater2 intIsGreater1))

testIntIsGreater3 :: Test
testIntIsGreater3 = TestCase (assertEqual "testIsGreater | 20_30" False (isGreaterThan intIsGreater2 intIsGreater3))

testIntIsGreater4 :: Test
testIntIsGreater4 = TestCase (assertEqual "testIsGreater | 30_20" True (isGreaterThan intIsGreater3 intIsGreater2))

testIntIsGreater5 :: Test
testIntIsGreater5 = TestCase (assertEqual "testIsGreater | 10_30" False (isGreaterThan intIsGreater1 intIsGreater3))

testIntIsGreater6 :: Test
testIntIsGreater6 = TestCase (assertEqual "testIsGreater | 30_10" True (isGreaterThan intIsGreater3 intIsGreater1))

testStringIsGreater1 :: Test
testStringIsGreater1 = TestCase (assertEqual "testIsGreater | as_in" False (isGreaterThan stringIsGreater1 stringIsGreater2))

testStringIsGreater2 :: Test
testStringIsGreater2 = TestCase (assertEqual "testIsGreater | in_as" True (isGreaterThan stringIsGreater2 stringIsGreater1))

testStringIsGreater3 :: Test
testStringIsGreater3 = TestCase (assertEqual "testIsGreater | in_to" False (isGreaterThan stringIsGreater2 stringIsGreater3))

testStringIsGreater4 :: Test
testStringIsGreater4 = TestCase (assertEqual "testIsGreater | to_in" True (isGreaterThan stringIsGreater3 stringIsGreater2))

testStringIsGreater5 :: Test
testStringIsGreater5 = TestCase (assertEqual "testIsGreater | as_to" False (isGreaterThan stringIsGreater1 stringIsGreater3))

testStringIsGreater6 :: Test
testStringIsGreater6 = TestCase (assertEqual "testIsGreater | to_as" True (isGreaterThan stringIsGreater3 stringIsGreater1))

--LookUp Tests

testLookupTree1 = InternalNode 5 "Jade" (InternalNode 2 "Josh" Leaf Leaf) (InternalNode 10 "James" Leaf Leaf)
testLookupTree2 = InternalNode "C" 6 Leaf Leaf
testLookupTree3 = InternalNode "M" 6 (InternalNode "G" 36 Leaf Leaf) (InternalNode "S" 6 (InternalNode "P" 260 Leaf Leaf) (InternalNode "W" 90 Leaf Leaf))

testLookup1 :: Test
testLookup1 = TestCase (assertEqual "Lookup1_7_ShouldReturnNothing" (Nothing) (lookup 7 testLookupTree1))

testLookup2 :: Test
testLookup2 = TestCase (assertEqual "Lookup2_5_ShouldReturnJade" (Just "Jade") (lookup 5 testLookupTree1))

testLookup3 :: Test
testLookup3 = TestCase (assertEqual "Lookup3_LookupChildNode_10_ShouldReturnJames" (Just "James") (lookup 10 testLookupTree1))

testLookup4 :: Test
testLookup4 = TestCase (assertEqual "Lookup4_PolymorphicData_C_ShouldReturn6" (Just 6) (lookup "C" testLookupTree2))

testLookup5 :: Test
testLookup5 = TestCase (assertEqual "Lookup5_PolymorphicData_W_ShouldReturn90" (Just 90) (lookup "W" testLookupTree3))


--Insert Tests

testInsertTree1 = InternalNode 5 "Jade" (InternalNode 2 "Josh" Leaf Leaf) (InternalNode 10 "James" Leaf Leaf)
testInsertTree2 = InternalNode 5 "Jade" (InternalNode 2 "Josh" Leaf Leaf) (InternalNode 10 "James" (InternalNode 9 "Jamie" Leaf Leaf) Leaf)
testInsertTree3 = InternalNode 5 "Jade" (InternalNode 2 "Joe" Leaf Leaf) (InternalNode 10 "James" Leaf Leaf)
testInsertTree4 = InternalNode 5 "Jade" Leaf Leaf
testInsertTree5 = InternalNode "C" 6 Leaf Leaf
testInsertTree6 = InternalNode "C" 6 (InternalNode "A" 41 Leaf Leaf) Leaf
testInsertTree7 = InternalNode "M" 6 (InternalNode "G" 36 Leaf Leaf) (InternalNode "S" 6 (InternalNode "P" 260 Leaf Leaf) (InternalNode "W" 90 Leaf Leaf))
testInsertTree8 = InternalNode "M" 6 (InternalNode "G" 36 Leaf Leaf) (InternalNode "S" 6 (InternalNode "P" 270 Leaf Leaf) (InternalNode "W" 90 Leaf Leaf))

testInsert1 :: Test
testInsert1 = TestCase (assertEqual "Insert1_9Jamie_NewNodeBelow" testInsertTree2 (insert 9 "Jamie" testInsertTree1))

testInsert2 :: Test
testInsert2 = TestCase (assertEqual "Insert2_2Josh->Joe_ReplaceExistingNode" testInsertTree3 (insert 2 "Joe" testInsertTree1))

testInsert3 :: Test
testInsert3 = TestCase (assertEqual "Insert3_5Jade_EmptyTree" testInsertTree4 (insert 5 "Jade" Leaf))

testInsert4 :: Test
testInsert4 = TestCase (assertEqual "Insert4_A41_PolymorphicData_NewNodeBelow" testInsertTree6 (insert "A" 41 testInsertTree5))

testInsert5 :: Test
testInsert5 = TestCase (assertEqual "Insert5_P260->270_PolymorphicData_ReplaceExistingNode" testInsertTree8 (insert "P" 270 testInsertTree7))


--List Entries Tests

testList1 = [(5, "Jade")]
testList2 = [(2, "Josh"), (5, "Jade"), (9, "Jamie"), (10, "James")]
testList3 = [("G", 36), ("M", 6), ("P", 270), ("S", 6), ("W", 90)]
testListEntriesTree1 = InternalNode 5 "Jade" Leaf Leaf
testListEntriesTree2 = InternalNode 5 "Jade" (InternalNode 2 "Josh" Leaf Leaf) (InternalNode 10 "James" (InternalNode 9 "Jamie" Leaf Leaf) Leaf)
testListEntriesTree3 = InternalNode "M" 6 (InternalNode "G" 36 Leaf Leaf) (InternalNode "S" 6 (InternalNode "P" 270 Leaf Leaf) (InternalNode "W" 90 Leaf Leaf))


testListEntries1 :: Test
testListEntries1 = TestCase (assertEqual "ListEntries1_ProduceList" testList1 (listEntries testListEntriesTree1))

testListEntries2 :: Test
testListEntries2 = TestCase (assertEqual "ListEntries2_MultipleEntries" testList2 (listEntries testListEntriesTree2))

testListEntries3 :: Test
testListEntries3 = TestCase (assertEqual "ListEntries3_PolymorphicData" testList3 (listEntries testListEntriesTree3))

--IsLeaf Tests
testIsLeafTree1 = InternalNode 5 "Jade" Leaf Leaf
testIsLeafTree2 = Leaf

testIsLeaf1 :: Test
testIsLeaf1 = TestCase (assertEqual "TestJadeLeaf" False (isLeaf testIsLeafTree1))

testIsLeaf2 :: Test
testIsLeaf2 = TestCase (assertEqual "TestLeaf" True (isLeaf testIsLeafTree2))

--Remove Tests

testRemoveTree1 = InternalNode 5 "Jade" (InternalNode 2 "Josh" Leaf Leaf) (InternalNode 10 "James" Leaf Leaf)
testRemoveTree2 = InternalNode 5 "Jade" (InternalNode 2 "Josh" Leaf Leaf) (InternalNode 10 "James" (InternalNode 9 "Jamie" Leaf Leaf) Leaf)

testRemoveTree3 = InternalNode 5 "Jade" (InternalNode 2 "Josh" Leaf Leaf) (InternalNode 9 "Jamie" Leaf Leaf)
testRemoveTree4 = InternalNode 5 "Jade" (InternalNode 2 "Josh" Leaf Leaf) (InternalNode 10 "James" (InternalNode 9 "Jamie" Leaf Leaf) (InternalNode 25 "Juan" Leaf Leaf))
testRemoveTree5 = InternalNode 5 "Jade" (InternalNode 2 "Josh" Leaf Leaf) (InternalNode 9 "Jamie" Leaf (InternalNode 25 "Juan" Leaf Leaf))

testRemoveTree6 = InternalNode 10 "Jade" (InternalNode 2 "Josh" Leaf Leaf) (InternalNode 15 "James" (InternalNode 12 "Jamie" (InternalNode 11 "Jodie" Leaf Leaf) (InternalNode 14 "Jamal" Leaf Leaf)) (InternalNode 25 "Juan" Leaf Leaf))
testRemoveTree7 = InternalNode 10 "Jade" (InternalNode 2 "Josh" Leaf Leaf) (InternalNode 14 "Jamal" (InternalNode 12 "Jamie" (InternalNode 11 "Jodie" Leaf Leaf) Leaf) (InternalNode 25 "Juan" Leaf Leaf))

testRemoveTree8 = InternalNode 10 "Jade" (InternalNode 2 "Josh" Leaf Leaf) (InternalNode 15 "James" (InternalNode 12 "Jamie" (InternalNode 11 "Jodie" Leaf Leaf) (InternalNode 14 "Jamal" (InternalNode 13 "Joel" Leaf Leaf) Leaf)) (InternalNode 25 "Juan" Leaf Leaf))
testRemoveTree9 = InternalNode 10 "Jade" (InternalNode 2 "Josh" Leaf Leaf) (InternalNode 14 "Jamal" (InternalNode 12 "Jamie" (InternalNode 11 "Jodie" Leaf Leaf) (InternalNode 13 "Joel" Leaf Leaf)) (InternalNode 25 "Juan" Leaf Leaf))

testRemoveTree10 = InternalNode "C" 6 Leaf Leaf
testRemoveTree11 = InternalNode "C" 6 (InternalNode "A" 41 Leaf Leaf) Leaf

testRemoveTree12 = InternalNode "M" 6 (InternalNode "G" 36 Leaf Leaf) (InternalNode "W" 90 Leaf Leaf)
testRemoveTree13 = InternalNode "M" 6 (InternalNode "G" 36 Leaf Leaf) (InternalNode "S" 6 Leaf (InternalNode "W" 90 Leaf Leaf))

testRemoveTree14 = InternalNode "M" 6 (InternalNode "G" 36 Leaf Leaf) (InternalNode "S" 6 (InternalNode "P" 260 Leaf Leaf) (InternalNode "W" 90 Leaf Leaf))
testRemoveTree15 = InternalNode "M" 6 (InternalNode "G" 36 Leaf Leaf) (InternalNode "P" 260 Leaf (InternalNode "W" 90 Leaf Leaf))

testRemove1 :: Test 
testRemove1 = TestCase (assertEqual "Remove1_9Jamie_BothLeaf" testRemoveTree1 (remove 9 testRemoveTree2))

testRemove2 :: Test 
testRemove2 = TestCase (assertEqual "Remove2_10James_OneLeaf" testRemoveTree3 (remove 10 testRemoveTree2))

testRemove3 :: Test
testRemove3 = TestCase (assertEqual "Remove3_10James_NeitherLeaf_OneLevel" testRemoveTree5 (remove 10 testRemoveTree4))

testRemove4 :: Test
testRemove4 = TestCase (assertEqual "Remove4_15James_NeitherLeaf_TwoLevels " testRemoveTree7 (remove 15 testRemoveTree6))

testRemove5 :: Test
testRemove5 = TestCase (assertEqual "Remove5_15James_NeitherLeaf_ThreeLevels" testRemoveTree9 (remove 15 testRemoveTree8))

testRemove6 :: Test
testRemove6 = TestCase (assertEqual "Remove6_A41_PolymorphicData_BothLeaf" testRemoveTree10 (remove "A" testRemoveTree11))

testRemove7 :: Test
testRemove7 = TestCase (assertEqual "Remove7_S6_PolymorphicData_OneLeaf" testRemoveTree12 (remove "S" testRemoveTree13))

testRemove8 :: Test
testRemove8 = TestCase (assertEqual "Remove8_S6_PolymorphicData_BothLeaf" testRemoveTree15 (remove "S" testRemoveTree14))

--IsNotLeaf Tests
testNotLeafTree1 = InternalNode 5 "Jade" Leaf Leaf
testNotLeaftree2 = Leaf

testNotLeaf1 :: Test
testNotLeaf1 = TestCase (assertEqual "TestJadeLeaf" True (isNotLeaf testNotLeafTree1))

testNotLeaf2 :: Test
testNotLeaf2 = TestCase (assertEqual "TestLeaf" False (isNotLeaf testNotLeaftree2))

--FindMaxLeftNode Tests
testMaxLeftIntsTree1 = InternalNode 15 "James" (InternalNode 12 "Jamie" (InternalNode 11 "Jodie" Leaf Leaf) (InternalNode 14 "Jamal" Leaf Leaf)) (InternalNode 25 "Juan" Leaf Leaf)
maxLeftTreeInts1 = InternalNode 25 "Juan" (InternalNode 15 "James" (InternalNode 12 "Jamie" (InternalNode 11 "Jodie" Leaf Leaf) (InternalNode 14 "Jamal" Leaf Leaf)) Leaf) Leaf

testMaxLeftIntsTree2 = InternalNode 15 "James" (InternalNode 12 "Juan" Leaf Leaf) (InternalNode 25 "Jamie" (InternalNode 20 "Jodie" Leaf Leaf) (InternalNode 30 "Jamal" (InternalNode 27 "Jolene" Leaf Leaf) Leaf))
maxLeftTreeInts2 = InternalNode 30 "Jamal" (InternalNode 15 "James" (InternalNode 12 "Juan" Leaf Leaf) (InternalNode 25 "Jamie" (InternalNode 20 "Jodie" Leaf Leaf) (InternalNode 27 "Jolene" Leaf Leaf))) Leaf

testMaxLeftStringsTree1 = InternalNode "S" 6 (InternalNode "P" 260 Leaf (InternalNode "R" 100 Leaf Leaf)) (InternalNode "W" 90 Leaf Leaf)
maxLeftNodeStrings1 = InternalNode "W" 90 (InternalNode "S" 6 (InternalNode "P" 260 Leaf (InternalNode "R" 100 Leaf Leaf)) Leaf) Leaf

testMaxLeftStringsTree2 = InternalNode "S" 6 (InternalNode "P" 260 Leaf (InternalNode "R" 100 (InternalNode "Q" 200 Leaf Leaf) Leaf)) (InternalNode "W" 90 (InternalNode "U" 69 Leaf Leaf) Leaf)
maxLeftNodeStrings2 = InternalNode "W" 90 (InternalNode "S" 6 (InternalNode "P" 260 Leaf (InternalNode "R" 100 (InternalNode "Q" 200 Leaf Leaf) Leaf)) (InternalNode "U" 69 Leaf Leaf)) Leaf


testMaxLeftInts1 :: Test
testMaxLeftInts1 = TestCase (assertEqual "testMaxLeftIntKeysNoChild | 15_James=14_Jamal" maxLeftTreeInts1 (findMaxLeftNode testMaxLeftIntsTree1 testMaxLeftIntsTree1 Leaf))

testMaxLeftInts2 :: Test
testMaxLeftInts2 = TestCase (assertEqual "testMaxLeftIntKeysWithWithChild | 15_James=14_Jamal" maxLeftTreeInts2 (findMaxLeftNode testMaxLeftIntsTree2 testMaxLeftIntsTree2 Leaf))

testMaxLeftStrings1 :: Test
testMaxLeftStrings1 = TestCase (assertEqual "testMaxLeftStringKeysNoChild | S_6=R_100" maxLeftNodeStrings1 (findMaxLeftNode testMaxLeftStringsTree1 testMaxLeftStringsTree1 Leaf))

testMaxLeftStrings2 :: Test
testMaxLeftStrings2 = TestCase (assertEqual "testMaxLeftStringKeysWithChild | S_6=R_100" maxLeftNodeStrings2 (findMaxLeftNode testMaxLeftStringsTree2 testMaxLeftStringsTree2 Leaf))


--RemoveIf Tests

testRemoveIfTree1 = InternalNode 10 "Jade" (InternalNode 2 "Josh" Leaf Leaf) (InternalNode 14 "Jamal" (InternalNode 12 "Jamie" (InternalNode 11 "Jodie" Leaf Leaf) (InternalNode 13 "Joel" Leaf Leaf)) (InternalNode 25 "Juan" Leaf Leaf))
testRemoveIfTree2 = InternalNode 10 "Jade" (InternalNode 2 "Josh" Leaf Leaf) (InternalNode 14 "Jamal" (InternalNode 12 "Jamie" Leaf Leaf) Leaf)

testRemoveIfTree3 = InternalNode 10 "Jade" (InternalNode 2 "Josh" Leaf Leaf) (InternalNode 15 "James" (InternalNode 12 "Jamie" (InternalNode 11 "Jodie" Leaf Leaf) (InternalNode 14 "Jamal" (InternalNode 13 "Joel" Leaf Leaf) Leaf)) (InternalNode 25 "Juan" Leaf Leaf))
testRemoveIfTree4 = InternalNode 15 "James" (InternalNode 11 "Jodie" Leaf (InternalNode 13 "Joel" Leaf Leaf)) (InternalNode 25 "Juan" Leaf Leaf)

testRemoveIfTree5 = InternalNode "M" 6 (InternalNode "G" 36 Leaf Leaf) (InternalNode "S" 6 (InternalNode "P" 260 Leaf Leaf) (InternalNode "W" 90 Leaf Leaf))
testRemoveIfTree6 = (InternalNode "S" 6 (InternalNode "P" 260 Leaf Leaf) (InternalNode "W" 90 Leaf Leaf))
testRemoveIfTree7 = InternalNode "M" 6 (InternalNode "G" 36 Leaf Leaf) Leaf

testRemoveIf1 :: Test
testRemoveIf1 = TestCase (assertEqual "RemoveIf1_OddKeys" testRemoveIfTree2 (removeIf odd testRemoveIfTree1))

testRemoveIf2 :: Test
testRemoveIf2 = TestCase (assertEqual "RemoveIf2_EvenKeys" testRemoveIfTree4 (removeIf even testRemoveIfTree3))

testRemoveIf3 :: Test
testRemoveIf3 = TestCase (assertEqual "RemoveIf3_PolymorphicData_EvenKeys" testRemoveIfTree7 (removeIf (\a -> a > "N") testRemoveIfTree5))

testRemoveIf4 :: Test
testRemoveIf4 = TestCase (assertEqual "RemoveIf4_PolymorphicData_OddKeys" testRemoveIfTree6 (removeIf (\a -> a < "N") testRemoveIfTree5))

--All Tests

allTests :: Test
allTests = TestList [testMaxLeftStrings1, testMaxLeftStrings2, testRemoveIf3, testRemoveIf4, testLookup1, testLookup2, testLookup3, testLookup4, testLookup5, testInsert1, testInsert2, testInsert3, testInsert4, testInsert5, testListEntries1, testListEntries2, testListEntries3, testRemove1, testRemove2, testRemove3, testRemove4, testRemove5, testRemove6, testRemove7, testRemove8, testRemoveIf1, testRemoveIf2, testIntIsGreater1, testIntIsGreater2, testIntIsGreater3, testIntIsGreater4, testIntIsGreater5, testIntIsGreater6, testMaxLeftInts1, testMaxLeftInts2, testIsLeaf1, testIsLeaf2, testNotLeaf1, testNotLeaf2]
