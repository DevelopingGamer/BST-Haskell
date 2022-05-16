--------------------------------------------------------------------------------------------Property Based Tests
module PropTests where
import Test.HUnit(Test(..))
-- import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
-- https://hackage.haskell.org/package/HUnit

import Prelude hiding (lookup, insert)
import BST(BST (InternalNode, Leaf), isGreaterThan, lookup, insert, listEntries, isNotLeaf, remove, isLeaf, findMaxLeftNode, removeIf)

main :: IO ()
main = defaultMain prop_tests


generateTree :: Ord keyType => Eq item => [(keyType, item)] -> BST keyType item
generateTree [] = Leaf
generateTree ((a,b) : rest) = insert a b (generateTree rest)

makeListEven :: [(Int, Maybe String)] -> [(Int, Maybe String)]
makeListEven [] = []
makeListEven ((a,b) : rest) = ((a * 2 + 2), b) : makeListEven rest

makeListOdd :: [(Int, Maybe String)] -> [(Int, Maybe String)]
makeListOdd [] = []
makeListOdd ((a,b) : rest) = ((a * 2 + 1), b) : makeListOdd rest

prop_lookup_insert :: Int -> String -> [(Int, String)] -> Bool
prop_lookup_insert requiredKey requiredItem entryList =
    lookup requiredKey (insert requiredKey requiredItem (generateTree entryList)) == Just requiredItem

prop_lookup_insert2 :: String -> Int -> [(String, Int)] -> Bool
prop_lookup_insert2 requiredKey requiredItem entryList =
    lookup requiredKey (insert requiredKey requiredItem (generateTree entryList)) == Just requiredItem

prop_lookup_remove_insert :: Int -> Maybe String -> [(Int, Maybe String)] -> Bool
prop_lookup_remove_insert requiredKey requiredItem entryList =
    lookup requiredKey (remove requiredKey (insert requiredKey requiredItem (generateTree entryList))) == Nothing

prop_lookup_remove_insert2 :: String -> Maybe Int -> [(String, Maybe Int)] -> Bool
prop_lookup_remove_insert2 requiredKey requiredItem entryList =
    lookup requiredKey (remove requiredKey (insert requiredKey requiredItem (generateTree entryList))) == Nothing

prop_lookup_insert_insert :: Int -> Maybe String -> Maybe String -> [(Int, Maybe String)] -> Bool
prop_lookup_insert_insert requiredKey firstItem secondItem entryList =
    lookup requiredKey (insert requiredKey secondItem (insert requiredKey firstItem (generateTree entryList))) == Just secondItem

prop_lookup_insert_insert2 :: String -> Maybe Int -> Maybe Int -> [(String, Maybe Int)] -> Bool
prop_lookup_insert_insert2 requiredKey firstItem secondItem entryList =
    lookup requiredKey (insert requiredKey secondItem (insert requiredKey firstItem (generateTree entryList))) == Just secondItem

prop_remove_list :: Int -> Maybe String -> [(Int, Maybe String)] -> Bool
prop_remove_list requiredKey requiredItem entryList =
    listEntries(remove requiredKey (insert requiredKey requiredItem (generateTree entryList))) == listEntries(remove requiredKey (insert requiredKey requiredItem (generateTree entryList)))

prop_remove_list_length :: Int -> Maybe String -> [(Int, Maybe String)] -> Bool
prop_remove_list_length requiredKey requiredItem entryList =
    length(listEntries(remove requiredKey (insert requiredKey requiredItem (generateTree entryList)))) < length(listEntries(insert requiredKey requiredItem (generateTree entryList)))

prop_listEntries_insert_remove :: Int -> Maybe String -> [(Int, Maybe String)] -> Bool
prop_listEntries_insert_remove requiredKey requiredItem inputList = 
    listEntries (remove requiredKey (insert requiredKey requiredItem (generateTree inputList))) == inputList

prop_removeIf_odd :: [(Int, Maybe String)] -> Bool
prop_removeIf_odd entryList = 
    listEntries(removeIf odd (generateTree ((makeListEven (entryList)) ++ (makeListOdd (entryList))))) == listEntries(generateTree (makeListEven (entryList)))




prop_tests :: TestTree
prop_tests = testGroup "All Prop tests" 
    [testProperty "Lookup Returns Insert" prop_lookup_insert, 
    testProperty "Lookup Returns Insert" prop_lookup_insert2, 
    testProperty "Lookup returns the removed insert" prop_lookup_remove_insert,
    testProperty "Lookup returns the removed insert" prop_lookup_remove_insert2,
    testProperty "Lookup Returns new Insert" prop_lookup_insert_insert, 
    testProperty "Lookup Returns new Insert" prop_lookup_insert_insert2,
    testProperty "Remove list same as removed twice" prop_remove_list,
    testProperty "Remove list less than plain insert" prop_remove_list_length,
    testProperty "RemoveIf Odd returns original list of evens" prop_removeIf_odd
    ]