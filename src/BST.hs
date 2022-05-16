{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
module BST
    ( BST(..), emptyTree, isGreaterThan, lookup, insert, listEntries, isNotLeaf, remove, isLeaf, findMaxLeftNode, removeIf
    ) where

import Prelude hiding (lookup, insert)


data BST keyType item = InternalNode keyType item (BST keyType item) (BST keyType item) 
              | Leaf deriving (Eq, Ord, Show)


emptyTree :: BST keyType item
emptyTree = Leaf

isGreaterThan :: Ord keyType => keyType -> keyType -> Bool
isGreaterThan key1 key2 = key1 > key2

lookup :: Ord keyType => keyType -> BST keyType item -> Maybe item
lookup soughtKey Leaf = Nothing
lookup soughtKey (InternalNode key item leftChild rightChild)
 = if soughtKey == key
    then Just item
    else if isGreaterThan soughtKey key
        then lookup soughtKey rightChild
    else lookup soughtKey leftChild

insert :: Ord keyType => keyType -> item -> BST keyType item -> BST keyType item
insert newKey value Leaf = InternalNode newKey value Leaf Leaf
insert newKey value (InternalNode key item leftChild rightChild)
    = if newKey == key
    then InternalNode key value leftChild rightChild
    else if isGreaterThan key newKey
        then InternalNode key item (insert newKey value leftChild) rightChild
    else InternalNode key item leftChild (insert newKey value rightChild)

listEntries :: Ord keyType => BST keyType item -> [(keyType, item)]
listEntries tree = listEntriesRec tree [] 

listEntriesRec :: Ord keyType => BST keyType item -> [(keyType, item)] -> [(keyType, item)] 
listEntriesRec Leaf list = list
listEntriesRec (InternalNode key item leftChild rightChild) list
    = list''' where
        
        list' = listEntriesRec rightChild list

        list'' = (key, item) : list' 

        list''' = listEntriesRec leftChild list''

   
isNotLeaf :: BST keyType item -> Bool
isNotLeaf Leaf = False
isNotLeaf _    = True


remove :: Ord keyType => keyType -> BST keyType item -> BST keyType item
remove keyToRemove Leaf = Leaf 
remove keyToRemove (InternalNode key item leftChild rightChild) 
 = if keyToRemove == key
     then if isLeaf leftChild && isNotLeaf rightChild
         then rightChild
         else if isLeaf rightChild && isNotLeaf leftChild
             then leftChild
        else if isNotLeaf leftChild && isNotLeaf rightChild
            then findMaxLeftNode leftChild leftChild rightChild
        else Leaf
    else if isGreaterThan key keyToRemove
        then InternalNode key item (remove keyToRemove leftChild) rightChild
    else InternalNode key item leftChild (remove keyToRemove rightChild)

isLeaf :: BST keyType item -> Bool
isLeaf Leaf = True
isLeaf _    = False

findMaxLeftNode :: Ord keyType => BST keyType item -> BST keyType item -> BST keyType item -> BST keyType item
findMaxLeftNode Leaf leftTree rightTree = Leaf
findMaxLeftNode (InternalNode key item leftChild rightChild) leftTree rightTree
    = if isNotLeaf rightChild
        then findMaxLeftNode rightChild leftTree rightTree
     else InternalNode key item (remove key leftTree) rightTree

removeIf :: Ord keyType => (keyType -> Bool) -> BST keyType item  -> BST keyType item
removeIf opp Leaf  = Leaf
removeIf opp (InternalNode key item leftChild rightChild)
    = if opp key
        then remove key (InternalNode key item (removeIf opp leftChild) (removeIf opp rightChild))
    else InternalNode key item (removeIf opp leftChild) (removeIf opp rightChild)


    
            
