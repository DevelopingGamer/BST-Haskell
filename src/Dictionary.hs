 module Dictionary
     ( Dictionary, dicCreate, dicLookup, dicListEntries, dicInsert, dicRemove, dicRemoveIf
     ) where

import Prelude hiding (lookup, insert)
import BST(BST (InternalNode, Leaf), insert, lookup, listEntries, remove, removeIf)

type Dictionary keyType item = BST keyType item

dicCreate :: Ord keyType => [(keyType, item)] -> Dictionary keyType item
dicCreate [] = Leaf
dicCreate ((a,b) : rest) = dicInsert a b (dicCreate rest)

dicLookup :: Ord keyType => keyType -> Dictionary keyType item -> Maybe item
dicLookup soughtKey dictionary = lookup soughtKey dictionary

dicInsert :: Ord keyType => keyType -> item -> Dictionary keyType item -> Dictionary keyType item
dicInsert newKey value dictionary = insert newKey value dictionary

dicListEntries :: Ord keyType => Dictionary keyType item -> [(keyType, item)]
dicListEntries dictionary = listEntries dictionary

dicRemove :: Ord keyType => keyType -> Dictionary keyType item -> Dictionary keyType item
dicRemove keyToRemove dictionary = remove keyToRemove dictionary

dicRemoveIf :: Ord keyType => (keyType -> Bool) -> Dictionary keyType item  -> Dictionary keyType item
dicRemoveIf opperation dictionary = removeIf opperation dictionary