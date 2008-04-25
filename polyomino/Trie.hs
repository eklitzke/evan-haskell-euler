-- Trie.hs

module Trie where

import RedBlackTree

-- loosely based on the implementation given by Okasaki

data Trie key value = Trie (Maybe value) (RedBlackTree (key, Trie key value)) deriving Show

trempty = Trie Nothing rbempty

trlookup (Trie v t) [] = v
trlookup (Trie v t) (k:ks) =
	case (t `rblookup` k) of
	Nothing -> Nothing
	Just t' -> t' `trlookup` ks

trupdate (Trie _ t) ([],v) = Trie (Just v) t
trupdate (Trie u t) (k:ks,v) =
	let
		t' = case (t `rblookup` k) of
			Just t -> t
			Nothing -> trempty
		t'' = trupdate t' (ks,v)
	in Trie u (t `rbupdate` (k,t''))

trcount (Trie v t) =
	let
		self = case v of
			Just _ -> 1
			Nothing -> 0
		subtrees = rbtolist t
		subcounts = map (\(_,s)-> trcount s) subtrees
	in self + sum subcounts

trfromlist kvs = foldl trupdate trempty kvs

trtolist :: Trie k v -> [([k],v)]
trtolist t = dotrtolist t []
	where
		dotrtolist (Trie v t) ks =
			let
				entry = case v of
					Just v' -> [(reverse ks,v')]
					Nothing -> []
				subtrees = rbtolist t
				sublists = map (\(k,s)-> dotrtolist s (k:ks)) subtrees
			in entry ++ (concat sublists)

