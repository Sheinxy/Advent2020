-- Type definition
type String = [Char]
type BagCount = (String, Int)
type BagRule =  (String, [BagCount])
type BagRules = [BagRule] 

-- Get if a key can be found in a Key-Value pair list
hasKey :: [(String, b)] -> String -> Bool
hasKey [] _ = False
hasKey ((a, b) : d) k
  | a == k = True
  | otherwise = hasKey d k

-- Gets a value in a Key-Value pair list, given a key
getValue :: [(String, b)] -> String -> b
getValue [] _ = error "getValue: Not found"
getValue ((a, b) : d) k
  | a == k = b
  | otherwise = getValue d k

-- Checks if a bag can contain a given bag
canContain :: BagRules -> BagRule -> String -> Bool
canContain [] _ _ = False
canContain d bag k =
  let 
    _canContain :: BagRule -> Bool
    _canContain (bag, []) = bag == k
    _canContain (bag, bags)
      | hasKey bags k = True
      | otherwise = 
        any (_canContain) [(x, getValue d x) | (x, _) <- bags, hasKey d x]
  in
  _canContain bag

-- Counts how many bags can contain a given bag-key
countContain :: BagRules -> String -> Int
countContain d k =
  let
    _countContain :: Int -> BagRules -> Int
    _countContain acc [] = acc
    _countContain acc ((n, r) : l)
      | n == k = _countContain acc l
      | canContain d (n, r) k = _countContain (acc + 1) l
      | otherwise = _countContain acc l
  in
  _countContain 0 d

-- Counts how many bags a given bag-key contains
countInner :: BagRules -> String -> Int
countInner d b 
  | hasKey d b = 
    let 
      _countInner :: Int -> [BagCount] -> Int
      _countInner acc [] = acc
      _countInner acc ((bag, n) : l) 
        | hasKey d bag = 
          _countInner (acc + n * (countInner d bag + 1)) l
        | otherwise = _countInner acc l
    in
    _countInner 0 (getValue d b)
  | otherwise = 0