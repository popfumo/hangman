bubble :: [Integer] -> [Integer]
bubble [] = []
bubble [x] = [x]
bubble (x:y:xs) =
    if x > y
        then y: bubble(x:xs)
        else x: bubble(y:xs) 

bubbleSort ::  [Integer] -> [Integer]
bubbleSort [] = []
bubbleSort lst = case bubble lst of
    newList | newList == lst -> newList
            | otherwise -> bubbleSort newList 

-- Variant = length  lst 
bubbleSort2 [] = []
bubbleSort2 lst = 
    let newList = bubble lst 
    in bubbleSort2 (init newList) ++ [last newList] 

--bubbleSort2 [1,2,3]
--lst == [1,2,3]
-- x == 1
-- xs = [2,3]


--lst@(x:xs) = 
    --bubble lst 