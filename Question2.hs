--ex2.1
switch :: (a,a) -> (a,a)
switch (a,b) = (b,a)

left :: (a,a) -> (a,a)
left (a,b) = (a,a)

right :: (a,a) -> (a,a)
right (a,b) = (b,b)

lstrict :: (a, b) -> (a, b)
lstrict (x, y) = x `seq` (x, y)

--ex2.2
ret1 :: [b] -> Maybe b
ret1 b =
    if length b > 1 then
        Just (b !! 1)
    else
        Nothing

ret2 :: [b] -> Maybe b
ret2 b =
    if length b > 2 then
        Just (b !! 2)
    else
        Nothing

--ex2.3
func3 :: Maybe a -> (a -> Maybe b) -> Maybe b
func3 Nothing _ = Nothing
func3 (Just a) f = f a

--ex2.4
func4 :: (a -> b) -> Either c a -> Either c b
func4 _ (Left c) = Left c
func4 f (Right v) = Right $ f v

--ex2.5
func5 :: Maybe (Either a b) -> Either a (Maybe b)
func5 Nothing = Right Nothing
func5 (Just (Left a)) = Left a
func5 (Just (Right b)) = Right (Just b)

func52 :: Maybe (Either a b) -> Either a (Maybe b)
func52 Nothing = Right Nothing
func52 (Just (Left a)) = Left a
func52 (Just (Right b)) = Right Nothing

--ex2.6
func6 :: Functor f => f a -> f [a]
func6 e = fmap (\a -> [a]) e