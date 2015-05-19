-- file: ch06/JSONClass.hs

module JSONClass  where

type JSONError = String


newtype JAry a = JAry {
	fromJAry :: [a]
	} deriving (Eq, Ord, Show)

newtype JObj a = JObj {
	fromJObj :: [(String, a)]
	} deriving (Eq, Ord, Show)

class JSON a where
	toJValue :: a -> JValue
	fromJValue :: JValue -> Either JSONError a


data JValue 	= JString String
		| JNumber Double
		| JBool Bool
		| JNull
		| JObject (JObj JValue)   -- was [(String, JValue)]
		| JArray (JAry JValue)    -- was [JValue]
		deriving (Eq, Ord, Show)

---------------------------------------------------------


instance (JSON a) => JSON (JAry a) where
--	fromJValue :: (JSON a) => JValue -> Either JSONError (JAry a)

	fromJValue (JArray (JAry a)) = whenRight JAry (mapEithers fromJValue a)
	  where	whenRight :: (b -> c) -> Either a b -> Either a c
		whenRight _ (Left err) = Left err
		whenRight f (Right a) = Right (f a)
		mapEithers :: (a -> Either b c) -> [a] -> Either b [c]
		mapEithers f (x:xs) = case mapEithers f xs of
			Left err -> Left err
			Right ys -> case f x of
				Left err -> Left err
				Right y -> Right (y:ys)
		mapEithers _ _ = Right []
	fromJValue _ = Left "not a JSON array"

--	toJValue :: (JSON a) => JAry a -> JValue
	toJValue = JArray . JAry . map toJValue . fromJAry

--import Control.Arrow (second)
instance (JSON a) => JSON (JObj a) where
	toJValue = JObject . JObj . map (second toJValue) . fromJObj
	  where second :: (b -> c) -> (a, b) -> (a, c)
		second f (a,b) = (a, f b)

	fromJValue (JObject (JObj o)) = whenRight JObj (mapEithers unwrap o)
	  where unwrap (k,v) = whenRight ((,) k) (fromJValue v)
		whenRight :: (b -> c) -> Either a b -> Either a c
		whenRight _ (Left err) = Left err
		whenRight f (Right a) = Right (f a)
		mapEithers :: (a -> Either b c) -> [a] -> Either b [c]
		mapEithers f (x:xs) = case mapEithers f xs of
			Left err -> Left err
			Right ys -> case f x of
				Left err -> Left err
				Right y -> Right (y:ys)
		mapEithers _ _ = Right []

	fromJValue _ = Left "not a JSON object"
		


instance JSON JValue where
    toJValue jv = id jv 
    fromJValue jv = Right jv
