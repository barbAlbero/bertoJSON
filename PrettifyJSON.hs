-- file: PrettifyJSON.hs
	module PrettifyJSON where
		import Data.List
--		import SimpleJSON
--		import PutJSON
		import JSONClass

		prettyIO :: JValue -> String
		prettyIO v =  (intercalate "\n" (pretty v))
--		type text = [String]

		pretty :: JValue -> [String]
		pretty (JString s)   = [show s]
		pretty (JNumber n)   = [show n]
		pretty (JBool True)  = ["true"]
		pretty (JBool False) = ["false"]
		pretty JNull         = ["null"]

		pretty (JArray a) =  
		  case fromJAry a of 
			[] -> ["[","]"]
			(a:as) ->  ["["] ++ (valuehead a) ++ (valuetail as) ++ ["]"]
--	mi è sembrato di capire che non dovrei fare pattern maching data la particolare costruzione di JAry
		  where valuehead x  = (map ("\t"++) (pretty x))
		       	valuetail [] = []
		       	valuetail xs = concat (map ( (headAppend ",") . (map ("\t"++)) . pretty) xs)
			  where headAppend s (x:xs) =  (s ++ x):xs  

--		pretty (JObject []) = ["{","}"]
--		pretty (JObject cs@(c:cs')) = ["{"] ++ (adjustfirstline 1 " " (values cs) ) ++ ["}"]
		pretty (JObject o) = 
		  case fromJObj o of 
			[] -> ["{","}"]
			os -> ["{"] ++ (adjustfirstline 1 " " (values os) ) ++ ["}"]
		  where adjustfirstline :: Int -> String -> [String] -> [String]
		        adjustfirstline n s [] = [s] 
		        adjustfirstline n s (x:xs) = (s++(drop n x)):xs
		        values  :: [(String, JValue)] -> [String]
		        values  abs   = concat ( map valll abs )
		          where valll :: (String, JValue) -> [String]
		                valll ab =  (adjustfirstline 0 (","++(show (fst ab)++":" ))  (((map ("\t"++)). pretty) (snd ab) ) )  
	            
		        
		        
		        
		        
		        
		        
  

