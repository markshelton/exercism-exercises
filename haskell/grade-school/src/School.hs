module School (School, add, empty, grade, sorted) where

import Data.Map (Map, insertWith, findWithDefault, empty, toList)
import Data.List (sortOn, sort)

type School = Map Grade [Name]
type Grade = Int
type Name = String

add :: Grade -> Name -> School -> School
add grade name = insertWith (++) grade [name]

grade :: Grade -> School -> [Name]
grade x school = sort $ findWithDefault [] x school

sorted :: School -> [(Grade, [Name])]
sorted s = sortOn fst $ zip outer_keys inner_keys
    where 
        s' = toList s
        outer_keys = map fst s'
        inner_keys = map (sort . snd) s'

