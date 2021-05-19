module Decisions where

-- if...then...else

-- >>> message "Ivan"
-- "Adios"

-- >>> message "Juan"
-- "Hola!"

message name = if name == "Juan"
                then "Hola!"
                else "Adios"


-- Nested Ifs
-- >>> message2 "Ivan"
-- "Hola Ivan!"

-- >>> message2 "Juan"
-- "Hola!"
message2 name = if name == "Juan"
                then "Hola!"
                else if name == "Ivan"
                    then "Hola Ivan!"
                    else "Adios!"


-- Guards

-- >>> message3 "Ivan"
-- "Hola Ivan!"

-- >>> message3 "Juan"
-- "Hola!"

message3 name
    | name == "Juan" = "Hola!"
    | name == "Ivan" = "Hola Ivan!"
    | otherwise = "Adios!"

-- Case expressions

-- Common wildcards RE /[0-9].?a*/ | Linux CLI (ls a*) | Windows CMD (dir a*) * -> todo ? -> uno 

{-
>>> message4 "Hola\nMundo"
"Wow!"

>>> message4 "Ivan"
"Adios"

>>> message4 "Jvan"
"Hola Ivan!"

-}
message4 name =
    case name of
        "Jvan" -> "Hola Ivan!"
        "Juan" -> "Hola!"
        "Hola\nMundo" -> "Wow!"
        _ -> "Adios"

-- >>> message5 "Adios"
-- "Adios!"

message5 name =
    case name of
        _ -> "Adios!"
        1 -> "Hola!"
        2 -> "Hola Ivan!"

-- Parameter pattern matching

-- >>> message6 "Hola"
-- "Adios!"

-- >>> message6 "Juan"
-- "Hola!"

-- >>> message6 "Ivan"
-- "Hola Ivan!"

message6 "Juan" = "Hola!"
message6 "Ivan" = "Hola Ivan!"
message6 _ = "Adios!"

