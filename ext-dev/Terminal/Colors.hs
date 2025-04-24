module Terminal.Colors (
  yellow,
  green,
  cyan,
  grey
) where


-- | Wrap a string in yellow color
yellow :: String -> String
yellow str = "\ESC[33m" ++ str ++ "\ESC[0m"

-- | Wrap a string in green color
green :: String -> String
green str = "\ESC[32m" ++ str ++ "\ESC[0m"

-- | Wrap a string in cyan color
cyan :: String -> String
cyan str = "\ESC[36m" ++ str ++ "\ESC[0m"

-- | Wrap a string in grey color
grey :: String -> String
grey str = "\ESC[90m" ++ str ++ "\ESC[0m" 