module InputOutput where


class Input a where
  input :: a -> IO String
  parseMaybe :: Read b => a -> (String -> Maybe b)

class Output a where
  output :: a -> (String -> IO ())


