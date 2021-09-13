module Exceptions where

import Data.Data (Typeable)
import Control.Exception (Exception)

newtype FetchFailedException = FetchFailedException String
    deriving (Show, Typeable)

instance Exception FetchFailedException
