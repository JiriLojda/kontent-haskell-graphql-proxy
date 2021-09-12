module Haxl.Env (MyHaxl) where

import Haxl.Core.Monad (GenHaxl)
import Config (Config)

type MyHaxl = GenHaxl Config ()
