{-# LANGUAGE TemplateHaskell #-}


module MudData where

import Lens.Micro
import Lens.Micro.TH


type Output = String -- outut from server

type Input = String -- commands that player inputs

data MudState = MudState 
                { _output :: Output -- output, separaed by newline
                , _history  :: [Input] -- command history
                , _command :: Input -- current command, possibly imcomplete
                }


makeLenses ''MudState
