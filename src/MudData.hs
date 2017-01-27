{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}


module MudData where

import Lens.Micro
import Lens.Micro.TH

import Data.Text (Text)


type Output = Text -- outut from server

type Command = Text -- commands that player inputs

type History = [Command]

data MudState = MudState 
                { _output :: Output -- output, separaed by newline
                , _history  :: History -- command history
                , _command :: Command -- current command, possibly imcomplete
                }


makeLenses ''MudState
