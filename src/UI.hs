{-# LANGUAGE OverloadedStrings #-}

module UI where


import Control.Monad.State

import UI.HSCurses.Logging
import UI.HSCurses.Widgets
import qualified UI.HSCurses.Curses as Curses
import qualified UI.HSCurses.CursesHelper as CursesH

title = "mud"
separator = "input"
help = "Ctrl^D :quit"


type Output = String

data MudState = MudState 
     { ui_styles :: [CursesH.CursesStyle]
     , mud_output :: Output
     }

type Mud = StateT MudState IO
  
runMud :: [CursesH.CursesStyle] -> Output -> Mud a -> IO a
runMud stys output mud = evalStateT mud (MudState { ui_styles = stys
                                                  , mud_output = output})

nthStyle :: Int -> Mud CursesH.CursesStyle
nthStyle n = do
         cs <- gets ui_styles
         return $ cs !! n

getSize = liftIO $ Curses.scrSize

styles = [ CursesH.defaultStyle 
         , CursesH.AttributeStyle [CursesH.Bold] CursesH.GreenF CursesH.DarkBlueB
         ]

defStyle = nthStyle 0
lineStyle = nthStyle 1

lineDrawingStyle =
    do sty <- lineStyle
       return $ mkDrawingStyle sty

lineOptions =
    do sz <- getSize
       ds <- lineDrawingStyle
       return $ TWOptions { twopt_size = TWSizeFixed (1, getWidth sz)
                          , twopt_style = ds
                          , twopt_halign = AlignLeft }


type ToplineWidget = TextWidget
type MidlineWidget = TextWidget
type BotlineWidget = TextWidget
type MsglineWidget = TableWidget
type OutputWidget = TextWidget
type InputWidget = TextWidget


mkToplineWidget =
    do opts <- lineOptions
       return $ newTextWidget (opts { twopt_halign = AlignCenter }) title


mkMidlineWidget =
    do opts <- lineOptions
       return $ newTextWidget (opts { twopt_halign = AlignCenter }) separator


mkBotlineWidget = 
    do opts <- lineOptions 
       return $ newTextWidget opts help


-- We need to insert a dummy widget at the lower-right corner of the window,
-- i.e. at the lower-right corner of the message line. Otherwise, an
-- error occurs because drawing a character to this position moves the
-- cursor to the next line, which doesn't exist.
mkMsglineWidget =
     do sz <- getSize
        let width = getWidth sz
            opts = TWOptions { twopt_size = TWSizeFixed (1, width - 1)
                              , twopt_style = defaultDrawingStyle
                              , twopt_halign = AlignLeft }
            tw = newTextWidget opts "msgline"
            row = [TableCell tw, TableCell $ EmptyWidget (1,1)]
            tabOpts = defaultTBWOptions { tbwopt_minSize = (1, width) }
        return $ newTableWidget tabOpts [row]

nlines = 4

outputHeight (h, _)= h-nlines
inputHeight = 1

outputOptions =
    do sz <- getSize
       return $ TWOptions { twopt_size = TWSizeFixed ( outputHeight sz
                                                     , getWidth sz)
                          , twopt_style = defaultDrawingStyle
                          , twopt_halign = AlignLeft }

inputOptions =
    do sz <- getSize
       return $ TWOptions { twopt_size = TWSizeFixed ( inputHeight
                                                     , getWidth sz)
                          , twopt_style = defaultDrawingStyle
                          , twopt_halign = AlignLeft }

mkOutputWidget :: Mud OutputWidget
mkOutputWidget = 
    do output <- gets mud_output
       return $ newTextWidget defaultTWOptions output

mkInputWidget :: Mud InputWidget
mkInputWidget =
     return $ newTextWidget defaultTWOptions []

resize :: Widget w => Mud w -> Mud ()
resize f =
    do liftIO $ do Curses.endWin
                   Curses.resetParams
                   Curses.cursSet Curses.CursorInvisible
                   Curses.refresh
       w <- f
       redraw w
       eventloop w

redraw :: Widget w => w -> Mud ()
redraw w =
    do sz <- getSize
       liftIO $ draw (0, 0) sz DHNormal w
       liftIO $ Curses.refresh

eventloop w =
   do k <- CursesH.getKey (resize mkMainWidget)
      debug ("Got key" ++ show k)
      case k of
           Curses.KeyUp -> return ()
           _             -> eventloop w
 
data MainWidget = MainWidget
     { toplineWidget :: ToplineWidget
     , outputWidget  :: OutputWidget
     , midlineWidget :: MidlineWidget
     , inputWidget   :: InputWidget
     , botlineWidget :: BotlineWidget 
     , msglineWidget :: MsglineWidget}

instance Widget MainWidget where
     draw pos sz hint w = draw pos sz hint (mkRealMainWidget (Just sz) w)
     minSize w = minSize (mkRealMainWidget Nothing w)

mkRealMainWidget msz w =
    let cells = [ TableCell $ toplineWidget w
                , TableCell $ outputWidget w
                , TableCell $ midlineWidget w
                , TableCell $ inputWidget w
                , TableCell $ botlineWidget w
                , TableCell $ msglineWidget w ]
        rows = map singletonRow cells
        opts = case msz of
                 Nothing -> defaultTBWOptions
                 Just sz -> defaultTBWOptions { tbwopt_minSize = sz }
        in newTableWidget opts rows

mkMainWidget =
    do tlw <- mkToplineWidget
       opw <- mkOutputWidget
       mlw <- mkMidlineWidget
       ipw <- mkInputWidget
       blw <- mkBotlineWidget
       msglw <- mkMsglineWidget
       return $ MainWidget tlw opw mlw ipw blw msglw 



mudMain :: Mud ()
mudMain =
   do w <- mkMainWidget
      redraw w

