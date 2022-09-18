{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Monad
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Maybe
import Data.List
import Data.String
import GHC.Exts

data Expr = Paren Expr
          | Tuple [Expr]
          | Function String [Expr]
          | Operator String Expr Expr
          | Expr String
          | Exprs [Expr]
          | Source String [String]
          | Value Expr String -- Record Value
v = Value

instance IsString Expr where
  fromString str = case str of
    _ -> Expr str

instance IsList Expr where
  type Item Expr = Expr
  fromList = Exprs
  toList a = [a]

instance Num Expr where
  (+) = Operator "+"
  (-) = Operator "-"
  (*) = Operator "*"
  negate x = Paren $ Exprs ["-", x]
  abs x = Function "abs" [x]
  signum x = iif (x `eq_` "0") "0"
           $ iif (x `more_` "0") "1" "-1"
  fromInteger = Expr . show

{- NOTE to self: DO NOT use deicmal numbers, show decimals as text, use "0.5" not 0.5 in calculations -}

instance Fractional Expr where
  (/) = Operator "/"
  recip x = "1" / x
  fromRational = Expr . show

render :: Expr -> String
render expr = case expr of
  Paren e -> "(" ++ render e ++ ")"
  Tuple as -> paren $ concat $ intersperse ", " $ render <$> as
  Function name params -> name ++ render (Tuple params)
  Operator name a b -> paren $ render $ [a, " ", Expr name, " ", b]
  Expr e -> e
  Exprs exprs -> render =<< exprs
  Source source sources -> (source ++) $ sources >>= \source -> ":GetSourceTool" ++ paren ("\"" ++ source ++ "\"")
  Value e str -> render e ++ "." ++ str

getSource = getSource_ Nothing
getSource_ Nothing = Source "self"
getSource_ (Just source) = Source source


paren a = "(" ++ a ++ ")"
iif bool a b = Function "iif" [bool, a, b]
point a b = Function "Point" [a,b]
max_ a b = Function "max" [a,b]
min_ a b = Function "min" [a,b]
cos_ e = Function "cos" [e]
floor_ a = Function "floor" [a]
negate_ a = Paren $ [" - ", a]
eq_ = Operator "=="
exp_ = Operator "^"
more_ = Operator ">"
moreE_ = Operator ">="
less_ = Operator "<"
lessE_ = Operator "<="

ifOff x = iif [x, "== 0"]

-- inputs should br between 0 and 1
-- slow start, fast mid, slow end
smooth x = (cos_ ("pi" * x - "pi") + "1") / "2"

-- exponential growth
-- inputs should br between 0 and 1
slowFast = flip exp_
-- slowFast 1 = consistent
-- exp_ 2 = slow start, fast end
-- ...
-- exp_ 6 = very slow start, very fast end
fastSlow = flip exp_ . recip
-- fastSlow 1 = consistent
-- fastSlow 2 = fast start, slow end
-- ...
-- fastSlow 6 = very fast start, very slow end

distanceToStart = "time" - "comp.RenderStart" :: Expr
distanceToEnd = "comp.RenderEnd" - "time" :: Expr
frameRate = "comp:GetPrefs(\"Comp.FrameFormat.Rate\")"

timeControl onOff start end duration' sizeConst' =
  let duration = floor_ $ (frameRate * duration') / "1000" --["RC.FrameRate * ", duration', "/ 1000"]
      sizeConst = fromMaybe "1" sizeConst'
      sideWrap rc side = iif ((rc - side) `lessE_` duration)
                             (sizeConst * (rc - side) / duration)
      leftSide = sideWrap distanceToStart start
      rightSide = sideWrap distanceToEnd (negate end)
  in iif [onOff, " == 0"]
         "0"
       $ iif [distanceToStart `less_` start, " or ", distanceToEnd `less_` negate end]
             "0"
           $ leftSide
               $ rightSide
               $ sizeConst


--------------------------------------------------------------------------------
-- 2.0

-- Zoom Effect
--maskBorderCenter = Source ["Output", "Foreground", "EffectMask"] `v` "Center" -- self:GetSourceTool("Mask"):GetSourceTool("Foreground"):GetSourceTool("EffectMask").Center
--maskBorderWidth =
--maskBorderHeight =

inZoom = 1
inScale = 1 -- 0 to 1

zoomInPivot = render $
  let focus axis min max = max_ min $ min_ max $ axis
      mask = getSource ["EffectMask"]
      maskMin axis axisControl = (mask `v` "Center" `v` axis) - ((mask `v` axisControl) / "2")
      maskMax axis axisControl = (mask `v` "Center" `v` axis) + ((mask `v` axisControl) / "2")
      x = focus "Focus.X" (maskMin "X" "Width")  (maskMax "X" "Width")
      y = focus "Focus.Y" (maskMin "Y" "Height") (maskMax "Y" "Height")
  in iif ("InZoom == 0") (join point $ "0.5") -- Off
         -- On
       $ point x y
         --on point (\(focusPoint, center) -> focusPoint)
         --         (x, mask `v` "Center" `v` "X")
         --         (y, mask `v` "Center" `v` "Y")

zoomInSize = render $
  let useLongAxis = iif $ ("ContentLength" `v` "X") `more_` ("ContentLength" `v` "Y") -- if true use width, else use height
      xScale = ("BorderLength" `v` "X") /  ("ContentLength" `v` "X")
      yScale = ("BorderLength" `v` "Y") / ("ContentLength" `v` "Y")
  in iif ("InZoom == 0") "1" -- Off
         -- On
       $ "1"
       + ("Scale" - "1")
       * smooth (timeControl "InZoom" "Start" "End" "Duration" Nothing)


--
outZoom = 1
outScale = 1 -- 0 to 1,

zoomOutPivot = render $
  let borderMask        = getSource ["EffectMask"]
      contentBorderMask = getSource ["Input", "EffectMask"]
      --leftHalf  c w = c - w / "2"
      --rightHalf c w = c + w / "2"
      mkAxis axis = -- center length =
        let contentCenter_ = "ContentCenter" `v` axis --contentCenter = contentBorderMask `v` "Center" `v` center
            contentLength_ = "ContentLength" `v` axis --contentLength = contentBorderMask `v` length
            borderCenter_  = "BorderCenter"  `v` axis  --borderCenter =  borderMask        `v` "Center" `v` center
            borderLength_  = "BorderLength"  `v` axis --borderLength =  borderMask        `v` length
            --contentLeft  = contentCenter_ - contentLength_ / "2"
            --contentRight = contentCenter_ + contentLength_ / "2"
            borderLeft   = borderCenter_  - borderLength_  / "2"
            --borderRight  = borderCenter_  + borderLength_  / "2"
            sizeChange = borderLength_ / contentLength_
        in ((sizeChange * contentCenter_ - "0.5") / (sizeChange - "1"))
           -- * borderLength_ - borderLeft
           -- this is also also wrong
           --iif (borderCenter_ `more_` contentCenter_)
           --    {-left zoom-}
           --    ((contentCenter_ * ((contentLeft - borderLeft) / (borderCenter_ - contentLeft))) + borderLeft)
           --    {-right zoom-}
           --    (((contentCenter_ - 0.5) * ((borderCenter_ - contentRight) / (contentRight - borderRight))) + contentCenter_)
               -- this is also wrong
               --{-left zoom-}
               --((borderCenter_ - contentCenter_) * (contentLeft - borderLeft) / (contentCenter_ - borderLeft))
               --{-right zoom-}
               --((contentCenter_ - borderCenter_) * (borderRight - contentRight) / (borderRight - contentCenter_))
        -- this is all wrong
        --    contentCenter_ * (contentLength_ + "1") - (borderCenter_ * contentLength_)
        -- + signum (contentCenter_ - borderCenter_)
        --   * (contentLength_ * (contentLength_ - borderLength_ + "1") / "2")
           --("0.5" + (contentCenter - "0.5") / ("1" - contentLength))
             --(contentCenter `add_` (contentLength `times_` (contentLength `div_` "2")))
             --((contentLength {-`times_` rightHalf contentCenter contentLength-}) `add_` leftHalf contentCenter contentLength)
           -- * borderLength + borderCenter - borderLength / "2"
      xAxis = mkAxis "X" --"Width"
      yAxis = mkAxis "Y" --"Height"
  in point xAxis yAxis

zoomOutSize = render $
  let useLongAxis = iif $ ("ContentLength" `v` "X") `more_` ("ContentLength" `v` "Y") -- if true use width, else use height
      xScale = ("BorderLength" `v` "X") / ("ContentLength" `v` "X")
      yScale = ("BorderLength" `v` "Y") / ("ContentLength" `v` "Y")
  in ifOff "OutZoom" "1" -- Off
         -- On
       $ "1"
       + (useLongAxis xScale yScale - "1")
       * "Scale"
       * smooth (timeControl "OutZoom" "Start" "End" "Duration" Nothing)
-- not using anim curves
--zoomOutScale = render $
--  let borderMask        = Source "OutTransform" ["EffectMask"]
--      contentBorderMask = Source "OutTransform" ["Input", "EffectMask"]
--      useLongAxis = iif $ (contentBorderMask `v` "Width") `more_` (contentBorderMask `v` "Height") -- if true use width, else use height
--      xScale = (borderMask `v` "Width") /  (contentBorderMask `v` "Width")
--      yScale = (borderMask `v` "Height") / (contentBorderMask `v` "Height")
--  in iif ("OutZoom == 0") "0" -- Off
--         -- On
--       $ useLongAxis xScale yScale - 1



-- Expression Character Limit = 1023, theese values exist to get past this barrier
contentCenter = render $        getSource ["Input", "EffectMask"] `v` "Center"
contentLength = render $ point (getSource ["Input", "EffectMask"] `v` "Width")
                               (getSource ["Input", "EffectMask"] `v` "Height")
borderCenter =  render $        getSource ["Input", "Background", "EffectMask"] `v` "Center"
borderLength =  render $ point (getSource ["Input", "Background", "EffectMask"] `v` "Width")
                               (getSource ["Input", "Background", "EffectMask"] `v` "Height")



--
slideCenter = render $
  let mkAxis :: String -> Expr -> Expr -> Expr
      mkAxis axis onOff fromOpposite =
        let contentCenter_ = getSource ["Input"] `v` "ContentCenter" `v` axis --"ContentCenter" `v` axis
            contentLength_ = getSource ["Input"] `v` "ContentLength" `v` axis --"ContentLength" `v` axis
            outZoomSize = getSource ["Input"] `v` "Size"
            sig = negate (fromOpposite * "2" - "1")
            -- content side picks the oppposite side of the mask, fromLeft == right mask side
            slideDistance = contentCenter_ + sig * (fromOpposite + contentLength_ * (outZoomSize - "0.5"))
        in ifOff onOff "0.5"
             $ --"0.5"contentSide fromOpposite
               "0.5"
             + slideDistance
             * (Paren (smooth "TimeControl") - "1")
  in ifOff "Slide" (point "0.5" "0.5")
       $ point (mkAxis "X" "Horizontal" "FromRight")
               (mkAxis "Y" "Vertical"   "FromUp")

-- this is here as a temporary fix to a bug
-- if any media is masked and that mask is moved outside of view, everything that
-- was supposed to be removed by the mask will be shown
-- the fix is to reduce the size of that masked media when it is moved out of view
-- so that it cannot be shown
slideSize = render $ iif ["Slide == 0 or TimeControl > 0"] "1" "0"

slideTimeControl = render $ timeControl "Slide" "Enter" "Leave" "Duration" Nothing


--------------------------------------------------------------------------------

putGroup :: String -> [String] -> IO ()
putGroup title values = do
  putStrLn $ title ++ "\n"
  traverse putStrLn values
  putStrLn "\n\n"

main = do
  putGroup "Zoom Effect"
    ["zoomInPivot: "   ++ zoomInPivot
    ,"zoomInSize: "    ++ zoomInSize
    ,""
    ,"zoomOutPivot: "  ++ zoomOutPivot
    ,"zoomOutSize: "   ++ zoomOutSize
    ,"contentCenter: " ++ contentCenter
    ,"contentLength: " ++ contentLength
    ,"borderCenter: "  ++ borderCenter
    ,"borderLength: "  ++ borderLength
    ,""
    ,"slideCenter: "        ++ slideCenter
    ,"slideSize: "          ++ slideSize
    ,"slideTimeControl: "   ++ slideTimeControl]

  putGroup "Frame Effect"
    []










--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- 1.0


{-
renderEC onOff start end' duration' sizeConst' =
  let end = negate_ end'
      duration = floor_ $ div_ (times_ frameRate duration') "1000" --["RC.FrameRate * ", duration', "/ 1000"]
      sizeConst = fromMaybe "1" sizeConst'
      sideWrap rc side = iif [rc, " - ", side, " <= ", duration]
                             [sizeConst, " * (", rc, " - ", side, ") / ", duration]
      leftSide = sideWrap distanceToStart start
      rightSide = sideWrap distanceToEnd end
  in iif [onOff, " == 0"]
         "0"
       $ iif [distanceToStart, " < ", start, " or ", distanceToEnd, " < ", end]
             "0"
           $ leftSide
               $ rightSide
               $ sizeConst




--------------------------------------------------------------------------------
-- Ease Control




--------------------------------------------------------------------------------
-- Cam
-- Expand Pivot (0.01089, 0.98082)
-- Mask (x = 0.01046 to 0.18583, y = 0.66982 to 0.98160)
camCenter = render
  $ flip point "0.5"
  $ iif (eq_ "1" $ smooth camSlideControl) "-0.5"
  $ minus_ "0.5"
  $ times_"0.19"
  $ smooth camSlideControl
camSize = render
  $ add_ "1"
  $ times_ "0.75"
  $ smooth camZoomControl


camZoom = True
camZoomStart = 0
camZoomEnd = 0
camSlide = True
camSlideLeave = 0
camSlideReturn = 0

camZoomControl = easeControl "CamZoom" "CamZoomStart" "CamZoomEnd" "CamZoomDuration" Nothing
camSlideControl = easeControl "CamSlide" "CamSlideLeave" "CamSlideReturn" "CamSlideDuration" $ Just "Size"

camZoomDuration = 134
camSlideDuration = 134




--------------------------------------------------------------------------------
-- Chat Frame
-- Expand Pivot (0.01043, 0.01836)
-- Mask (x = 0.01036 to 0.18576, y = 0.01829 to 0.65112)
chatFrameCenter = render
  $ flip point "0.5"
  $ minus_ "0.5"
  $ times_"0.28"
  $ Paren
  $ minus_ "1"
  $ smooth chatFrameSlideControl

chatFrame = 0
chatFrameSlideEnter = 0
chatFrameSliderLeave = 0

chatFrameSlideControl = easeControl "ChatFrameSlide" "ChatFrameSlideEnter" "ChatFrameSlideLeave" "ChatFrameSlideDuration" Nothing

chatFrameSlideDuration = 134



--------------------------------------------------------------------------------
-- Game In
-- Borderless Mask (x = 0.20198 to 0.98555, y = 0.19100 to 0.97430)
gameInPivot = render $
  let focus point min max = max_ min $ min_ max $ point
      x = focus "GameInFocus.X" "0.20198" "0.98555"
      y = focus "GameInFocus.Y" "0.19100" "0.97430"
  in iif ("GameInZoom == 0")
         (join point $ "0.5")
       $ on point (\(focusPoint, center) -> focusPoint --add_ center $ times_ "GameInZoomControl" $ minus_ focusPoint center
                                            --[center, " + ", Paren [focusPoint, " - ", center], " * GameInZoomControl"]
                                            )
                  (x, "0.59336")
                  (y, "0.58191")

gameInFocus = (0.59336, 0.58191)
gameInSize = render
  $ iif "GameInZoom == 0" "1"
  $ add_ "1"
  $ times_ gameInZoomControl
  $ Paren
  $ minus_ "1" (smooth "GameInZoomSize")


gameInZoom = True
gameInZoomStart = 0
gameInZoomEnd = 0

gameInZoomControl = easeControl "GameInZoom" "GameInZoomStart" "GameInZoomEnd" "GameInZoomDuration" Nothing

gameInZoomDuration = 134



--------------------------------------------------------------------------------
-- Game Out
-- Border Mask (x = 0.19699 to 0.98973, y = 0.18232 to 0.98150)

gameOutZoom = True
gameOutZoomStart = 0
gameOutZoomEnd = 0

gameOutZoomControl = easeControl "GameOutZoom" "GameOutZoomStart" "GameOutZoomEnd" "GameOutZoomDuration" Nothing

gameOutZoomDuration = 134



--------------------------------------------------------------------------------
-- Game Frame
gameFrameCenter = render
  $ flip point "0.5"
  $ add_ "0.5"
  $ times_"0.79"
  $ Paren
  $ minus_ "1"
  $ smooth gameFrameSlideControl

gameFrameSlide = True
gameFrameSlideEnter = 0
gameFrameSlideLeave = 0

gameFrameSlideControl = easeControl "GameFrame" "GameFrameSlideEnter" "GameFrameSlideLeave" "GameFrameSlideDuration" Nothing

gameFrameSlideDuration = 134



--------------------------------------------------------------------------------

putGroup1 :: String -> [String] -> IO ()
putGroup1 title values = do
  putStrLn $ title ++ "\n"
  traverse putStrLn values
  putStrLn "\n\n"

main1 = do
  putGroup "Cam"
   ["camCenter: " ++ camCenter
   ,"camSize: " ++ camSize
   ,"camZoomControl: " ++ camZoomControl
   ,"camSlideControl: " ++ camSlideControl
   ]

  putGroup "Chat Frame"
   ["chatFrameSlideControl: " ++ chatFrameSlideControl
   ]

  putGroup "Game In"
   ["gameInPivot: " ++ gameInPivot
   ,"gameInSize: " ++ gameInSize
   ,"gameInZoomControl: " ++ gameInZoomControl
   ]

  putGroup "Game Out"
   ["gameOutZoomControl: " ++ gameOutZoomControl
   ]

  putGroup "Game Frame"
   ["gameFrameControl: " ++ gameFrameSlideControl
   ]


--iif(GameInZoom == 0
--  , Point(0.5, 0.5)
--  , Point((0.59336 + (GameInZoomControl * (max(0.20198, min(0.98555, GameInFocus.X)) - 0.59336)))
--        , (0.58191 + (GameInZoomControl * (max(0.19100, min(0.97430, GameInFocus.Y)) - 0.58191))))
--   )

Point((((ContentCenter.X * (ContentLength.X + 1)) - (BorderCenter.X * ContentLength.X)) + (iif(((ContentCenter.X - BorderCenter.X) == 0), 0, iif(((ContentCenter.X - BorderCenter.X) > 0), 1, -1)) * ((ContentLength.X * ((ContentLength.X - BorderLength.X) + 1)) / 2))), (((ContentCenter.Y * (ContentLength.Y + 1)) - (BorderCenter.Y * ContentLength.Y)) + (iif(((ContentCenter.Y - BorderCenter.Y) == 0), 0, iif(((ContentCenter.Y - BorderCenter.Y) > 0), 1, -1)) * ((ContentLength.Y * ((ContentLength.Y - BorderLength.Y) + 1)) / 2))))


-}
