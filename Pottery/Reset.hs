{-|

A minimal reset.

-}

{-# LANGUAGE OverloadedStrings #-}

module Pottery.Reset
       ( reset
       ) where

import Clay hiding  (em)
import Clay.Elements(em)
import Control.Monad
import Prelude hiding (div, span)

-- | The reset style.
reset :: Css
reset = do
  -- comon stuff
  commonReset

  -- Setting colours
  body ? do
    background white
    color black

  basicTypography
  tableReset

-- | Reset common selectors.
commonReset :: Css
commonReset = forM_ [ html, body, div, span, "applet", object, iframe
                    , h1, h2, h3, h4, h5, h6, p, blockquote, pre
                    , a, abbr, "acronym", address, "big", cite, code
                    , del, dfn, em, img, ins, kbd, q, s, samp
                    , small, "strike", strong, sub, sup, "tt", var
                    , b, u, i, "center"
                    , dl, dt, dd, ol, ul, li
                    , fieldset, form, label, legend
                    , table, caption, tbody, tfoot, thead, tr, th, td
                    , article, aside, canvas, details, embed
                    , figure, figcaption, footer, header, hgroup
                    , menu, nav, output, ruby, section, summary
                    , time, mark, audio, video
                    ] setCommons
   where setCommons :: Selector -> Css
         setCommons sel = sel ? do
           -- getrid of margin, padding, border
           margin  0 0 0 0
           padding 0 0 0 0
           borderStyle none

           -- Some font hacks.
           fontSize   normal
           "vertical-align" -: "baseline"

-- | Set bold, strong, etc.
basicTypography :: Css
basicTypography = do
  -- Set the italic fonts
  i ? fontStyle italic

  -- bold text
  b ? fontWeight bold

  -- strong and nested strong
  strong ? do fontWeight bold
              strong <? do fontStyle italic
                           strong <? fontStyle normal

  -- emphasis is obtained by italic fonts but nested emphasis should
  -- go back to normal font.
  em ? do fontStyle italic
          em <? do fontStyle normal
                   em <? fontStyle normal

  -- stuff in small font should be small
  small ? do fontSize $ pct 80

  -- subscripts and super scripts.
  forM_ [sub, sup] $ \ sel -> sel ? do
    fontSize       $ pct 75
    lineHeight     $ px 0
    position       relative

-- | Reseting tables
tableReset :: Css
tableReset =  table ? do "border-collapse" -: "collapse"
                         "border-spacing"  -: "0"
