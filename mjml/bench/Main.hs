-- | Rendering time for a typical email, and how it grows with the number of
-- rows and with the size of the text in a single element. Each pair of sizes
-- is 4× apart, so linear code takes about 4× as long for the larger one.
module Main (main) where

import Data.Text (Text)
import Data.Text qualified as T
import Test.Tasty.Bench

import Text.Mjml (defaultOptions, parseText, renderText)


main :: IO ()
main =
    defaultMain
        [ bench "typical order confirmation" (nf render (rows 10))
        , bgroup
            "rows"
            [ bench "100" (nf render (rows 100))
            , bench "400" (nf render (rows 400))
            ]
        , bgroup
            "text content"
            [ bench "50 KB" (nf render (bigText 50))
            , bench "200 KB" (nf render (bigText 200))
            ]
        , bgroup
            "parse only, text content"
            [ bench "50 KB" (nf parse (bigText 50))
            , bench "200 KB" (nf parse (bigText 200))
            ]
        ]
  where
    render = either (error . show) id . renderText defaultOptions
    parse = either (error . show) (const ()) . parseText defaultOptions


-- | A receipt with a header, one grouped two-column row per item, a total and
-- a button.
rows :: Int -> Text
rows n =
    T.concat $
        [ "<mjml><mj-head><mj-title>Order</mj-title><mj-attributes>"
        , "<mj-all font-family=\"Helvetica, Arial, sans-serif\" /><mj-text font-size=\"15px\" color=\"#333333\" />"
        , "</mj-attributes></mj-head><mj-body background-color=\"#f4f4f4\">"
        , "<mj-section background-color=\"#ffffff\" padding=\"24px\"><mj-column>"
        , "<mj-text font-size=\"22px\" font-weight=\"bold\">Thanks!</mj-text>"
        , "<mj-text>We have received your order.</mj-text></mj-column></mj-section>"
        ]
            <> concat
                [ [ "<mj-section background-color=\"#ffffff\" padding=\"0 24px\"><mj-group>"
                  , "<mj-column width=\"70%\"><mj-text>2 × Item number "
                  , T.pack (show i)
                  , "</mj-text></mj-column><mj-column width=\"30%\"><mj-text align=\"right\">€18.00</mj-text>"
                  , "</mj-column></mj-group></mj-section>"
                  ]
                | i <- [1 .. n]
                ]
            <> [ "<mj-section background-color=\"#ffffff\"><mj-column>"
               , "<mj-divider border-color=\"#dddddd\" border-width=\"1px\" />"
               , "<mj-text align=\"right\">Total €43.00</mj-text>"
               , "<mj-button href=\"https://example.com/track\">Track your parcel</mj-button>"
               , "</mj-column></mj-section></mj-body></mjml>"
               ]


-- | One text element holding roughly this many kilobytes of HTML.
bigText :: Int -> Text
bigText kb =
    "<mjml><mj-body><mj-section><mj-column><mj-text>"
        <> T.replicate (kb * 1024 `div` 64) "<p>Lorem ipsum <b>dolor</b> sit amet, consectetur adipiscing.</p>"
        <> "</mj-text></mj-column></mj-section></mj-body></mjml>"
