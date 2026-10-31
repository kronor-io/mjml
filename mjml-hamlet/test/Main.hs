-- | Each template must render exactly as the same document written in MJML's
-- XML syntax; each rejected template must be rejected for the right reason.
module Main (main) where

import Control.Monad (unless)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Exit (exitFailure)
import Text.Blaze (preEscapedToMarkup)

import Text.Mjml (Node, defaultOptions, defaultRegistry, endingTagNames, renderText)
import Text.Mjml.Hamlet (fromSafeUrl, mjml, renderMjml, safeUrl)
import Text.Mjml.Hamlet.Check (checkTemplate)
import Text.Mjml.Hamlet.Syntax (parseTemplate)

import Readme qualified


data Line = Line {name :: Text, quantity :: Int}


data Delivery = Pickup Text | Home


main :: IO ()
main = do
    failures <- newIORef (0 :: Int)
    let check label ok detail = unless ok $ do
            T.putStrLn ("FAIL " <> label <> "\n" <> detail)
            modifyIORef' failures (+ 1)
        same label nodes xml =
            let a = renderMjml nodes
                b = renderText defaultOptions xml
             in check label (a == b) (firstDifference (either (T.pack . show) id a) (either (T.pack . show) id b))
        rejects label template expected =
            let found = case parseTemplate template of
                    Left (_, why) -> [why]
                    Right docs -> map snd (checkTemplate defaultRegistry (endingTagNames defaultRegistry) docs)
             in check label (any (expected `T.isInfixOf`) found) ("expected an error mentioning " <> T.pack (show expected) <> ", got " <> T.pack (show found))

    let lines' = [Line "Socks" 2, Line "Hat <wool> & co" 1]
        reference = "TZ-1" :: Text
        tracking = safeUrl "https://t.example"

    same
        "structure, attributes and text"
        [mjml|
        <mj-section background-color="#f0f0f0">
          <mj-column width="70%">
            <mj-text font-size="20px">Order #{reference}
          <mj-column>
            <mj-button href="https://example.com/o?id=#{reference}" .primary>Track
        |]
        "<mjml><mj-body><mj-section background-color=\"#f0f0f0\"><mj-column width=\"70%\"><mj-text font-size=\"20px\">Order TZ-1</mj-text></mj-column><mj-column><mj-button href=\"https://example.com/o?id=TZ-1\" css-class=\"primary\">Track</mj-button></mj-column></mj-section></mj-body></mjml>"

    same
        "$forall with record fields, and escaping"
        [mjml|
        <mj-section>
          <mj-column>
            $forall line <- lines'
              <mj-text>#{line.name} × #{line.quantity}
        |]
        "<mjml><mj-body><mj-section><mj-column><mj-text>Socks × 2</mj-text><mj-text>Hat &lt;wool&gt; &amp; co × 1</mj-text></mj-column></mj-section></mj-body></mjml>"

    same
        "$if, $maybe, $case and HTML inside an ending tag"
        [mjml|
        <mj-section>
          <mj-column>
            $if null lines'
              <mj-text>Empty
            $else
              <mj-text>
                <p>Thank you!
                <p>
                  Two lines
                  joined by a space
            $maybe url <- tracking
              <mj-button href=#{url}>Track
            $nothing
              <mj-text>No tracking
            $case Pickup "Locker 4"
              $of Pickup place
                <mj-text>Collect it from #{place}
              $of Home
                <mj-text>Delivered home
        |]
        "<mjml><mj-body><mj-section><mj-column><mj-text><p>Thank you!</p><p>Two lines joined by a space</p></mj-text><mj-button href=\"https://t.example\">Track</mj-button><mj-text>Collect it from Locker 4</mj-text></mj-column></mj-section></mj-body></mjml>"

    let signature = preEscapedToMarkup ("<i>Kronor</i>" :: Text)
        footer :: [Node]
        footer =
            [mjml|
            <mj-section>
              <mj-column>
                <mj-text>^{signature}
            |]
    same
        "a whole document, with a head and embedded nodes"
        [mjml|
        <mjml lang="sv">
          <mj-head>
            <mj-title>Order #{reference}
            <mj-attributes>
              <mj-all font-family="Arial">
              <mj-class name="muted" color="#888888">
          <mj-body>
            <mj-section>
              <mj-column>
                <mj-text mj-class="muted">Hej
            ^{footer}
        |]
        "<mjml lang=\"sv\"><mj-head><mj-title>Order TZ-1</mj-title><mj-attributes><mj-all font-family=\"Arial\" /><mj-class name=\"muted\" color=\"#888888\" /></mj-attributes></mj-head><mj-body><mj-section><mj-column><mj-text mj-class=\"muted\">Hej</mj-text></mj-column></mj-section><mj-section><mj-column><mj-text><i>Kronor</i></mj-text></mj-column></mj-section></mj-body></mjml>"

    let quoteBreak = "x\" onmouseover=\"alert(1)" :: Text
        cssBreak = "red;background:url(https://t.example/p)" :: Text
        fontBreak = "Arial;color:red" :: Text
        fontOk = "'Open Sans', Arial" :: Text
        styleBreak = "color:red}body{display:none" :: Text
        safe = safeUrl "https://example.com/a?b=1&c=2"

    same
        "an interpolation after a literal scheme is percent-encoded"
        [mjml|
        <mj-section>
          <mj-column>
            <mj-button href="https://example.com/o?id=#{quoteBreak}">Go
        |]
        "<mjml><mj-body><mj-section><mj-column><mj-button href=\"https://example.com/o?id=x%22%20onmouseover=%22alert(1)\">Go</mj-button></mj-column></mj-section></mj-body></mjml>"

    same
        "typed and CSS attributes that could add declarations are dropped"
        [mjml|
        <mj-section background-color=#{cssBreak}>
          <mj-column>
            <mj-text font-family=#{fontBreak} color=#{cssBreak}>A
            <mj-text font-family=#{fontOk}>B
        |]
        "<mjml><mj-body><mj-section><mj-column><mj-text>A</mj-text><mj-text font-family=\"&#39;Open Sans&#39;, Arial\">B</mj-text></mj-column></mj-section></mj-body></mjml>"

    same
        "a SafeUrl is used whole; HTML style attributes are checked"
        [mjml|
        <mj-section>
          <mj-column>
            $maybe url <- safe
              <mj-button href=#{url}>Go
            <mj-text>
              <span style=#{styleBreak}>Hi
        |]
        "<mjml><mj-body><mj-section><mj-column><mj-button href=\"https://example.com/a?b=1&amp;c=2\">Go</mj-button><mj-text><span>Hi</span></mj-text></mj-column></mj-section></mj-body></mjml>"

    check "safeUrl accepts https" (fmap fromSafeUrl (safeUrl " https://a.example/x ") == Just "https://a.example/x") ""
    check "safeUrl rejects javascript:" (null (safeUrl "javascript:alert(1)")) ""
    check "safeUrl rejects quotes" (null (safeUrl "https://a.example/'x")) ""
    check "safeUrl rejects whitespace" (null (safeUrl "https://a.example/ x")) ""

    rejects "URL starting with an interpolation" "<mj-button href=\"#{u}/x\">" "must be a single #{...} of type SafeUrl"
    rejects "URL whose scheme an interpolation completes" "<mj-image src=\"java#{u}\">" "must be a single #{...} of type SafeUrl"
    rejects "interpolated event handler" "<mj-text>\n  <a onclick=#{x}>" "event handler attributes such as onclick"
    rejects "embed in an attribute" "<mj-text css-class=^{x}>" "cannot be used in an attribute value"
    rejects "unknown element" "<mj-sectoin>" "unknown MJML element <mj-sectoin>"
    rejects "HTML in structure" "<mj-section>\n  <div>" "<div> is HTML"
    rejects "misplaced element" "<mj-section>\n  <mj-text>Hi" "mj-text cannot be used inside mj-section"
    rejects "undeclared attribute" "<mj-text colour=\"red\">Hi" "Attribute colour is illegal on <mj-text>"
    rejects "bad enum value" "<mj-text align=\"middle\">Hi" "Attribute align has invalid value: middle"
    rejects "bad unit value" "<mj-column width=\"half\">" "Attribute width has invalid value: half"
    rejects "text outside an ending tag" "<mj-column>\n  Hello" "text can only appear inside an ending tag"
    rejects "element inside an ending tag" "<mj-text>\n  <mj-button>" "<mj-button> inside an ending tag"
    rejects "dangling $else" "$else" "$else without a matching $if"
    rejects "dangling $of" "$of Home" "$of without a matching $case"
    rejects "tabs" "\t<mj-section>" "tabs are not allowed"

    let readmeOrder =
            Readme.Order
                { Readme.reference = "TZ-1042"
                , Readme.customer = "Ada"
                , Readme.items = [Readme.Item "Wool socks" 2 "€18.00", Readme.Item "Beanie" 1 "€25.00"]
                , Readme.total = "€43.00"
                , Readme.trackingUrl = safeUrl "https://track.example/TZ-1042"
                }
        rendered = Readme.confirmation readmeOrder
        contains t = either (const False) (t `T.isInfixOf`) rendered
    check "README order confirmation renders" (all contains ["Thanks, Ada!", "2 × Wool socks", "Total €43.00", "https://track.example/TZ-1042", "mj-column-per-70"]) (T.pack (show (fmap (T.take 200) rendered)))
    check "README welcome renders" (either (const False) ("Welcome aboard" `T.isInfixOf`) Readme.renderWelcome) ""

    n <- readIORef failures
    if n == 0 then putStrLn "all mjml-hamlet tests passed" else exitFailure


-- | The two texts around where they first differ.
firstDifference :: Text -> Text -> Text
firstDifference a b =
    let i = maybe 0 (\(common, _, _) -> T.length common) (T.commonPrefixes a b)
        around t = T.take 160 (T.drop (max 0 (i - 60)) t)
     in "  template: " <> around a <> "\n  xml:      " <> around b
