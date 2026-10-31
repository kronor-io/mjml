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
import Text.Mjml.Hamlet (mjml, renderMjml)
import Text.Mjml.Hamlet.Check (checkTemplate)
import Text.Mjml.Hamlet.Syntax (parseTemplate)


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
             in check label (a == b) (T.pack (show a) <> "\n  vs\n" <> T.pack (show b))
        rejects label template expected =
            let found = case parseTemplate template of
                    Left (_, why) -> [why]
                    Right docs -> map snd (checkTemplate defaultRegistry (endingTagNames defaultRegistry) docs)
             in check label (any (expected `T.isInfixOf`) found) ("expected an error mentioning " <> T.pack (show expected) <> ", got " <> T.pack (show found))

    let lines' = [Line "Socks" 2, Line "Hat <wool> & co" 1]
        reference = "TZ-1" :: Text
        tracking = Just "https://t.example" :: Maybe Text

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

    n <- readIORef failures
    if n == 0 then putStrLn "all mjml-hamlet tests passed" else exitFailure
