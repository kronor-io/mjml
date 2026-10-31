-- | The examples in the README, compiled and run by the test suite so they
-- stay correct. Keep the two in sync.
module Readme (
    Order (..),
    Item (..),
    confirmation,
    welcome,
    renderWelcome,
) where

import Data.Text (Text)

import Text.Mjml (MjmlError, Node, defaultOptions, renderText)
import Text.Mjml.Hamlet (SafeUrl, mjml, renderMjml)


-- README: "Writing templates in Haskell"

data Order = Order
    { reference :: Text
    , customer :: Text
    , items :: [Item]
    , total :: Text
    , trackingUrl :: Maybe SafeUrl
    }


data Item = Item
    { name :: Text
    , quantity :: Int
    , price :: Text
    }


confirmation :: Order -> Either MjmlError Text
confirmation order =
    renderMjml
        [mjml|
        <mjml>
          <mj-head>
            <mj-title>Order #{order.reference}
            <mj-preview>Thanks for your order, #{order.customer}
            <mj-attributes>
              <mj-all font-family="Helvetica, Arial, sans-serif">
              <mj-text font-size="15px" line-height="22px" color="#333333">
          <mj-body background-color="#f4f4f4">
            <mj-section padding="12px">
            <mj-section background-color="#ffffff" padding="24px">
              <mj-column>
                <mj-text font-size="22px" font-weight="bold">Thanks, #{order.customer}!
                <mj-text>We have received order #{order.reference} and will tell you when it ships.
            $forall item <- order.items
              <mj-section background-color="#ffffff" padding="0 24px">
                <mj-group>
                  <mj-column width="70%">
                    <mj-text>#{item.quantity} × #{item.name}
                  <mj-column width="30%">
                    <mj-text align="right">#{item.price}
            <mj-section background-color="#ffffff" padding="0 24px 24px">
              <mj-column>
                <mj-divider border-color="#dddddd" border-width="1px">
                <mj-text align="right" font-weight="bold">Total #{order.total}
                $maybe url <- order.trackingUrl
                  <mj-button href=#{url} background-color="#1a73e8">Track your parcel
            ^{footer}
        |]


footer :: [Node]
footer =
    [mjml|
    <mj-section>
      <mj-column>
        <mj-text font-size="12px" color="#888888" align="center">
          You are receiving this email because you placed an order with us.
    |]


-- README: "Rendering MJML documents"

welcome :: Text
welcome =
    "<mjml>\
    \  <mj-body>\
    \    <mj-section>\
    \      <mj-column>\
    \        <mj-image src=\"https://example.com/logo.png\" width=\"120px\" />\
    \        <mj-text font-size=\"20px\">Welcome aboard</mj-text>\
    \        <mj-button href=\"https://example.com/start\">Get started</mj-button>\
    \      </mj-column>\
    \    </mj-section>\
    \  </mj-body>\
    \</mjml>"


renderWelcome :: Either MjmlError Text
renderWelcome = renderText defaultOptions welcome
