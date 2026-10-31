-- | What the quasiquoter rejects before generating any code: the checks
-- MJML's validator makes, done at compile time.
--
-- Every element must be an MJML element, sit inside a parent that accepts it,
-- and carry only attributes it declares; an attribute written as a literal
-- must also have a value of the declared type. HTML is only allowed inside
-- ending tags such as @mj-text@, and text only there too.
module Text.Mjml.Hamlet.Check (
    checkTemplate,
) where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

import Text.Mjml (Registry)
import Text.Mjml.Hamlet.Runtime (safeUrlPrefix, urlAttributes)
import Text.Mjml.Hamlet.Syntax
import Text.Mjml.Schema (checkAttribute, checkChild, knownTag)


-- | Every problem found, in template order.
checkTemplate :: Registry -> Set Text -> [Doc] -> [(Pos, Text)]
checkTemplate reg ending = concatMap (structure Nothing)
  where
    -- Inside MJML structure: the parent, if there is one in the template.
    structure :: Maybe Text -> Doc -> [(Pos, Text)]
    structure parent = \case
        DTag p name as kids
            | not (knownTag reg name) ->
                [ ( p
                  , if "mj" `T.isPrefixOf` name
                        then "unknown MJML element <" <> name <> ">"
                        else "<" <> name <> "> is HTML, which can only appear inside an ending tag such as <mj-text>, <mj-button> or <mj-raw>"
                  )
                ]
            | otherwise ->
                placement p parent name
                    <> concatMap (attribute name) as
                    <> if
                        | name `Set.member` ending -> concatMap html kids
                        | name `elem` ["mj-attributes", "mj-class"] -> []
                        | otherwise -> concatMap (structure (Just name)) kids
        DText p _ ->
            [ ( p
              , "text can only appear inside an ending tag such as <mj-text>"
                    <> maybe "" (\n -> ", not directly inside <" <> n <> ">") parent
              )
            ]
        DEmbed _ _ -> []
        DForall _ _ _ kids -> concatMap (structure parent) kids
        DWith _ _ _ kids -> concatMap (structure parent) kids
        DIf _ branches final ->
            concat [concatMap (structure parent) ks | (_, _, ks) <- branches] <> maybe [] (concatMap (structure parent)) final
        DMaybe _ _ _ kids nothing -> concatMap (structure parent) kids <> maybe [] (concatMap (structure parent)) nothing
        DCase _ _ alts -> concat [concatMap (structure parent) ks | (_, _, ks) <- alts]
        DOf p _ _ -> [(p, "$of without a matching $case")]

    placement p parent child = case parent of
        Just par | Just why <- checkChild reg par child -> [(p, why)]
        _ -> []

    attribute tag = \case
        Attr p attrName value ->
            maybe [] (\why -> [(p, why)]) (checkAttribute reg tag attrName (value >>= literal))
                <> interpolation p attrName (maybe [] id value)
        ClassAttr p ps ->
            maybe [] (\why -> [(p, why)]) (checkAttribute reg tag "css-class" Nothing)
                <> interpolation p "class" ps

    -- Where an interpolation may go in an attribute value.
    interpolation p attrName ps
        | any isEmbed ps = [(p, "^{...} cannot be used in an attribute value; use #{...}")]
        | not (any isInterp ps) = []
        | attrName `elem` urlAttributes = case ps of
            [PInterp _ _] -> []
            PLit lit : _ | safeUrlPrefix lit -> []
            _ ->
                [ ( p
                  , "the URL in "
                        <> attrName
                        <> " must be a single #{...} of type SafeUrl, or start with a literal https://, http://, mailto:, /, # or ?"
                  )
                ]
        | "on" `T.isPrefixOf` T.toLower attrName = [(p, "event handler attributes such as " <> attrName <> " cannot be interpolated")]
        | otherwise = []

    isEmbed = \case
        PEmbed _ _ -> True
        _ -> False
    isInterp = \case
        PInterp _ _ -> True
        _ -> False

    literal = \case
        [] -> Just ""
        [PLit t] -> Just t
        _ -> Nothing

    -- Inside an ending tag everything is HTML, which MJML copies as it is.
    html = \case
        DTag p name as kids
            | "mj-" `T.isPrefixOf` name ->
                [(p, "<" <> name <> "> inside an ending tag would be copied as HTML; close the ending tag first")]
            | otherwise -> concatMap htmlAttribute as <> concatMap html kids
        DForall _ _ _ kids -> concatMap html kids
        DWith _ _ _ kids -> concatMap html kids
        DIf _ branches final -> concat [concatMap html ks | (_, _, ks) <- branches] <> maybe [] (concatMap html) final
        DMaybe _ _ _ kids nothing -> concatMap html kids <> maybe [] (concatMap html) nothing
        DCase _ _ alts -> concat [concatMap html ks | (_, _, ks) <- alts]
        DText _ _ -> []
        DEmbed _ _ -> []
        DOf p _ _ -> [(p, "$of without a matching $case")]

      where
        htmlAttribute = \case
            Attr p attrName value -> interpolation p attrName (maybe [] id value)
            ClassAttr p ps -> interpolation p "class" ps
