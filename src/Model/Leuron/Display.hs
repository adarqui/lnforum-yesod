{-# LANGUAGE RecordWildCards #-}

module Model.Leuron.Display (
  leuronDisplay,
  leuronTypeToTextM
) where



import Import
import LN.T.Leuron
import qualified Data.Text as T (pack)
import Misc.Codec (decodeText)
import Data.Maybe (fromJust)



leuronDisplay :: Leuron -> LeuronData -> Widget
leuronDisplay leuron leuron_data = do

  leuron_data_display <- (case leuron_data of

    LnFact Fact{..} -> do
      [whamlet|
<p>
  <pre>#{factText}
      |]

    LnFactList FactList{..} -> do
      [whamlet|
<p>
  <h3>Fact</h3>
  <pre>#{factListFact}
<p>
  <h3>Facts</h3>
  <ul>
    $forall item <- factListList
      <li>
        <pre>#{item}
      |]

    LnCard Card{..} -> do
      [whamlet|
<p>
  <h3>Front
  <pre>#{cardFront}
<p>
  <h3>Back
  <pre>#{cardBack}
      |]

    LnDCard DCard{..} -> do
      [whamlet|
<p>#{dcardFront}
<p>#{dcardBack}
      |]

    LnDCardX DCardX{..} -> do
      [whamlet|
      |]

    LnAcronym Acronym{..} -> do
      [whamlet|
<p>#{acronymAbbreviation}
<p>#{acronymMeaning}
      |]

    LnSynonym Synonym{..} -> do
      [whamlet|
<p>#{synonymA}
<p>#{synonymB}
      |]

    LnAntonym Antonym{..} -> do
      [whamlet|
<p>#{antonymA}
<p>#{antonymB}
      |]

    LnTemplate Template{..} -> do
      [whamlet|
      |]

    LnImageAssociation ImageAssociation{..} -> do
      [whamlet|
      |]

    LnScript Script{..} -> do
      [whamlet|
      |]

    LnLinearDemo LinearDemo{..} -> do
      [whamlet|
      |]

    LnTable Table{..} -> do
      [whamlet|
      |]

    LnQA QA{..} -> do
      [whamlet|
<p>#{qaQuestion}
<p>#{qaAnswer}
      |]

    _             -> do
      [whamlet|Unknown|])

  examples' <- [whamlet|
  $if null examples
  $else
    <h3>Examples
    <ul>
      $forall example <- examples
        <li>
          <pre>#{example}
|]

  sections' <- [whamlet|
  $maybe section <- (leuronSection leuron)
    <h3>Section</h3>
      <pre>#{section}
  $nothing
    <div>
|]

  categories' <- [whamlet|
    $if null categories
    $else
      <h3>Categories
      <ul>
        $forall category <- categories
          <li>
            <pre>#{show category}
|]

  return $ mconcat [leuron_data_display, examples', sections', categories']

  -- TODO: FIXME
  where
  examples = case (leuronExamples leuron) of
    Nothing -> []
    (Just ex) -> fromJust $ (decodeText ex :: Maybe [Text])

  categories = case (leuronCategories leuron) of
    Nothing -> []
    (Just cats) -> fromJust $ (decodeText cats :: Maybe [[Text]])



leuronTypeToTextM :: Leuron -> Text
leuronTypeToTextM leuron =
  case (decodeText (leuronData leuron)) of
    Nothing -> T.pack "Unknown"
    (Just v) -> leuronTypeToText v
