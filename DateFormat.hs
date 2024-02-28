module DateFormat where

import Hakyll
import Data.Time.Format (parseTimeM, formatTime, defaultTimeLocale)
import Data.Time.Clock (UTCTime)

-- "created" and "modified" are metadata fields in the markdown file
-- They have the format "YYYY-MM-DD", sometimes created has a time as well: "YYYY-MM-DDTHH:MM:SSZ"
--
-- Example:
-- created: 2018-01-19T16:50:20Z
-- modified: 2024-02-22


-- Custom date field extractor and formatter (dateField is not flexible enough)
customDateField :: String -> String -> String -> Context a
customDateField key format metadataKey = field key $ \item -> do
    let identifier = itemIdentifier item
    metadata <- getMetadata identifier
    let maybeDateStr = lookupString metadataKey metadata
    case maybeDateStr of
        Just dateStr -> parseAndFormatDate dateStr format
        Nothing -> fail $ "Metadata field '" ++ metadataKey ++ "' not found for " ++ show identifier

-- Helper to parse and format date
parseAndFormatDate :: String -> String -> Compiler String
parseAndFormatDate dateStr format = case timed dateStr of
    Just time -> return $ formatTime defaultTimeLocale format time
    Nothing -> fail $ "Could not parse date: " ++ dateStr

-- needed as the date parser is strict about the format
timed :: String -> Maybe UTCTime
timed x = parseTimeM True defaultTimeLocale "%Y-%m-%d" x