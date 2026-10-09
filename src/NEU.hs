-- | Shared entry point for NEU campus services.
module NEU (versionString) where

import Data.Version (showVersion)
import Paths_neu (version)

versionString :: String
versionString = showVersion version
