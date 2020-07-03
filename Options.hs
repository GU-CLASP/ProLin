module Options where

import Options.Applicative
data Options = Options {optFuel :: Int
                       ,optPauseStep :: Bool
                       ,optPauseInteractive :: Bool
                       ,optShowRules :: Bool
                       ,optShowState :: Bool
                       ,optProgramFile :: String}

defaults :: Options
defaults = Options 10 False False True True "<interactive>"

options :: ParserInfo Options
options =
  info ((Options <$>
         option auto (showDefault <> value 10 <> short 'f' <> long "fuel" <> help "amount of fuel" <> metavar "INT") <*>
         switch (long "step" <> short 'p' <> help "pause after each step") <*>
         switch (long "interactive" <> short 'i' <> help "pause at each interaction (Utter/Heard)") <*>
         flag True False (long "no-show-rules" <> short 'r' <> help "don't show the rules that are applied") <*>
         flag True False (long "no-show-state" <> short 's' <> help "don't show the state after each step") <*>
         (argument str (metavar "INPUT"))
        ) <**> helper) (fullDesc <> progDesc "pli checker and interpreter")

readOptions :: IO Options
readOptions = execParser options
