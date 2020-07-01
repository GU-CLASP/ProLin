module Options where

import Options.Applicative
data Options = Options {optFuel :: Int
                       ,optPauseStep :: Bool
                       ,optProgramFile :: String}

defaults :: Options
defaults = Options 10 False "<interactive>"

options :: ParserInfo Options
options =
  info ((Options <$>
         option auto (showDefault <> value 10 <> short 'f' <> long "fuel" <> help "amount of fuel" <> metavar "INT") <*>
         switch (long "step" <> short 's' <> help "pause after each step") <*>
         (argument str (metavar "INPUT"))
        ) <**> helper) (fullDesc <> progDesc "pli checker and interpreter")

readOptions :: IO Options
readOptions = execParser options
