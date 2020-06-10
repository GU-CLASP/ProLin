
import TopLevel
import Options.Applicative

options :: ParserInfo (Int, String)
options =
  info (((,) <$>
         option auto (showDefault <> value 10 <> short 'f' <> long "fuel" <> help "amount of fuel" <> metavar "INT") <*>
         (argument str (metavar "INPUT"))
        ) <**> helper) (fullDesc <> progDesc "pli checker and interpreter")

main :: IO ()
main = do
  (fuel,fname) <- execParser options
  (r0,rs) <- loadAndPrepareModule fname
  _ <- run fuel r0 rs
  return ()

