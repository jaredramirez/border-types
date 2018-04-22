{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI where

import           Control.Exception          (Exception, SomeException)
import qualified Control.Exception          as Exception
import qualified Control.Monad              as M
import qualified Control.Monad.IO.Class     as MIO
import qualified Control.Monad.Trans.Class  as MC
import qualified Control.Monad.Trans.Except as E
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import           Data.Semigroup             ((<>))
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Version               as V
import qualified Misc
import           Options.Applicative        (Parser, ParserInfo, (<**>), (<|>))
import qualified Options.Applicative        as OptApp
import qualified Paths_border_types         as Self
import qualified System.Directory           as SysDir
import           Types                      (RootConfig)
import qualified Types

start :: IO ()
start = mainSubroutine =<< OptApp.execParser opts

data Op
  = Version
  | GenerateTypes Text

opts :: ParserInfo Op
opts =
  OptApp.info
    (mainSubroutineParser <**> OptApp.helper)
    (OptApp.fullDesc <>
     OptApp.progDesc "Generate types from type definition file FILENAME" <>
     OptApp.header "border-types - A tool to keep you types in sync")

-- MAIN SUBROUTINE
mainSubroutineParser :: Parser Op
mainSubroutineParser = generateSubroutineParser <|> versionSubroutineParser

mainSubroutine :: Op -> IO ()
mainSubroutine op =
  case op of
    Version            -> versionSubroutine
    GenerateTypes path -> generateSubroutine path

-- VERSION SUBROUTINE
versionSubroutineParser :: Parser Op
versionSubroutineParser =
  OptApp.flag'
    Version
    (OptApp.long "version" <> OptApp.short 'V' <>
     OptApp.help "Print installed version number")

versionSubroutine :: IO ()
versionSubroutine = putStrLn (V.showVersion Self.version)

-- GENERATE TYPES SUBROUTINE
data GenerateTypeSubroutineError
  = FileNotFound Text
  | CannotReadFile Text

type ExceptT a = E.ExceptT GenerateTypeSubroutineError IO a

liftIOToExceptT ::
     Exception e => (e -> GenerateTypeSubroutineError) -> IO a -> ExceptT a
liftIOToExceptT toErr io = E.withExceptT toErr (E.ExceptT $ Exception.try io)

forSomeExecption :: a -> SomeException -> a
forSomeExecption err _ = err

generateSubroutineParser :: Parser Op
generateSubroutineParser =
  GenerateTypes <$>
  OptApp.strArgument
    (OptApp.metavar "FILENAME" <> OptApp.help "Path to type definition file")

generateSubroutine :: Text -> IO ()
generateSubroutine path =
  let except =
        E.withExceptT
          dispalyGenerateTypeSubroutineError
          (doesFileExist path >>= readFileAsByteString)
  in E.runExceptT except >>= \case
       Left errorMessage -> putStrLn (Text.unpack errorMessage)
       Right _ -> putStrLn (Text.unpack "Success")

doesFileExist :: Text -> ExceptT Text
doesFileExist path =
  liftIOToExceptT
    (forSomeExecption (FileNotFound path))
    (SysDir.doesFileExist $ Text.unpack path) >>= \doesExist ->
    E.ExceptT
      (return $
       if doesExist
         then Right path
         else Left (FileNotFound path))

readFileAsByteString :: Text -> ExceptT ByteString
readFileAsByteString path =
  liftIOToExceptT
    (forSomeExecption (CannotReadFile path))
    (BS.readFile $ Text.unpack path)

tab :: Text
tab = "    "

dispalyGenerateTypeSubroutineError :: GenerateTypeSubroutineError -> Text
dispalyGenerateTypeSubroutineError err =
  "\n" <> tab <>
  case err of
    FileNotFound path ->
      "Uh oh, I couldn't find any file at \"" <> path <> "\". Is there a typo?"
    CannotReadFile path ->
      "Uh oh, I couldn't read the file at \"" <> path <> "\"."
