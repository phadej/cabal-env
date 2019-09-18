{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- |
-- Copyright: Oleg Grenrus
-- License: GPL-3.0-or-later
module Main (main) where

import Control.Applicative       (many, optional, (<**>), (<|>))
import Control.Exception         (handle)
import Control.Monad             (unless, when)
import Data.ByteString           (ByteString)
import Data.Char                 (isAlphaNum)
import Data.Foldable             (traverse_)
import Data.List                 (intercalate, isPrefixOf, sort)
import Data.List.Split           (splitOn)
import Data.Map                  (Map)
import Data.Maybe                (mapMaybe)
import Data.Version              (showVersion)
import System.Directory
       (createDirectoryIfMissing, getAppUserDataDirectory, listDirectory)
import System.Exit               (ExitCode (..), exitFailure)
import System.FilePath           ((</>))
import System.FilePath.Glob      (glob)
import System.IO                 (hPutStrLn, stderr)
import System.IO.Error           (IOError)
import System.Process
       (StdStream (NoStream), createProcess, cwd, proc, std_in, waitForProcess)
import System.Process.ByteString (readProcessWithExitCode)
import Text.PrettyPrint          ((<+>))
import Text.Read                 (readMaybe)

import Distribution.Parsec            (eitherParsec, explicitEitherParsec)
import Distribution.Pretty            (prettyShow)
import Distribution.Simple.Utils      (fromUTF8BS)
import Distribution.Types.Dependency  (Dependency (..))
import Distribution.Types.PackageId   (PackageId, PackageIdentifier (..))
import Distribution.Types.PackageName (PackageName, mkPackageName)
import Distribution.Version
       (Version, VersionRange, anyVersion, intersectVersionRanges, nullVersion,
       thisVersion)
import System.IO.Temp                 (withSystemTempDirectory)

import qualified Data.ByteString                 as BS
import qualified Data.List.NonEmpty              as NE
import qualified Data.Map.Strict                 as Map
import qualified Distribution.Compat.CharParsing as P
import qualified Distribution.Fields.Pretty      as C
import qualified Distribution.Pretty             as C
import qualified Options.Applicative             as O
import qualified Text.PrettyPrint                as PP

import Paths_cabal_env (version)

main :: IO ()
main = do
    opts <- O.execParser optsP'
    ghcEnvDir <- getGhcEnvDir (optCompiler opts)

    case optAction opts of
        Nothing         -> installAction opts ghcEnvDir
        Just ActionShow -> showAction opts ghcEnvDir
        Just ActionList -> listAction opts ghcEnvDir
  where
    optsP' = O.info (optsP <**> O.helper <**> versionP) $ mconcat
        [ O.fullDesc
        , O.progDesc "Manage package-enviroments"
        , O.header "cabal-env - a better cabal-install install --lib"
        ]

    versionP = O.infoOption (showVersion version)
        $ O.long "version" <> O.help "Show version"

-------------------------------------------------------------------------------
-- List
-------------------------------------------------------------------------------

listAction :: Opts -> FilePath -> IO ()
listAction Opts {..} ghcEnvDir = do
    when optVerbose $ putStrLn $ "Environments available in " ++ ghcEnvDir
    fs <- listDirectory ghcEnvDir
    traverse_ putStrLn (sort fs)

-------------------------------------------------------------------------------
-- Show
-------------------------------------------------------------------------------

showAction :: Opts -> FilePath -> IO ()
showAction Opts {..} ghcEnvDir = do
    pkgIds <- getEnvironmentContents $ ghcEnvDir </> optEnvName
    when optVerbose $ putStrLn $ "Packages in " ++ optEnvName ++ " environment"
    traverse_ (putStrLn . prettyShow) (sort pkgIds)

-------------------------------------------------------------------------------
-- Install
-------------------------------------------------------------------------------

installAction :: Opts -> FilePath -> IO ()
installAction Opts {..} ghcEnvDir = do
    unless (null optDeps) $ do
        when optVerbose $ do
            putStrLn "=== ghc environment directory"
            putStrLn ghcEnvDir

        createDirectoryIfMissing True ghcEnvDir
        pkgIds <- getEnvironmentContents $ ghcEnvDir </> optEnvName

        when optVerbose $ do
            putStrLn "=== packages in environment"
            traverse_ (putStrLn . prettyShow) pkgIds

        let cabalFile = fakePackage $ Map.fromListWith intersectVersionRanges $
                [ (pn, if ver == nullVersion || optAnyVer then anyVersion else thisVersion ver)
                | PackageIdentifier pn ver <- pkgIds
                , pn `notElem` [ mkPackageName "rts" ]
                ] ++
                [ (pn, vr)
                | Dependency pn vr _ <- optDeps
                ]

        when optVerbose $ do
            putStrLn "=== Generated fake-package.cabal"
            putStrLn cabalFile

        withSystemTempDirectory "cabal-env-fake-package-XXXX" $ \tmpDir -> do
            writeFile (tmpDir </> "fake-package.cabal") cabalFile
            writeFile (tmpDir </> "cabal.project") $ unlines
                [ "packages: ."
                , "with-compiler: " ++ optCompiler
                , "documentation: False"
                , "write-ghc-environment-files: always"
                , "package *"
                , "  documentation: False"
                ]

            (_, _, _, hdl) <- createProcess $
                    let p0 = proc "cabal" ["v2-build", "all", "--builddir=dist-newstyle"]
                        p  = p0
                            { cwd    = Just tmpDir
                            , std_in = NoStream
                            }
                        in p
            ec <- waitForProcess hdl

            case ec of
                ExitFailure _ -> do
                    hPutStrLn stderr "ERROR: cabal v2-build failed"
                    hPutStrLn stderr "This is might be due inconsistent dependencies (delete package env file, or try -a) or something else"
                    exitFailure

                ExitSuccess -> do
                    -- TODO: use cabal-plan to read stuff, better than relying on ghc.environment files, maybe?
                    matches <- glob $ tmpDir </> ".ghc.environment.*-*-*"
                    case matches of
                        [envFile] -> do
                            envFileContents <- BS.readFile envFile
                            when optVerbose $ do
                                putStrLn "=== local .ghc.environment file"
                                BS.putStr envFileContents

                            -- strip local stuff
                            let ls :: [String]
                                ls = filter (\l -> not $ any (`isPrefixOf` l) ["package-db dist-newstyle", "package-id fake-package-0-inplace"])
                                   $ lines
                                   $ fromUTF8BS envFileContents

                            -- (over)write ghc environment file
                            when optVerbose $ do
                                putStrLn "=== writing environment file"
                                putStr $ unlines ls

                            writeFile (ghcEnvDir </> optEnvName) (unlines ls)

                        _ -> die "Cannot find .ghc.environment file"


-------------------------------------------------------------------------------
-- GHC environment directory
-------------------------------------------------------------------------------

getGhcEnvDir :: FilePath -> IO FilePath
getGhcEnvDir ghc = do
    ghcDir   <- getAppUserDataDirectory "ghc"

    infoBS <- readProcessWithExitCode' ghc ["--info"]
    info <- maybe (die "Cannot parse compilers --info output") return $
        readMaybe (fromUTF8BS infoBS)

    case lookup ("Project name" :: String) info of
        Just "The Glorious Glasgow Haskell Compilation System" -> do
            versionStr <- maybe (die "cannot find Project version in info") return $
                lookup "Project version" info
            ver <- case eitherParsec versionStr of
                Right ver -> return (ver :: Version)
                Left err  -> die $ "Project version cannot be parsed\n" ++ err

            targetStr <- maybe (die "cannot find Target platform in info") return $
                lookup "Target platform" info
            (x,y) <- case splitOn "-" targetStr of
                [x, _, y] -> return (x, y)
                _         -> die "Target platform is not triple"

            return $ ghcDir </> (x ++ "-" ++ y ++ "-" ++ prettyShow ver) </> "environments"

        _ -> die "Your compiler is not GHC"

-------------------------------------------------------------------------------
-- GHC environment file
-------------------------------------------------------------------------------

getEnvironmentContents :: FilePath -> IO [PackageId]
getEnvironmentContents fp = handle onIOError $ do
    contents <- BS.readFile fp
    return
        $ mapMaybe (either (const Nothing) Just . parse)
        $ lines
        $ fromUTF8BS contents
  where
    onIOError :: IOError -> IO [a]
    onIOError _ = return []

    parse :: String -> Either String PackageId
    parse = explicitEitherParsec $ do
        _ <- P.string "package-id"
        P.spaces
        parts <- P.sepByNonEmpty (P.munch1 $ \c -> c == '.' || isAlphaNum c) (P.char '-')
        either fail return $
            if isHash (NE.last parts)
            then parse' (NE.init parts)
            else parse' (NE.toList parts)

    isHash :: String -> Bool
    isHash s = length s == 64

    parse' :: [String] -> Either String PackageId
    parse' = eitherParsec . intercalate "-"

-------------------------------------------------------------------------------
-- Options parser
-------------------------------------------------------------------------------

-- TODO: commands to list package environments, their contents, delete, copy.
-- TODO: special . name for "package environment in this directory"
data Opts = Opts
    { optCompiler :: FilePath
    , optEnvName  :: String
    , optAnyVer   :: Bool
    , optVerbose  :: Bool
    , optDeps     :: [Dependency]
    , optAction   :: Maybe Action
    }

data Action
    = ActionShow  -- ^ show package environment contents
    | ActionList  -- ^ list package environments

optsP :: O.Parser Opts
optsP = Opts
    <$> O.strOption (O.short 'w' <> O.long "with-compiler" <> O.value "ghc" <> O.showDefault <> O.help "Specify compiler to use")
    <*> O.strOption (O.short 'n' <> O.long "name" <> O.value "default" <> O.showDefault <> O.help "Environment name")
    <*> O.switch (O.short 'a' <> O.long "any" <> O.help "Allow any version of existing packages")
    <*> O.switch (O.short 'v' <> O.long "verbose" <> O.help "Print stuff...")
    <*> many (O.argument (O.eitherReader eitherParsec) (O.metavar "PKG..." <> O.help "packages (with possible bounds)"))
    -- behaviour flags
    <*> optional actionP

actionP :: O.Parser Action
actionP = showP <|> listP where
    showP = O.flag' ActionShow (O.short 's' <> O.long "show" <> O.help "Shows the contents of the environment")
    listP = O.flag' ActionList (O.short 'l' <> O.long "list" <> O.help "List package environments")

-------------------------------------------------------------------------------
-- Fake project
-------------------------------------------------------------------------------

fakePackage :: Map PackageName VersionRange -> String
fakePackage deps = C.showFields (const [])
    [ fi "cabal-version" $ PP.text "2.4"
    , fi "name"          $ PP.text "fake-package"
    , fi "version"       $ PP.text "0"

    , C.PrettySection () "library" []
        [ fi "default-language" "Haskell2010"
        , fi "build-depends" $ PP.text "base"
        , fi "build-depends" $ PP.vcat
            [ PP.comma <+> C.pretty pn <+> C.pretty vr
            | (pn, vr) <- Map.toList deps
            ]
        ]
    ]
  where
    fi = C.PrettyField ()

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

die :: String -> IO a
die msg = do
    hPutStrLn stderr $ "Panic: " ++ msg
    exitFailure

{-
runProcess
    :: FilePath      -- ^ Working directory
    -> String        -- ^ Command
    -> [String]      -- ^ Arguments
    -> ByteString    -- ^ Stdin
    -> IO (ExitCode, ByteString, ByteString)
runProcess cwd cmd args stdin =
    readCreateProcessWithExitCode p stdin
  where
    p0 = proc cmd args
    p  = p0
        { cwd = Just cwd
        }
-}

readProcessWithExitCode' :: FilePath -> [String] -> IO ByteString
readProcessWithExitCode' cmd args = do
    (ec, out, err) <- readProcessWithExitCode cmd args BS.empty
    case ec of
        ExitSuccess   -> return out
        ExitFailure _ -> do
            hPutStrLn stderr $ "Error running " ++ cmd ++ " " ++ unwords args
            BS.hPutStr stderr err
            exitFailure
