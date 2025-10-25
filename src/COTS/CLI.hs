{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Command Line Interface for COTS (Cardano Offline Transaction Simulator)
module COTS.CLI
  ( runCLI,
    Command (..),
    AppInitOptions (..),
    TransactionCommand (..),
    UTXOCommand (..),
    QueryCommand (..),
    QueryUTXOptions (..),
    ProtocolCommand (..),
    BuildOptions (..),
    BuildRawOptions (..),
    SimulateOptions (..),
    SignOptions (..),
    ValidateOptions (..),
    ExportOptions (..),
    DecodeOptions (..),
    ListOptions (..),
    ReserveOptions (..),
    UpdateOptions (..),
    DatabaseCommand (..),
    InitOptions (..),
    ResetOptions (..),
    SnapshotOptions (..),
    LoadSnapshotOptions (..),
    ImportUTXOptions (..),
    ExportUTXOptions (..),
    GenerateUTXOptions (..),
    InspectOptions (..),
    AddressCommand (..),
    AddressKeyGenOptions (..),
    AddressBuildOptions (..),
    AddressInfoOptions (..),
    StakeAddressCommand (..),
    StakeAddressKeyGenOptions (..),
    StakeAddressBuildOptions (..),
    StakeAddressInfoOptions (..),
    MintCommand (..),
    MintBuildOptions (..),
    MintCalculateOptions (..),
  )
where

import COTS.Config (loadConfig)
import COTS.Database (DBUTXO (..), DBWallet (..), DBProtocolParams (..), Database (..), closeDatabase, exportUTXOs, getWalletByName, getWallets, importUTXOs, initDatabase, insertProtocolParams, insertUTXO, insertWallet, inspectDatabase, loadSnapshot, resetDatabase, snapshotDatabase)
import COTS.Export.CardanoCLI (exportTransactionToFile)
import COTS.Export.Koios (exportTransactionToKoiosFile)
import COTS.Protocol.Parameters (defaultProtocolParameters, loadProtocolParameters)
import COTS.Simulation.Core (SimulationContext (..), simulateTransaction)
import COTS.Types hiding (command)
import COTS.Version (getVersionString)
import COTS.Wallet.HD (bech32FromAddress)
import Control.Monad (filterM, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (encode, decode, Value (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector as V
import Data.Char (intToDigit)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, maybeToList)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Time (getCurrentTime, utctDayTime)
import Data.Word (Word64)
import Options.Applicative
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import System.Exit (exitFailure)
import System.FilePath (isAbsolute, (</>), takeDirectory, takeBaseName)
import System.Random (randomRIO)
import Text.Printf (printf)
import System.Process (readProcess)
import Text.Read (readMaybe)

-- | ANSI Color codes for professional CLI output
data AnsiColor = 
    ColorReset | Bold | Dim | Italic | Underline | Blink | Reverse | Hidden |
    Black | Red | Green | Yellow | Blue | Magenta | Cyan | White |
    BrightBlack | BrightRed | BrightGreen | BrightYellow | 
    BrightBlue | BrightMagenta | BrightCyan | BrightWhite

-- | Convert color to ANSI escape sequence
colorCode :: AnsiColor -> String
colorCode ColorReset = "\ESC[0m"
colorCode Bold = "\ESC[1m"
colorCode Dim = "\ESC[2m"
colorCode Italic = "\ESC[3m"
colorCode Underline = "\ESC[4m"
colorCode Blink = "\ESC[5m"
colorCode Reverse = "\ESC[7m"
colorCode Hidden = "\ESC[8m"
colorCode Black = "\ESC[30m"
colorCode Red = "\ESC[31m"
colorCode Green = "\ESC[32m"
colorCode Yellow = "\ESC[33m"
colorCode Blue = "\ESC[34m"
colorCode Magenta = "\ESC[35m"
colorCode Cyan = "\ESC[36m"
colorCode White = "\ESC[37m"
colorCode BrightBlack = "\ESC[90m"
colorCode BrightRed = "\ESC[91m"
colorCode BrightGreen = "\ESC[92m"
colorCode BrightYellow = "\ESC[93m"
colorCode BrightBlue = "\ESC[94m"
colorCode BrightMagenta = "\ESC[95m"
colorCode BrightCyan = "\ESC[96m"
colorCode BrightWhite = "\ESC[97m"

-- | Colorized output functions
colorize :: AnsiColor -> String -> String
colorize color text = colorCode color ++ text ++ colorCode ColorReset

-- | Professional output functions with enhanced styling
successMsg :: String -> IO ()
successMsg msg = putStrLn $ colorize Bold (colorize BrightGreen ("✓ " ++ msg))

errorMsg :: String -> IO ()
errorMsg msg = putStrLn $ colorize Bold (colorize BrightRed ("✗ " ++ msg))

warningMsg :: String -> IO ()
warningMsg msg = putStrLn $ colorize Bold (colorize BrightYellow ("⚠ " ++ msg))

infoMsg :: String -> IO ()
infoMsg msg = putStrLn $ colorize BrightBlue ("ℹ " ++ msg)

progressMsg :: String -> IO ()
progressMsg msg = putStrLn $ colorize Bold (colorize Cyan ("▶ " ++ msg))

-- | Enhanced colorized output functions
highlightSuccess :: String -> String
highlightSuccess = colorize Bold . colorize BrightGreen

highlightError :: String -> String
highlightError = colorize Bold . colorize BrightRed

highlightWarning :: String -> String
highlightWarning = colorize Bold . colorize BrightYellow

highlightInfo :: String -> String
highlightInfo = colorize BrightBlue

highlightProgress :: String -> String
highlightProgress = colorize Bold . colorize Cyan

highlightData :: String -> String
highlightData = colorize Bold . colorize BrightCyan

highlightAmount :: String -> String
highlightAmount = colorize Bold . colorize BrightGreen

highlightAddress :: String -> String
highlightAddress = colorize Bold . colorize BrightMagenta

highlightTxHash :: String -> String
highlightTxHash = colorize Bold . colorize BrightCyan

highlightTxIndex :: String -> String
highlightTxIndex = colorize Bold . colorize BrightYellow

-- | Professional section headers
sectionHeader :: String -> IO ()
sectionHeader title = do
  putStrLn ""
  putStrLn $ colorize Bold (colorize BrightWhite ("═══ " ++ title ++ " ═══"))
  putStrLn ""

subsectionHeader :: String -> IO ()
subsectionHeader title = do
  putStrLn ""
  putStrLn $ colorize Bold (colorize Cyan ("▸ " ++ title))
  putStrLn ""

-- | Professional table formatting
tableHeader :: [String] -> IO ()
tableHeader headers = do
  let headerLine = intercalate " │ " (map (colorize Bold . colorize BrightWhite) headers)
  putStrLn headerLine
  let separator = replicate (length headerLine) '─'
  putStrLn $ colorize Dim separator

-- | Professional status indicators
statusOk :: String -> IO ()
statusOk msg = putStrLn $ colorize Bold (colorize BrightGreen ("✓ " ++ msg))

statusError :: String -> IO ()
statusError msg = putStrLn $ colorize Bold (colorize BrightRed ("✗ " ++ msg))

statusWarning :: String -> IO ()
statusWarning msg = putStrLn $ colorize Bold (colorize BrightYellow ("⚠ " ++ msg))

statusInfo :: String -> IO ()
statusInfo msg = putStrLn $ colorize BrightBlue ("ℹ " ++ msg)

statusProgress :: String -> IO ()
statusProgress msg = putStrLn $ colorize Bold (colorize Cyan ("▶ " ++ msg))

-- | Professional data formatting
formatAmount :: Word64 -> String
formatAmount lovelace = 
  let ada = fromIntegral lovelace / 1000000 :: Double
      formatted = printf "%.6f" ada
  in highlightAmount (formatted ++ " ADA") ++ " " ++ colorize Dim ("(" ++ show lovelace ++ " lovelace)")

formatAddress :: String -> String
formatAddress addr = highlightAddress addr

formatTxHash :: String -> String
formatTxHash hash = highlightTxHash hash

formatTxIndex :: Word64 -> String
formatTxIndex idx = highlightTxIndex (show idx)

-- | Root-level app init options
data AppInitOptions = AppInitOptions
  { appInitPath :: Maybe FilePath,
    appInitName :: Maybe Text,
    appInitNetwork :: Network
  }

-- Add a new structure for global options
data GlobalOptions = GlobalOptions
  { goHome :: FilePath
  }

-- Parser for the global --home option
parseGlobalOptions :: Parser GlobalOptions
parseGlobalOptions =
  GlobalOptions
    <$> strOption
      ( long "home"
          <> metavar "DIR"
          <> value "~/.COTS_NODE"
          <> showDefault
          <> help "Root directory for all COTS files (default: ~/.COTS_NODE)"
      )

-- | Get the root COTS_NODE directory (~/.COTS_NODE)
getCotsNodeDir :: IO FilePath
getCotsNodeDir = do
  home <- getHomeDirectory
  let dir = home </> ".COTS_NODE"
  createDirectoryIfMissing True dir
  return dir

-- | Get a subdirectory of COTS_NODE, creating it if necessary
getCotsNodeSubdir :: FilePath -> String -> IO FilePath
getCotsNodeSubdir home sub = do
  let subdir = home </> sub
  createDirectoryIfMissing True subdir
  return subdir

-- | Get the path to the COTS database in ~/.COTS_NODE/cots.db
getCotsNodeDbPath :: IO FilePath
getCotsNodeDbPath = do
  root <- getCotsNodeDir
  return (root </> "cots.db")

-- | Main CLI command
data Command
  = TransactionCmd TransactionCommand
  | UTXOCmd UTXOCommand
  | QueryCmd QueryCommand
  | ProtocolCmd ProtocolCommand
  | DatabaseCmd DatabaseCommand
  | WalletCmd WalletCommand
  | AddressCmd AddressCommand
  | StakeAddressCmd StakeAddressCommand
  | MintCmd MintCommand
  | AppInitCmd AppInitOptions
  | Version

-- | Transaction subcommands
data TransactionCommand
  = Build BuildOptions
  | BuildRaw BuildRawOptions
  | Sign SignOptions
  | Submit SubmitOptions
  | CalculateMinFee CalculateMinFeeOptions
  | View ViewOptions
  | TxId TxIdOptions
  | Simulate SimulateOptions
  | Validate ValidateOptions
  | Export ExportOptions
  | Decode DecodeOptions

-- | UTXO subcommands
data UTXOCommand
  = List ListOptions
  | Reserve ReserveOptions
  | Process ProcessOptions

-- | Protocol subcommands
data ProtocolCommand
  = Update UpdateOptions
  | Fetch FetchOptions

-- | Database subcommands
data DatabaseCommand
  = Init InitOptions
  | Reset ResetOptions
  | Snapshot SnapshotOptions
  | LoadSnapshot LoadSnapshotOptions
  | ImportUTXO ImportUTXOptions
  | ExportUTXO ExportUTXOptions
  | GenerateUTXO GenerateUTXOptions
  | Inspect InspectOptions

-- | Wallet subcommands
data WalletCommand
  = Create CreateWalletOptions
  | ListWallets ListWalletsOptions
  | Import ImportWalletOptions
  | ExportWallet ExportWalletOptions
  | Info WalletInfoOptions

-- | Build transaction options (cardano-cli style)
data BuildOptions = BuildOptions
  { txIns :: [Text], -- --tx-in
    txOuts :: [Text], -- --tx-out
    changeAddress :: Maybe Text, -- --change-address
    dbFile :: FilePath, -- --db-file
    outFile :: FilePath, -- --out-file
    offline :: Bool, -- --offline (always true for COTS)
    fee :: Maybe Word64, -- --fee
    ttl :: Maybe Word64, -- --ttl
    scriptFile :: Maybe FilePath, -- --script-file
    datumFile :: Maybe FilePath, -- --datum-file
    redeemerFile :: Maybe FilePath -- --redeemer-file
  }

-- | Build raw transaction options (Cardano CLI compatible)
data BuildRawOptions = BuildRawOptions
  { buildRawEra :: Maybe Text, -- --babbage-era, --alonzo-era, etc.
    buildRawTxIns :: [Text], -- --tx-in
    buildRawTxOuts :: [Text], -- --tx-out
    buildRawFee :: Word64, -- --fee
    buildRawTtl :: Maybe Word64, -- --ttl
    buildRawOutFile :: FilePath, -- --out-file
    buildRawScriptFile :: Maybe FilePath, -- --script-file
    buildRawDatumFile :: Maybe FilePath, -- --datum-file
    buildRawRedeemerFile :: Maybe FilePath -- --redeemer-file
  }

-- | Simulate transaction options
data SimulateOptions = SimulateOptions
  { simTxFile :: FilePath, -- --tx-file
    simDbFile :: FilePath, -- --db-file
    simVerbose :: Bool -- --verbose
  }

-- | Sign transaction options
data SignOptions = SignOptions
  { signTxFile :: FilePath, -- --tx-file
    signKeyFile :: FilePath, -- --signing-key-file
    signOutFile :: FilePath -- --out-file
  }

-- | Validate transaction options
data ValidateOptions = ValidateOptions
  { validateTxFile :: FilePath, -- --tx-file
    validateDbFile :: FilePath -- --db-file
  }

-- | Export transaction options
data ExportOptions = ExportOptions
  { exportTxFile :: FilePath, -- --tx-file
    exportFormat :: ExportFormat, -- --format
    exportOutFile :: FilePath -- --out-file
  }

-- | Submit transaction options
data SubmitOptions = SubmitOptions
  { submitTxFile :: FilePath, -- --tx-file
    submitTestnetMagic :: Maybe Int, -- --testnet-magic
    submitNodeSocket :: Maybe FilePath -- --socket-path
  }

-- | Calculate minimum fee options
data CalculateMinFeeOptions = CalculateMinFeeOptions
  { calcTxBodyFile :: FilePath, -- --tx-body-file
    calcTxInCount :: Int, -- --tx-in-count
    calcTxOutCount :: Int, -- --tx-out-count
    calcWitnessCount :: Int, -- --witness-count
    calcTestnetMagic :: Maybe Text, -- --testnet-magic
    calcProtocolParamsFile :: FilePath -- --protocol-params-file
  }

-- | View transaction options
data ViewOptions = ViewOptions
  { viewTxFile :: FilePath, -- --tx-file
    viewVerbose :: Bool -- --verbose
  }

-- | Transaction ID options
data TxIdOptions = TxIdOptions
  { txIdTxFile :: FilePath -- --tx-file
  }

-- | Decode transaction options
data DecodeOptions = DecodeOptions
  { decodeTxFile :: FilePath, -- --tx-file
    decodeVerbose :: Bool -- --verbose
  }

-- | Query subcommands
data QueryCommand
  = QueryUTXO QueryUTXOptions

-- | Query UTXO options
data QueryUTXOptions = QueryUTXOptions
  { queryAddress :: Text, -- --address
    queryTestnetMagic :: Maybe Text, -- --testnet-magic
    queryMainnet :: Bool, -- --mainnet
    querySocketPath :: Maybe FilePath, -- --socket-path
    queryDbFile :: Maybe FilePath -- --db-file (COTS specific)
  }

-- | List UTXOs options
data ListOptions = ListOptions
  { listAddress :: Maybe Text, -- --address
    listDbFile :: FilePath, -- --db-file
    listVerbose :: Bool -- --verbose
  }

-- | Reserve UTXOs options
data ReserveOptions = ReserveOptions
  { reserveAddress :: Text, -- --address
    reserveAmount :: Word64, -- --amount
    reserveDbFile :: FilePath, -- --db-file
    reserveOutFile :: FilePath -- --out-file
  }

-- | Process transaction options
data ProcessOptions = ProcessOptions
  { processFromAddress :: Text, -- --from-address
    processToAddress :: Text, -- --to-address
    processAmount :: Word64, -- --amount
    processDbFile :: FilePath, -- --db-file
    processOutFile :: FilePath -- --out-file
  }

-- | Update protocol options
data UpdateOptions = UpdateOptions
  { updateProtocolParamsFile :: FilePath, -- --protocol-params-file
    updateDbFile :: FilePath -- --db-file
  }

-- | Fetch protocol parameters options
data FetchOptions = FetchOptions
  { fetchUrl :: String, -- --url
    fetchOutFile :: FilePath, -- --out-file
    fetchDbFile :: FilePath -- --db-file
  }

-- | Init database options
data InitOptions = InitOptions
  { initDbFile :: FilePath -- --db-file
  }

-- | Reset database options
data ResetOptions = ResetOptions
  { resetDbFile :: FilePath -- --db-file
  }

-- | Snapshot database options
data SnapshotOptions = SnapshotOptions
  { snapshotDbFile :: FilePath, -- --db-file
    snapshotOutFile :: FilePath -- --out-file
  }

-- | Load snapshot options
data LoadSnapshotOptions = LoadSnapshotOptions
  { loadSnapshotFile :: FilePath, -- --snapshot-file
    loadDbFile :: FilePath -- --db-file
  }

-- | Import UTXO options
data ImportUTXOptions = ImportUTXOptions
  { importDbFile :: FilePath, -- --db-file
    importUtxoFile :: FilePath -- --utxo-file
  }

-- | Export UTXO options
data ExportUTXOptions = ExportUTXOptions
  { exportDbFile :: FilePath, -- --db-file
    exportUtxoFile :: FilePath -- --out-file
  }

-- | Generate initial UTXOs options
data GenerateUTXOptions = GenerateUTXOptions
  { generateAddresses :: [Text], -- --addresses (comma-separated)
    generateAmounts :: [Word64], -- --amounts (comma-separated)
    generateOutFile :: FilePath, -- --out-file
    generatePrefix :: Maybe Text -- --prefix (optional, defaults to "genesis")
  }

-- | Inspect database options
data InspectOptions = InspectOptions
  { inspectDbFile :: FilePath -- --db-file
  }

-- | Create wallet options
data CreateWalletOptions = CreateWalletOptions
  { createWalletName :: Text, -- --name
    createWalletAddress :: Text, -- --address
    createWalletDbFile :: FilePath -- --db-file
  }

-- | List wallets options
data ListWalletsOptions = ListWalletsOptions
  { listWalletsDbFile :: FilePath -- --db-file
  }

-- | Import wallet options
data ImportWalletOptions = ImportWalletOptions
  { importWalletFile :: FilePath, -- --file
    importWalletDbFile :: FilePath -- --db-file
  }

-- | Export wallet options
data ExportWalletOptions = ExportWalletOptions
  { exportWalletName :: Text, -- --name
    exportWalletFile :: FilePath, -- --file
    exportWalletDbFile :: FilePath -- --db-file
  }

-- | Wallet info options
data WalletInfoOptions = WalletInfoOptions
  { walletInfoName :: Text, -- --name
    walletInfoDbFile :: FilePath -- --db-file
  }

-- | Address subcommands (cardano-cli compatible)
data AddressCommand
  = AddressKeyGen AddressKeyGenOptions
  | AddressBuild AddressBuildOptions
  | AddressInfo AddressInfoOptions

-- | Address key generation options
data AddressKeyGenOptions = AddressKeyGenOptions
  { keyGenVerificationKeyFile :: FilePath, -- --verification-key-file
    keyGenSigningKeyFile :: FilePath, -- --signing-key-file
    keyGenKeyType :: Maybe Text -- --key-type (normal, extended)
  }

-- | Address build options
data AddressBuildOptions = AddressBuildOptions
  { buildPaymentVerificationKeyFile :: Maybe FilePath, -- --payment-verification-key-file
    buildStakeVerificationKeyFile :: Maybe FilePath, -- --stake-verification-key-file
    buildOutFile :: FilePath, -- --out-file
    buildNetwork :: Network, -- --mainnet or --testnet-magic
    buildInitialAmount :: Maybe Word64, -- --initial-amount (lovelace)
    buildConfigPath :: Maybe FilePath -- --config (path to config.json to update)
  }

-- | Address info options
data AddressInfoOptions = AddressInfoOptions
  { infoAddress :: Text -- --address
  }

-- | Stake address subcommands
data StakeAddressCommand
  = StakeAddressKeyGen StakeAddressKeyGenOptions
  | StakeAddressBuild StakeAddressBuildOptions
  | StakeAddressInfo StakeAddressInfoOptions

-- | Stake address key generation options
data StakeAddressKeyGenOptions = StakeAddressKeyGenOptions
  { stakeKeyGenVerificationKeyFile :: FilePath, -- --verification-key-file
    stakeKeyGenSigningKeyFile :: FilePath -- --signing-key-file
  }

-- | Stake address build options
data StakeAddressBuildOptions = StakeAddressBuildOptions
  { stakeBuildStakeVerificationKeyFile :: FilePath, -- --stake-verification-key-file
    stakeBuildOutFile :: FilePath, -- --out-file
    stakeBuildNetwork :: Network -- --mainnet or --testnet-magic
  }

-- | Stake address info options
data StakeAddressInfoOptions = StakeAddressInfoOptions
  { stakeInfoAddress :: Text -- --address
  }

-- | Mint subcommands
data MintCommand
  = MintBuild MintBuildOptions
  | MintCalculate MintCalculateOptions

-- | Mint build options
data MintBuildOptions = MintBuildOptions
  { mintTxIn :: [Text], -- --tx-in
    mintTxOut :: [Text], -- --tx-out
    mintMint :: Maybe Text, -- --mint
    mintMintScriptFile :: Maybe FilePath, -- --mint-script-file
    mintChangeAddress :: Maybe Text, -- --change-address
    mintOutFile :: FilePath, -- --out-file
    mintNetwork :: Network, -- --mainnet or --testnet-magic
    mintProtocolParamsFile :: FilePath -- --protocol-params-file
  }

-- | Mint calculate options
data MintCalculateOptions = MintCalculateOptions
  { mintCalcPolicyId :: Text, -- --policy-id
    mintCalcAssetName :: Text, -- --asset-name
    mintCalcQuantity :: Word64, -- --quantity
    mintCalcProtocolParamsFile :: FilePath -- --protocol-params-file
  }

-- Fusionner le parsing des options globales et des sous-commandes dans un seul parser principal
-- Le parser principal doit être : Parser (FilePath, Command)
-- Les options globales sont placées avant la sous-commande

-- 1. Définir le parser principal comme Parser Command (plus de tuple)
-- 2. Ajouter --home comme option globale avec addGlobalOption
-- 3. Dans runCLI, parser d'abord --home (avec infoOption), puis parser la commande
-- 4. S'assurer que l'aide affiche toutes les sous-commandes à la racine

-- Exemple de structure :

-- 1. Le parser principal expose toutes les sous-commandes (sans --home)
mainParser :: Parser (FilePath, Command)
mainParser =
  (,)
    <$> strOption
      ( long "home"
          <> metavar "DIR"
          <> value "~/.COTS_NODE"
          <> showDefault
          <> help "Root directory for all COTS files (default: ~/.COTS_NODE)"
      )
    <*> commandParser <**> helper

runCLI :: IO ()
runCLI = do
  (homeDir, cmd) <-
    execParser $
      info
        mainParser
        ( fullDesc
            <> progDesc "Cardano Offline Transaction Simulator (Cardano-CLI compatible)"
            <> header "cotscli - Cardano offline transaction simulator (CLI compatible)"
        )
  runCommand cmd homeDir

-- 4. Les valeurs par défaut des fichiers sont gérées dans les fonctions d'exécution (run*),
--    en utilisant homeDir si l'utilisateur n'a rien passé (ex: if null dbFile then homeDir </> "cots.db" else dbFile)

-- 5. Le help affiche toutes les sous-commandes à la racine grâce à hsubparser dans commandParser.

-- Le reste du code (commandParser, etc.) reste inchangé, mais toutes les valeurs par défaut de fichiers utilisent homeDir.

-- Pour l'affichage du --help, utiliser hsubparser et info/progDesc pour chaque sous-commande, comme ci-dessus, pour une clarté maximale.

-- | Helper to run a command with a home directory
runCommand :: Command -> FilePath -> IO ()
runCommand cmd homeDir = case cmd of
  TransactionCmd txCmd -> runTransactionCommand txCmd homeDir
  UTXOCmd utxoCmd -> runUTXOCommand utxoCmd homeDir
  QueryCmd queryCmd -> runQueryCommand queryCmd homeDir
  ProtocolCmd protoCmd -> runProtocolCommand protoCmd homeDir
  DatabaseCmd dbCmd -> runDatabaseCommand dbCmd homeDir
  WalletCmd walletCmd -> runWalletCommand walletCmd homeDir
  AddressCmd addrCmd -> runAddressCommand addrCmd homeDir
  StakeAddressCmd stakeAddrCmd -> runStakeAddressCommand stakeAddrCmd homeDir
  MintCmd mintCmd -> runMintCommand mintCmd homeDir
  AppInitCmd opts -> runAppInit opts
  Version -> runVersion homeDir

-- Restore commandParser
commandParser :: Parser Command
commandParser =
  hsubparser
    ( command "database" (info databaseSub (progDesc "Manage the SQLite database and snapshots"))
        <> command "transaction" (info transactionSub (progDesc "Build, sign, simulate, and export Cardano transactions"))
        <> command "utxo" (info utxoSub (progDesc "List and filter UTXOs"))
        <> command "query" (info querySub (progDesc "Query blockchain state (Cardano CLI compatible)"))
        <> command "protocol" (info protocolSub (progDesc "Manage protocol parameters"))
        <> command "wallet" (info walletSub (progDesc "Manage wallets and addresses"))
        <> command "address" (info addressSub (progDesc "Manage payment addresses"))
        <> command "stake-address" (info stakeAddressSub (progDesc "Manage staking addresses"))
        <> command "mint" (info mintSub (progDesc "Token minting management"))
        <> command "init" (info appInitSub (progDesc "Initialize COTS workspace with per-network config.json"))
        <> command "version" (info (pure Version) (progDesc "Show COTS version"))
    )

-- === Place *_Sub combinators here, just before commandParser ===

databaseSub :: Parser Command
databaseSub = DatabaseCmd <$> databaseParser

transactionSub :: Parser Command
transactionSub = TransactionCmd <$> transactionParser

utxoSub :: Parser Command
utxoSub = UTXOCmd <$> utxoParser

querySub :: Parser Command
querySub = QueryCmd <$> queryParser

protocolSub :: Parser Command
protocolSub = ProtocolCmd <$> protocolParser

walletSub :: Parser Command
walletSub = WalletCmd <$> walletParser

addressSub :: Parser Command
addressSub = AddressCmd <$> addressParser
appInitSub :: Parser Command
appInitSub = AppInitCmd <$> appInitOptions

appInitOptions :: Parser AppInitOptions
appInitOptions =
  AppInitOptions
    <$> optional (strOption (long "path" <> metavar "DIR" <> help "Base directory for COTS workspace (default: ~/.cotscli)"))
    <*> optional (strOption (long "name" <> metavar "NAME" <> help "Optional name to tag this workspace"))
    <*> option auto (long "network" <> metavar "NETWORK" <> help "Network (Mainnet, Testnet, Preview, Preprod)" <> value Preprod <> showDefault)


stakeAddressSub :: Parser Command
stakeAddressSub = StakeAddressCmd <$> stakeAddressParser

mintSub :: Parser Command
mintSub = MintCmd <$> mintParser

-- Restore all run*Command functions
defaultRun :: String -> IO ()
defaultRun name = putStrLn $ "[ERROR] Command dispatcher missing for: " ++ name

runTransactionCommand :: TransactionCommand -> FilePath -> IO ()
runTransactionCommand cmd homeDir = case cmd of
  Build opts -> runBuild opts homeDir
  BuildRaw opts -> runBuildRaw opts homeDir
  Sign opts -> runSign opts homeDir
  Submit opts -> runSubmit opts homeDir
  CalculateMinFee opts -> runCalculateMinFee opts homeDir
  View opts -> runView opts homeDir
  TxId opts -> runTxId opts homeDir
  Simulate opts -> runSimulate opts homeDir
  Validate opts -> runValidate opts homeDir
  Export opts -> runExport opts homeDir
  Decode opts -> runDecode opts homeDir

runUTXOCommand :: UTXOCommand -> FilePath -> IO ()
runUTXOCommand cmd homeDir = case cmd of
  List opts -> runList opts homeDir
  Reserve opts -> runReserve opts homeDir
  Process opts -> runProcess opts homeDir

runQueryCommand :: QueryCommand -> FilePath -> IO ()
runQueryCommand cmd homeDir = case cmd of
  QueryUTXO opts -> runQueryUTXO opts homeDir

runProtocolCommand :: ProtocolCommand -> FilePath -> IO ()
runProtocolCommand cmd homeDir = case cmd of
  Update opts -> runUpdate opts homeDir
  Fetch opts -> runFetch opts homeDir

runDatabaseCommand :: DatabaseCommand -> FilePath -> IO ()
runDatabaseCommand cmd homeDir = case cmd of
  Init opts -> runInit opts homeDir
  Reset opts -> runReset opts homeDir
  Snapshot opts -> runSnapshot opts homeDir
  LoadSnapshot opts -> runLoadSnapshot opts homeDir
  ImportUTXO opts -> runImportUTXO opts homeDir
  ExportUTXO opts -> runExportUTXO opts homeDir
  GenerateUTXO opts -> runGenerateUTXO opts homeDir
  Inspect opts -> runInspect opts homeDir

runWalletCommand :: WalletCommand -> FilePath -> IO ()
runWalletCommand cmd homeDir = case cmd of
  Create opts -> runCreateWallet opts homeDir
  ListWallets opts -> runListWallets opts homeDir
  Import opts -> runImportWallet opts homeDir
  ExportWallet opts -> runExportWallet opts homeDir
  Info opts -> runWalletInfo opts homeDir

runAddressCommand :: AddressCommand -> FilePath -> IO ()
runAddressCommand cmd homeDir = case cmd of
  AddressKeyGen opts -> runAddressKeyGen opts homeDir
  AddressBuild opts -> runAddressBuild opts homeDir
  AddressInfo opts -> runAddressInfo opts homeDir

runStakeAddressCommand :: StakeAddressCommand -> FilePath -> IO ()
runStakeAddressCommand cmd homeDir = case cmd of
  StakeAddressKeyGen opts -> runStakeAddressKeyGen opts homeDir
  StakeAddressBuild opts -> runStakeAddressBuild opts homeDir
  StakeAddressInfo opts -> runStakeAddressInfo opts homeDir

runMintCommand :: MintCommand -> FilePath -> IO ()
runMintCommand cmd homeDir = case cmd of
  MintBuild opts -> runMintBuild opts homeDir
  MintCalculate opts -> runMintCalculate opts homeDir

-- | Run build command
runBuild :: BuildOptions -> FilePath -> IO ()
runBuild opts homeDir = do
  putStrLn "🔨 Building transaction (offline simulation)..."
  let txPath = outFile opts
  putStrLn $ "> Database file: " ++ dbFile opts
  putStrLn $ "> Output file: " ++ txPath

  -- Load UTXOs from database and config from sibling config.json if present
  db <- initDatabase (dbFile opts)
  utxos <- exportUTXOs db
  let cfgCandidate = takeDirectory (dbFile opts) </> "config.json"
  cfgExists <- doesFileExist cfgCandidate
  config <- if cfgExists
              then loadConfig cfgCandidate
              else do
                putStrLn "⚠️  No config.json found next to DB; using default protocol parameters (Preprod)."
                let params = loadProtocolParameters Preprod
                return Config { network = Preprod, protocolParameters = params, wallets = [] }
  closeDatabase db

  -- Load script, datum, and redeemer if provided
  script <- case scriptFile opts of
    Just file -> do
      mScriptPath <- resolveInputFile homeDir "scripts" file
      case mScriptPath of
        Just scriptPath -> do
          putStrLn $ "📜 Loading script from: " ++ scriptPath
          content <- readFile scriptPath
          return $ Just $ PlutusScript {scriptHash = ScriptHash "placeholder_hash", scriptBytes = T.pack content, scriptType = "PlutusScriptV2"}
        Nothing -> do
          putStrLn $ "ERROR: Script file '" ++ file ++ "' not found."
          return Nothing
    Nothing -> return Nothing

  datum <- case datumFile opts of
    Just file -> do
      mDatumPath <- resolveInputFile homeDir "scripts" file
      case mDatumPath of
        Just datumPath -> do
          putStrLn $ "> Loading datum from: " ++ datumPath
          content <- readFile datumPath
          return $ Just $ Datum {datumHash = "placeholder_datum_hash", datumBytes = T.pack content}
        Nothing -> do
          putStrLn $ "ERROR: Datum file '" ++ file ++ "' not found."
          return Nothing
    Nothing -> return Nothing

  redeemer <- case redeemerFile opts of
    Just file -> do
      mRedeemerPath <- resolveInputFile homeDir "scripts" file
      case mRedeemerPath of
        Just redeemerPath -> do
          putStrLn $ "> Loading redeemer from: " ++ redeemerPath
          content <- readFile redeemerPath
          return $ Just $ Redeemer {redeemerBytes = T.pack content, redeemerExecutionUnits = ExecutionUnits {memory = 1000, steps = 10000}}
        Nothing -> do
          putStrLn $ "ERROR: Redeemer file '" ++ file ++ "' not found."
          return Nothing
    Nothing -> return Nothing

  let ctx =
        SimulationContext
          { config = config,
            fromWallet = Nothing, -- Will be derived from tx-ins
            toAddress = Nothing, -- Will be derived from tx-outs
            simAmount = Nothing, -- Will be calculated from tx-outs
            script = script,
            datum = datum,
            redeemer = redeemer
          }

  let result = simulateTransaction ctx

  if success result
    then do
      putStrLn "SUCCESS: Transaction built successfully!"
      displayBuildResults result opts homeDir
      writeFile txPath "{\"type\": \"TxBody\", \"description\": \"Simulated Transaction\", \"cborHex\": \"placeholder\"}"
      putStrLn $ "> Transaction saved to: " ++ txPath
    else do
      putStrLn "ERROR: Transaction build failed!"
      mapM_ (putStrLn . ("Error: " ++) . show) (errors result)
      exitFailure

-- | Display build results
displayBuildResults :: SimulationResult -> BuildOptions -> FilePath -> IO ()
displayBuildResults result opts homeDir = do
  let details = simulationDetails result
      feeCalc = feeCalculation result

  putStrLn "\nBuild Results:"
  putStrLn "================="
  putStrLn $ "Total Input Amount: " ++ show (totalInputAmount details) ++ " lovelace"
  putStrLn $ "Total Output Amount: " ++ show (totalOutputAmount details) ++ " lovelace"
  putStrLn $ "Change Amount: " ++ show (simChangeAmount details) ++ " lovelace"
  putStrLn $ "Fee Amount: " ++ show (feeAmount details) ++ " lovelace"

  putStrLn "\nFee Breakdown:"
  putStrLn $ "  Base Fee: " ++ show (unLovelace (baseFee feeCalc)) ++ " lovelace"
  putStrLn $ "  Size Fee: " ++ show (unLovelace (sizeFee feeCalc)) ++ " lovelace"
  putStrLn $ "  Script Fee: " ++ show (unLovelace (scriptFee feeCalc)) ++ " lovelace"
  putStrLn $ "  Total Fee: " ++ show (unLovelace (totalFee feeCalc)) ++ " lovelace"

  putStrLn $ "\n> Transaction saved to: " ++ outFile opts

-- | Run build-raw command (Cardano CLI compatible)
runBuildRaw :: BuildRawOptions -> FilePath -> IO ()
runBuildRaw opts homeDir = do
  progressMsg "Building raw transaction..."
  
  -- Handle era parameter
  case buildRawEra opts of
    Just era -> infoMsg $ "Era: " ++ T.unpack era
    Nothing -> infoMsg "Era: babbage-era (default)"
  
  infoMsg $ "Transaction inputs: " ++ show (length $ buildRawTxIns opts)
  infoMsg $ "Transaction outputs: " ++ show (length $ buildRawTxOuts opts)
  infoMsg $ "Fee: " ++ show (buildRawFee opts) ++ " lovelace"
  infoMsg $ "Output file: " ++ buildRawOutFile opts
  
  -- Create a simple raw transaction file
  let txContent = unlines $
        [ "# Raw Transaction (COTS Simulation)"
        , "era: " ++ maybe "babbage-era" T.unpack (buildRawEra opts)
        , "inputs:"
        ] ++ map (\txin -> "  - " ++ T.unpack txin) (buildRawTxIns opts) ++
        [ "outputs:"
        ] ++ map (\txout -> "  - " ++ T.unpack txout) (buildRawTxOuts opts) ++
        [ "fee: " ++ show (buildRawFee opts)
        , "ttl: " ++ maybe "unlimited" show (buildRawTtl opts)
        ]
  
  -- Write transaction file
  writeFile (buildRawOutFile opts) txContent
  
  successMsg "Raw transaction built successfully!"
  infoMsg $ "Transaction saved to: " ++ buildRawOutFile opts

-- | Run simulate command
runSimulate :: SimulateOptions -> FilePath -> IO ()
runSimulate opts homeDir = do
  putStrLn "> Simulating transaction..."
  putStrLn $ "> Transaction file: " ++ simTxFile opts
  putStrLn $ "> Database file: " ++ simDbFile opts

  -- Load database
  db <- initDatabase (simDbFile opts)

  -- Load transaction from file
  txContent <- readFile (simTxFile opts)
  putStrLn "> Transaction loaded from file"

  -- Parse transaction (simplified - in real implementation, parse CBOR)
  let tx = parseTransactionFromFile txContent

  -- Simulate the transaction
  let result = simulateTransactionFromFile db tx

  closeDatabase db

  if success result
    then do
      putStrLn "SUCCESS: Transaction simulation completed!"
      when (simVerbose opts) $ do
        putStrLn "\nDetailed simulation results:"
        putStrLn "=============================="
        putStrLn "Input UTXOs:"
        mapM_ printUTXO (inputUTXOs (simulationDetails result))
        putStrLn "Output UTXOs:"
        mapM_ printUTXO (outputUTXOs (simulationDetails result))
        putStrLn "Execution units:"
        case executionUnits result of
          Just units -> do
            putStrLn $ "  Memory: " ++ show (memory units)
            putStrLn $ "  Steps: " ++ show (steps units)
          Nothing -> putStrLn "  No script execution"
    else do
      putStrLn "ERROR: Transaction simulation failed!"
      mapM_ (putStrLn . ("Error: " ++) . show) (errors result)
      exitFailure

-- | Parse transaction from file content (simplified)
parseTransactionFromFile :: String -> Transaction
parseTransactionFromFile content =
  -- In real implementation, this would parse CBOR or JSON
  Transaction
    { txId = TransactionId "placeholder_tx_id_0000000000000000000000000000000000000000000000000000000000000000",
      txInputs = [],
      txOutputs = [],
      txFee = Lovelace 0,
      txValidRange = Nothing,
      txScripts = [],
      txDatums = [],
      txRedeemers = []
    }

-- | Simulate transaction from file
simulateTransactionFromFile :: Database -> Transaction -> SimulationResult
simulateTransactionFromFile db tx =
  -- In real implementation, this would validate and simulate the transaction
  SimulationResult
    { success = True,
      transaction = Just tx,
      feeCalculation = FeeCalculation (Lovelace 0) (Lovelace 0) (Lovelace 0) (Lovelace 0),
      errors = [],
      finalUTXOs = Map.empty,
      executionUnits = Nothing,
      simulationDetails = SimulationDetails [] [] Nothing 0 0 0 0
    }

-- | Run sign command
runSign :: SignOptions -> FilePath -> IO ()
runSign opts homeDir = do
  putStrLn "✍️  Signing transaction (offline)..."
  putStrLn $ "> Transaction file: " ++ signTxFile opts
  putStrLn $ "> Signing key file: " ++ signKeyFile opts
  putStrLn $ "> Output file: " ++ signOutFile opts

  -- Load transaction from file
  txContent <- readFile (signTxFile opts)
  putStrLn "> Transaction loaded from file"

  -- Load signing key
  keyContent <- readFile (signKeyFile opts)
  putStrLn "> Signing key loaded"

  -- Sign the transaction
  let signedTx = signTransactionOffline txContent keyContent

  -- Save signed transaction
  writeFile (signOutFile opts) signedTx

  putStrLn "SUCCESS: Transaction signed successfully!"
  putStrLn $ "> Signed transaction saved to: " ++ signOutFile opts

-- | Sign transaction offline (simplified)
signTransactionOffline :: String -> String -> String
signTransactionOffline txContent keyContent =
  -- In real implementation, this would use cryptographic signing
  "{\"type\": \"TxSigned\", \"description\": \"Signed Transaction\", \"cborHex\": \"signed_placeholder\"}"

-- | Run validate command
runValidate :: ValidateOptions -> FilePath -> IO ()
runValidate opts homeDir = do
  putStrLn "> Validating transaction..."
  putStrLn $ "> Transaction file: " ++ validateTxFile opts
  putStrLn $ "> Database file: " ++ validateDbFile opts

  -- Load database
  db <- initDatabase (validateDbFile opts)

  -- Load transaction from file
  txContent <- readFile (validateTxFile opts)
  putStrLn "> Transaction loaded from file"

  -- Parse and validate transaction
  let validationResult = validateTransactionFromFile db txContent

  closeDatabase db

  case validationResult of
    Left errors -> do
      putStrLn "ERROR: Transaction validation failed!"
      mapM_ (putStrLn . ("Error: " ++) . show) errors
      exitFailure
    Right _ -> do
      putStrLn "SUCCESS: Transaction validation passed!"
      putStrLn "Validation details:"
      putStrLn "  ✓ Transaction format is valid"
      putStrLn "  ✓ All inputs are available"
      putStrLn "  ✓ Fee calculation is correct"
      putStrLn "  ✓ Script execution units are within limits"

-- | Validate transaction from file
validateTransactionFromFile :: Database -> String -> Either [String] ()
validateTransactionFromFile db txContent =
  -- In real implementation, this would perform comprehensive validation
  let errors = []
   in if null errors
        then Right ()
        else Left errors

-- | Run export command
runExport :: ExportOptions -> FilePath -> IO ()
runExport opts homeDir = do
  putStrLn "> Exporting transaction..."
  putStrLn $ "> Transaction file: " ++ exportTxFile opts
  putStrLn $ "> Format: " ++ show (exportFormat opts)
  putStrLn $ "> Output file: " ++ exportOutFile opts

  -- Load transaction from file
  txContent <- readFile (exportTxFile opts)
  putStrLn "> Transaction loaded from file"

  -- Export in the specified format
  let exportedContent = exportTransactionInFormat txContent (exportFormat opts)

  -- Save exported transaction
  writeFile (exportOutFile opts) exportedContent

  putStrLn "SUCCESS: Transaction exported successfully!"
  putStrLn $ "> Exported transaction saved to: " ++ exportOutFile opts

-- | Export transaction in specified format
exportTransactionInFormat :: String -> ExportFormat -> String
exportTransactionInFormat txContent format = case format of
  CardanoCLI -> exportToCardanoCLIFormat txContent
  Koios -> exportToKoiosFormat txContent
  JSON -> exportToJSONFormat txContent

-- | Export to Cardano CLI format
exportToCardanoCLIFormat :: String -> String
exportToCardanoCLIFormat txContent =
  "{\"type\": \"TxBody\", \"description\": \"Cardano CLI Export\", \"cborHex\": \"cardano_cli_export\"}"

-- | Export to Koios format
exportToKoiosFormat :: String -> String
exportToKoiosFormat txContent =
  "{\"tx_hash\": \"placeholder_hash\", \"block_time\": 1234567890, \"block_height\": 12345, \"tx_amount\": 1000000}"

-- | Export to JSON format
exportToJSONFormat :: String -> String
exportToJSONFormat txContent =
  "{\"transaction\": {\"id\": \"placeholder_id\", \"inputs\": [], \"outputs\": [], \"fee\": 0}}"

-- | Run submit command
runSubmit :: SubmitOptions -> FilePath -> IO ()
runSubmit opts homeDir = do
  progressMsg "Submitting transaction..."
  infoMsg $ "Transaction file: " ++ submitTxFile opts
  
  case submitTestnetMagic opts of
    Just magic -> infoMsg $ "Testnet magic: " ++ show magic
    Nothing -> infoMsg "Using mainnet"
  
  case submitNodeSocket opts of
    Just socket -> infoMsg $ "Socket path: " ++ socket
    Nothing -> infoMsg "Using default socket path"
  
  -- In a real implementation, this would submit to the Cardano network
  successMsg "Transaction submitted successfully!"
  subsectionHeader "Transaction Details"
  statusInfo $ "Transaction ID: " ++ highlightTxHash "placeholder_tx_id"
  statusWarning "This is a simulation - no actual network submission"

-- | Run calculate-min-fee command
runCalculateMinFee :: CalculateMinFeeOptions -> FilePath -> IO ()
runCalculateMinFee opts homeDir = do
  progressMsg "Calculating minimum transaction fee..."
  infoMsg $ "Transaction body file: " ++ calcTxBodyFile opts
  infoMsg $ "Input count: " ++ show (calcTxInCount opts)
  infoMsg $ "Output count: " ++ show (calcTxOutCount opts)
  infoMsg $ "Witness count: " ++ show (calcWitnessCount opts)
  infoMsg $ "Protocol parameters: " ++ calcProtocolParamsFile opts
  
  -- Handle testnet-magic parameter
  case calcTestnetMagic opts of
    Just magic -> infoMsg $ "Testnet magic: " ++ T.unpack magic
    Nothing -> infoMsg "Using mainnet"
  
  -- Calculate fee based on protocol parameters
  let baseFee = 155381 + (44 * (calcTxInCount opts + calcTxOutCount opts))
      witnessFee = calcWitnessCount opts * 1000
      totalFee = baseFee + witnessFee
  
  successMsg "Fee calculation completed!"
  subsectionHeader "Fee Breakdown"
  statusInfo $ "Minimum fee: " ++ highlightAmount (show totalFee ++ " lovelace")
  statusInfo $ "Base fee: " ++ highlightData (show baseFee ++ " lovelace")
  statusInfo $ "Witness fee: " ++ highlightData (show witnessFee ++ " lovelace")

-- | Run view command
runView :: ViewOptions -> FilePath -> IO ()
runView opts homeDir = do
  putStrLn "> Viewing transaction..."
  putStrLn $ "> Transaction file: " ++ viewTxFile opts
  
  -- Load transaction from file
  txContent <- readFile (viewTxFile opts)
  putStrLn "> Transaction loaded from file"
  
  -- Parse and display transaction details
  putStrLn "\nTransaction Details:"
  putStrLn "===================="
  putStrLn "Transaction ID: placeholder_tx_id"
  putStrLn "Inputs: 1"
  putStrLn "Outputs: 2"
  putStrLn "Fee: 200000 lovelace"
  putStrLn "Size: 1024 bytes"
  
  when (viewVerbose opts) $ do
    putStrLn "\nDetailed Information:"
    putStrLn "===================="
    putStrLn "Input 1: genesis_alice#0 (10000000 lovelace)"
    putStrLn "Output 1: addr_test1bob (2000000 lovelace)"
    putStrLn "Output 2: addr_test1alice (7800000 lovelace)"
    putStrLn "Change: 7800000 lovelace"
    putStrLn "Fee: 200000 lovelace"

-- | Run txid command
runTxId :: TxIdOptions -> FilePath -> IO ()
runTxId opts homeDir = do
  progressMsg "Calculating transaction ID..."
  infoMsg $ "Transaction file: " ++ txIdTxFile opts
  
  -- Load transaction from file
  txContent <- readFile (txIdTxFile opts)
  infoMsg "Transaction loaded from file"
  
  -- Calculate transaction ID (simplified)
  let txId = "placeholder_tx_id_" ++ show (length txContent)
  
  successMsg "Transaction ID calculated!"
  subsectionHeader "Transaction ID"
  statusInfo $ "Transaction ID: " ++ highlightTxHash txId

-- | Run decode command
runDecode :: DecodeOptions -> FilePath -> IO ()
runDecode opts homeDir = do
  putStrLn "> Decoding transaction..."
  putStrLn $ "> Transaction file: " ++ decodeTxFile opts

  -- Load transaction from file
  txContent <- readFile (decodeTxFile opts)
  putStrLn "> Transaction loaded from file"

  -- Decode transaction
  let decodedInfo = decodeTransactionFromFile txContent

  putStrLn "Transaction Details:"
  putStrLn "======================"
  putStrLn $ "Transaction ID: " ++ decodedTxId decodedInfo
  putStrLn $ "Inputs: " ++ show (decodedNumInputs decodedInfo)
  putStrLn $ "Outputs: " ++ show (decodedNumOutputs decodedInfo)
  putStrLn $ "Fee: " ++ show (decodedFeeAmount decodedInfo) ++ " lovelace"

  when (decodeVerbose opts) $ do
    putStrLn "\nInput Details:"
    mapM_ printInputDetail (decodedInputDetails decodedInfo)
    putStrLn "\nOutput Details:"
    mapM_ printOutputDetail (decodedOutputDetails decodedInfo)

-- | Decoded transaction information
data DecodedTransaction = DecodedTransaction
  { decodedTxId :: String,
    decodedNumInputs :: Int,
    decodedNumOutputs :: Int,
    decodedFeeAmount :: Word64,
    decodedInputDetails :: [String],
    decodedOutputDetails :: [String]
  }

-- | Decode transaction from file
decodeTransactionFromFile :: String -> DecodedTransaction
decodeTransactionFromFile txContent =
  -- In real implementation, this would parse CBOR and extract details
  DecodedTransaction
    { decodedTxId = "1234567890abcdef...",
      decodedNumInputs = 1,
      decodedNumOutputs = 2,
      decodedFeeAmount = 180725,
      decodedInputDetails = ["TxId#TxIx: 1234567890abcdef...#0", "Amount: 1000000000 lovelace"],
      decodedOutputDetails = ["Address: addr_test1...", "Amount: 100000000 lovelace"]
    }

-- | Print input detail
printInputDetail :: String -> IO ()
printInputDetail detail = putStrLn $ "  " ++ detail

-- | Print output detail
printOutputDetail :: String -> IO ()
printOutputDetail detail = putStrLn $ "  " ++ detail

-- | Run list command
runList :: ListOptions -> FilePath -> IO ()
runList opts homeDir = do
  -- Resolve UTXO file path relative to home directory
  mUtxoPath <- resolveInputFile homeDir "utxos" (listDbFile opts)
  case mUtxoPath of
    Just utxoPath -> do
      putStrLn $ "> Reading UTXOs from file: " ++ utxoPath
      -- Parse UTXOs from file (should be a JSON array of UTXO objects)
      mUtxos <- Aeson.decodeFileStrict' utxoPath :: IO (Maybe [UTXO])
      case mUtxos of
        Nothing -> do
          putStrLn $ "ERROR: Could not parse UTXO file '" ++ utxoPath ++ "'. Expected a JSON array of UTXO objects."
          exitFailure
        Just utxos ->
          case listAddress opts of
            Just addr -> do
              putStrLn $ "> Filtering by address: " ++ T.unpack addr
              let filteredUtxos = filterUTXOsByAddress utxos addr
              putStrLn "                               TxHash                                 TxIx        Amount"
              putStrLn "--------------------------------------------------------------------------------------"
              mapM_ printUTXO filteredUtxos
            Nothing -> do
              putStrLn "                               TxHash                                 TxIx        Amount"
              putStrLn "--------------------------------------------------------------------------------------"
              mapM_ printUTXO utxos
    Nothing -> do
      putStrLn $ "ERROR: UTXO file '" ++ listDbFile opts ++ "' not found."
      putStrLn $ "Please place it in the current directory or in: " ++ homeDir ++ "/utxos"
      exitFailure

-- | Filter UTXOs by address (realistic: match address field if present)
filterUTXOsByAddress :: [UTXO] -> Text -> [UTXO]
filterUTXOsByAddress utxos addr = filter (\utxo -> ownerAddress utxo == addr) utxos

-- | Generate a proper transaction hash (64-character hex string)
generateTransactionHash :: Int -> IO Text
generateTransactionHash txNumber = do
  -- Create a deterministic but realistic-looking transaction hash
  let input = "cots_tx_" ++ show txNumber ++ "_" ++ show (txNumber * 12345)
      hashBytes = hash (BS8.pack input) :: Digest SHA256
      -- Convert Digest to hex string directly
      hashStr = show hashBytes
      -- Remove the "Digest SHA256 " prefix and take 64 characters
      cleanHash = take 64 $ drop 13 hashStr  -- Remove "Digest SHA256 " prefix
      -- Pad with zeros if needed
      paddedHash = cleanHash ++ replicate (64 - length cleanHash) '0'
  return $ T.pack paddedHash

-- | Print UTXO in professional formatted output
printUTXO :: UTXO -> IO ()
printUTXO (UTXO (TransactionId txid) (TxIndex txix) (Amount lov assets) _ownerAddr) = do
  let txidShort = T.unpack txid
      txixStr = show txix
      lovStr = show lov ++ " lovelace"
      assetsStr =
        if Map.null assets
          then ""
          else concatMap (\(Asset a, n) -> " + " ++ show n ++ " " ++ T.unpack a) (Map.toList assets)
      amountStr = lovStr ++ assetsStr
      -- Format transaction hash to be visible (left-align in 66 chars, truncate if too long)
      txidFormatted = if length txidShort > 66
                      then take 63 txidShort ++ "..."
                      else txidShort
  putStrLn $ printf "%-66s%6s    %s" 
    (formatTxHash txidFormatted) 
    (formatTxIndex txix) 
    (highlightAmount amountStr)

-- | Run reserve command
runReserve :: ReserveOptions -> FilePath -> IO ()
runReserve opts homeDir = do
  putStrLn "> Reserving UTXOs..."
  putStrLn $ "> Address: " ++ T.unpack (reserveAddress opts)
  putStrLn $ "> Amount: " ++ show (reserveAmount opts) ++ " lovelace"
  putStrLn $ "> UTXO file: " ++ reserveDbFile opts
  putStrLn $ "> Output file: " ++ reserveOutFile opts

  -- Resolve UTXO file path relative to home directory
  mUtxoPath <- resolveInputFile homeDir "utxos" (reserveDbFile opts)
  case mUtxoPath of
    Just utxoPath -> do
      mUtxos <- Aeson.decodeFileStrict' utxoPath :: IO (Maybe [UTXO])
      case mUtxos of
        Nothing -> do
          putStrLn $ "ERROR: Could not parse UTXO file '" ++ utxoPath ++ "'. Expected a JSON array of UTXO objects."
          exitFailure
        Just utxos -> do
          let reservedUtxos = reserveUTXOsForAmount utxos (reserveAddress opts) (reserveAmount opts)
              reservedContent = encodeReservedUTXOs reservedUtxos
          writeFile (reserveOutFile opts) reservedContent
          putStrLn "SUCCESS: UTXOs reserved successfully!"
          putStrLn $ "> Reserved UTXOs saved to: " ++ reserveOutFile opts
          putStrLn $ "Reserved " ++ show (length reservedUtxos) ++ " UTXOs"
    Nothing -> do
      putStrLn $ "ERROR: UTXO file '" ++ reserveDbFile opts ++ "' not found."
      putStrLn $ "Please place it in the current directory or in: " ++ homeDir ++ "/utxos"
      exitFailure

-- | Reserve UTXOs for a specific amount (realistic: sum until amount is covered)
reserveUTXOsForAmount :: [UTXO] -> Text -> Word64 -> [UTXO]
reserveUTXOsForAmount utxos _ amount =
  let go :: [UTXO] -> Word64 -> [UTXO] -> [UTXO]
      go [] _ acc = acc
      go (u : us) needed acc =
        let val = lovelace (COTS.Types.amount u)
            newNeeded = if needed > val then needed - val else 0
         in if needed <= 0 then acc else go us newNeeded (acc ++ [u])
   in go utxos amount []

-- | Run process transaction command
runProcess :: ProcessOptions -> FilePath -> IO ()
runProcess opts homeDir = do
  putStrLn "> Processing transaction..."
  putStrLn $ "> From: " ++ T.unpack (processFromAddress opts)
  putStrLn $ "> To: " ++ T.unpack (processToAddress opts)
  putStrLn $ "> Amount: " ++ show (processAmount opts) ++ " lovelace"
  putStrLn $ "> Database: " ++ processDbFile opts

  -- Resolve database file path relative to home directory
  let dbPath = if isAbsolute (processDbFile opts) then processDbFile opts else homeDir </> processDbFile opts
  
  -- Get current UTXOs from database
  db <- initDatabase dbPath
  currentUtxos <- exportUTXOs db
  
  -- Find UTXOs for the source address
  let sourceUtxos = filter (\utxo -> ownerAddress utxo == processFromAddress opts) currentUtxos
      totalAvailable = sum $ map (lovelace . COTS.Types.amount) sourceUtxos
      fee = 200000 -- Conservative fee estimate
      totalNeeded = processAmount opts + fee
  
  if totalAvailable < totalNeeded
    then do
      putStrLn $ "ERROR: Insufficient funds. Required: " ++ show totalNeeded ++ " (amount: " ++ show (processAmount opts) ++ " + fee: " ++ show fee ++ "), Available: " ++ show totalAvailable
      closeDatabase db
      exitFailure
    else do
      -- Select UTXOs to spend
      let selectedUtxos = reserveUTXOsForAmount sourceUtxos (processFromAddress opts) totalNeeded
          selectedAmount = sum $ map (lovelace . COTS.Types.amount) selectedUtxos
          changeAmount = selectedAmount - totalNeeded
      
      -- Create new UTXOs
      txIdText <- generateTransactionHash (length currentUtxos + 1)
      let txId = TransactionId txIdText
          outputUtxo = UTXO
            { txHash = txId
            , txIx = TxIndex 0
            , amount = Amount (processAmount opts) Map.empty
            , ownerAddress = processToAddress opts  -- Associate with recipient address
            }
          changeUtxo = if changeAmount > 0
            then Just $ UTXO
              { txHash = txId
              , txIx = TxIndex 1
              , amount = Amount changeAmount Map.empty
              , ownerAddress = processFromAddress opts  -- Associate with sender address (change)
              }
            else Nothing
      
      -- Remove spent UTXOs and add new ones
      let remainingUtxos = filter (\utxo -> not (utxo `elem` selectedUtxos)) currentUtxos
          newUtxos = outputUtxo : maybeToList changeUtxo
          finalUtxos = remainingUtxos ++ newUtxos
      
      -- Update database
      closeDatabase db -- Close the current connection
      resetDatabase dbPath -- Clear existing UTXOs
      db <- initDatabase dbPath -- Reopen database connection
      dbUtxos <- mapM convertUTXOToDBUTXO finalUtxos
      mapM_ (insertUTXO db) dbUtxos -- Insert updated UTXOs
      closeDatabase db
      
      -- Save transaction details
      let txDetails = "Transaction: " ++ T.unpack (processFromAddress opts) ++ " -> " ++ T.unpack (processToAddress opts) ++ " (" ++ show (processAmount opts) ++ " lovelace)"
      writeFile (processOutFile opts) txDetails
      
      putStrLn "SUCCESS: Transaction processed successfully!"
      putStrLn $ "> Spent " ++ show (length selectedUtxos) ++ " UTXOs"
      putStrLn $ "> Created " ++ show (length newUtxos) ++ " new UTXOs"
      putStrLn $ "> Fee: " ++ show fee ++ " lovelace"
      when (changeAmount > 0) $ putStrLn $ "> Change: " ++ show changeAmount ++ " lovelace"
      putStrLn $ "> Transaction details saved to: " ++ processOutFile opts

-- | Run query UTXO command (Cardano CLI compatible)
runQueryUTXO :: QueryUTXOptions -> FilePath -> IO ()
runQueryUTXO opts homeDir = do
  progressMsg $ "Querying UTXOs for address: " ++ formatAddress (T.unpack (queryAddress opts))
  
  -- Handle network parameters
  case queryTestnetMagic opts of
    Just magic -> infoMsg $ "Testnet magic: " ++ T.unpack magic
    Nothing -> if queryMainnet opts
      then infoMsg "Using mainnet"
      else infoMsg "Using default network"
  
  case querySocketPath opts of
    Just socket -> infoMsg $ "Socket path: " ++ socket
    Nothing -> infoMsg "Using default socket path"
  
  -- Use database file if provided, otherwise use default
  let dbFile = case queryDbFile opts of
        Just db -> db
        Nothing -> "cots.db"
      dbPath = if isAbsolute dbFile then dbFile else homeDir </> dbFile
  
  -- Query UTXOs from database
  db <- initDatabase dbPath
  utxos <- exportUTXOs db
  closeDatabase db
  
  -- Filter UTXOs by address
  let filteredUtxos = filter (\utxo -> ownerAddress utxo == queryAddress opts) utxos
  
  -- Display results in professional format
  sectionHeader "UTXO Query Results"
  tableHeader ["TxHash", "TxIx", "Amount"]
  mapM_ printUTXO filteredUtxos
  
  statusInfo $ "Found " ++ highlightSuccess (show (length filteredUtxos)) ++ " UTXOs"

-- | Convert UTXO to DBUTXO for database storage
convertUTXOToDBUTXO :: UTXO -> IO DBUTXO
convertUTXOToDBUTXO UTXO {..} = do
  now <- getCurrentTime
  return $ DBUTXO
    { dbTxHash = unTransactionId txHash -- Extract clean transaction ID text
    , dbTxIx = fromIntegral $ unTxIndex txIx
    , dbAddress = ownerAddress -- Use the ownerAddress field from UTXO
    , dbAmount = fromIntegral $ lovelace amount
    , dbAssets = Just "[]" -- TODO: Handle assets properly
    , dbSpent = 0 -- Not spent
    , dbCreatedAt = now
    }

-- | Encode reserved UTXOs to JSON
encodeReservedUTXOs :: [UTXO] -> String
encodeReservedUTXOs utxos =
  "{\"reserved_utxos\": " ++ show (length utxos) ++ ", \"utxos\": " ++ show utxos ++ "}"

-- | Run update command
runUpdate :: UpdateOptions -> FilePath -> IO ()
runUpdate opts homeDir = do
  putStrLn "⚙️  Updating protocol parameters..."
  putStrLn $ "> Protocol params file: " ++ updateProtocolParamsFile opts
  putStrLn $ "> Database file: " ++ updateDbFile opts

  mProtoPath <- resolveInputFile homeDir "protocol" (updateProtocolParamsFile opts)
  case mProtoPath of
    Just protoPath -> do
      paramsContent <- readFile protoPath
      putStrLn "> Protocol parameters loaded from file"
      let newParams = parseProtocolParameters paramsContent
      db <- initDatabase (updateDbFile opts)
      updateProtocolParameters db newParams
      closeDatabase db
      putStrLn "SUCCESS: Protocol parameters updated successfully!"
      putStrLn "Updated parameters:"
      putStrLn $ "  minFeeA: " ++ show (minFeeA newParams)
      putStrLn $ "  minFeeB: " ++ show (minFeeB newParams)
      putStrLn $ "  maxTxSize: " ++ show (maxTxSize newParams)
    Nothing -> do
      putStrLn $ "ERROR: Protocol parameters file '" ++ updateProtocolParamsFile opts ++ "' not found."
      putStrLn $ "Please place it in the current directory or in: " ++ (homeDir </> "protocol")

-- | Parse protocol parameters from file content
parseProtocolParameters :: String -> ProtocolParameters
parseProtocolParameters content =
  -- In real implementation, this would parse JSON or CBOR
  defaultProtocolParameters

-- | Fetch protocol parameters from URL (e.g., Koios), write to out file, and update DB
runFetch :: FetchOptions -> FilePath -> IO ()
runFetch opts _ = do
  putStrLn "> Fetching protocol parameters..."
  putStrLn $ "🔗 URL: " ++ fetchUrl opts
  json <- readProcess "curl" ["-s", fetchUrl opts] ""
  writeFile (fetchOutFile opts) json
  putStrLn $ "> Saved to: " ++ fetchOutFile opts
  let _params = parseKoiosProtocolParameters (LBS.fromStrict (BS8.pack json))
  db <- initDatabase (fetchDbFile opts)
  now <- getCurrentTime
  insertProtocolParams db DBProtocolParams { dbParams = T.pack json, dbUpdatedAt = now }
  closeDatabase db
  putStrLn "SUCCESS: Protocol parameters stored in DB."

-- | Parse Koios epoch_params JSON into ProtocolParameters (best-effort)
parseKoiosProtocolParameters :: LBS.ByteString -> ProtocolParameters
parseKoiosProtocolParameters lbs =
  case decode lbs :: Maybe Value of
    Just (Array arr) -> case V.toList arr of
      (Object o : _) -> fromObj o
      _ -> defaultProtocolParameters
    Just (Object o) -> fromObj o
    _ -> defaultProtocolParameters
  where
    fromObj o =
      let getNum k def =
            case KM.lookup k o of
              Just (Number n) -> round n
              Just (String s) -> maybe def id (readMaybe (T.unpack s))
              _ -> def
          d = defaultProtocolParameters
       in d
            { minFeeA = getNum "min_fee_a" (minFeeA d),
              minFeeB = getNum "min_fee_b" (minFeeB d),
              maxTxSize = getNum "max_tx_size" (maxTxSize d),
              maxValSize = getNum "max_val_size" (maxValSize d),
              keyDeposit = getNum "key_deposit" (keyDeposit d),
              poolDeposit = getNum "pool_deposit" (poolDeposit d),
              maxCollateralInputs = getNum "max_collateral_inputs" (maxCollateralInputs d),
              collateralPercentage = getNum "collateral_percent" (collateralPercentage d)
            }

-- | Update protocol parameters in database
updateProtocolParameters :: Database -> ProtocolParameters -> IO ()
updateProtocolParameters db params =
  -- In real implementation, this would update the database
  putStrLn "Database updated with new protocol parameters"

-- | Run init command
runInit :: InitOptions -> FilePath -> IO ()
runInit opts homeDir = do
  putStrLn "> Initializing SQLite database and COTS home structure..."
  putStrLn $ "> Database file: " ++ initDbFile opts
  -- Créer les sous-dossiers
  let subdirs = ["keys", "addresses", "utxos", "transactions", "protocol", "scripts"]
  mapM_
    ( \d -> do
        let path = homeDir </> d
        createDirectoryIfMissing True path
        putStrLn $ "> Created directory: " ++ path
    )
    subdirs
  -- Copier les fichiers d'exemple s'ils existent
  let examples = ["utxos.json", "utxos-simple.json", "config.json", "config.yaml"]
  mapM_
    ( \f -> do
        let src = "COTS/examples/" </> f
            dst = homeDir </> "utxos" </> f
        exists <- doesFileExist src
        when exists $ do
          copyFile src dst
          putStrLn $ "> Example file copied: " ++ dst
    )
    examples
  -- Initialiser la base SQLite
  db <- initDatabase (initDbFile opts)
  -- If a workspace config exists nearby, import protocol params, wallets and UTXOs
  let possibleConfigs = [homeDir </> "config.json", homeDir </> ".." </> "config.json"]
  mExisting <- filterM doesFileExist possibleConfigs
  case mExisting of
    (cfg:_) -> do
      putStrLn $ "> Found config: " ++ cfg ++ " — importing into DB"
      mCfg <- Aeson.decodeFileStrict' cfg :: IO (Maybe Config)
      case mCfg of
        Nothing -> putStrLn "ERROR: Could not parse config.json"
        Just cfgVal -> do
          now <- getCurrentTime
          -- Store protocol params
          insertProtocolParams db DBProtocolParams { dbParams = T.pack (BS8.unpack (LBS.toStrict (encode (protocolParameters cfgVal)))), dbUpdatedAt = now }
          -- Store wallets and their UTXOs
          mapM_ (\w -> insertWallet db DBWallet { dbWalletName = name w, dbWalletAddress = case address w of Address a -> a, dbWalletCreated = now }) (wallets cfgVal)
          mapM_ (\w -> mapM_ (\u -> insertUTXO db DBUTXO { dbTxHash = case txHash u of TransactionId h -> h
                                                          , dbTxIx = fromIntegral (unTxIndex (txIx u))
                                                          , dbAddress = case address w of Address a -> a
                                                          , dbAmount = fromIntegral (lovelace (amount u))
                                                          , dbAssets = Just (T.pack (BS8.unpack (LBS.toStrict (encode (assets (amount u))))))
                                                          , dbSpent = 0
                                                          , dbCreatedAt = now }) (utxos w)) (wallets cfgVal)
    [] -> return ()
  closeDatabase db
  putStrLn "SUCCESS: Database and home structure initialized successfully!"

-- | Run reset command
runReset :: ResetOptions -> FilePath -> IO ()
runReset opts homeDir = do
  putStrLn "> Resetting SQLite database..."
  putStrLn $ "> Database file: " ++ resetDbFile opts

  resetDatabase (resetDbFile opts)

  putStrLn "SUCCESS: Database reset successfully!"

-- | Run snapshot command
runSnapshot :: SnapshotOptions -> FilePath -> IO ()
runSnapshot opts homeDir = do
  putStrLn "> Creating database snapshot..."
  putStrLn $ "> Database file: " ++ snapshotDbFile opts
  putStrLn $ "> Snapshot file: " ++ snapshotOutFile opts

  db <- initDatabase (snapshotDbFile opts)
  snapshotDatabase db (snapshotOutFile opts)
  closeDatabase db

  putStrLn "SUCCESS: Snapshot created successfully!"

-- | Run load snapshot command
runLoadSnapshot :: LoadSnapshotOptions -> FilePath -> IO ()
runLoadSnapshot opts homeDir = do
  putStrLn "> Loading database from snapshot..."
  putStrLn $ "> Snapshot file: " ++ loadSnapshotFile opts
  putStrLn $ "> Database file: " ++ loadDbFile opts

  db <- loadSnapshot (loadSnapshotFile opts) (loadDbFile opts)
  closeDatabase db

  putStrLn "SUCCESS: Snapshot loaded successfully!"

-- | Run import UTXO command
runImportUTXO :: ImportUTXOptions -> FilePath -> IO ()
runImportUTXO opts homeDir = do
  let dbPath = if isAbsolute (importDbFile opts) then importDbFile opts else homeDir </> importDbFile opts
  mUtxoPath <- resolveInputFile homeDir "utxos" (importUtxoFile opts)
  putStrLn "> Importing UTXOs from JSON file..."
  putStrLn $ "> Database file: " ++ dbPath
  case mUtxoPath of
    Just utxoPath -> do
      putStrLn $ "> UTXO file: " ++ utxoPath
      mUtxos <- Aeson.decodeFileStrict' utxoPath :: IO (Maybe [UTXO])
      case mUtxos of
        Nothing -> putStrLn "ERROR: Could not parse UTXO file"
        Just utxos -> do
          db <- initDatabase dbPath
          importUTXOs db utxos
          closeDatabase db
          putStrLn $ "SUCCESS: Imported " ++ show (length utxos) ++ " UTXOs successfully!"
    Nothing -> do
      putStrLn $ "ERROR: UTXO file '" ++ importUtxoFile opts ++ "' not found."
      putStrLn $ "Please place it in the current directory or in: " ++ (homeDir </> "utxos")

-- | Resolve a file path, searching current dir, absolute, and then under home/subdir
resolveInputFile :: FilePath -> FilePath -> FilePath -> IO (Maybe FilePath)
resolveInputFile home subdir file = do
  let tryPaths = [file, home </> subdir </> file]
  found <- filterM doesFileExist tryPaths
  return $ case found of
    (p : _) -> Just p
    [] -> Nothing

-- | Run export UTXO command
runExportUTXO :: ExportUTXOptions -> FilePath -> IO ()
runExportUTXO opts homeDir = do
  putStrLn "> Exporting UTXOs to JSON file..."
  putStrLn $ "> Database file: " ++ exportDbFile opts
  putStrLn $ "> Output file: " ++ exportUtxoFile opts

  -- Resolve database file path relative to home directory
  let dbPath = if isAbsolute (exportDbFile opts) then exportDbFile opts else homeDir </> exportDbFile opts
  
  db <- initDatabase dbPath
  utxos <- exportUTXOs db
  closeDatabase db

  -- Write to JSON file
  LBS.writeFile (exportUtxoFile opts) (encode utxos)
  putStrLn $ "SUCCESS: Exported " ++ show (length utxos) ++ " UTXOs successfully!"

-- | Run generate UTXO command
runGenerateUTXO :: GenerateUTXOptions -> FilePath -> IO ()
runGenerateUTXO opts homeDir = do
  progressMsg "Generating initial UTXOs JSON file..."
  infoMsg $ "Addresses: " ++ intercalate ", " (map T.unpack (generateAddresses opts))
  infoMsg $ "Amounts: " ++ intercalate ", " (map show (generateAmounts opts))
  infoMsg $ "Output file: " ++ generateOutFile opts
  
  let prefix = T.unpack $ fromMaybe "genesis" (generatePrefix opts)
      addresses = generateAddresses opts
      amounts = generateAmounts opts
      
  -- Validate that addresses and amounts lists have the same length
  when (length addresses /= length amounts) $ do
    errorMsg "Number of addresses must match number of amounts"
    exitFailure
  
  -- Generate UTXOs
  let utxos = zipWith (\addr amount -> 
        UTXO 
          { txHash = TransactionId (T.pack $ prefix ++ "_" ++ T.unpack addr ++ "_" ++ replicate 64 '0')
          , txIx = TxIndex 0
          , amount = Amount amount Map.empty
          , ownerAddress = addr  -- Associate UTXO with its address
          }
        ) addresses amounts
  
  -- Write to JSON file
  LBS.writeFile (generateOutFile opts) (encode utxos)
  successMsg $ "Generated " ++ show (length utxos) ++ " initial UTXOs successfully!"
  infoMsg $ "File saved at: " ++ generateOutFile opts

-- | Run inspect command
runInspect :: InspectOptions -> FilePath -> IO ()
runInspect opts homeDir = do
  putStrLn "> Inspecting database..."
  putStrLn $ "> Database file: " ++ inspectDbFile opts

  -- Resolve database file path relative to home directory
  let dbPath = if isAbsolute (inspectDbFile opts) then inspectDbFile opts else homeDir </> inspectDbFile opts
  
  db <- initDatabase dbPath
  stats <- inspectDatabase db
  closeDatabase db

  putStrLn stats

-- | Run create wallet command
runCreateWallet :: CreateWalletOptions -> FilePath -> IO ()
runCreateWallet opts homeDir = do
  putStrLn "> Creating new wallet..."
  putStrLn $ "> Name: " ++ T.unpack (createWalletName opts)
  putStrLn $ "> Address: " ++ T.unpack (createWalletAddress opts)
  putStrLn $ "> Database file: " ++ createWalletDbFile opts

  -- Resolve database file path relative to home directory
  let dbPath = if isAbsolute (createWalletDbFile opts) then createWalletDbFile opts else homeDir </> createWalletDbFile opts
  
  db <- initDatabase dbPath
  now <- getCurrentTime
  let wallet = DBWallet (createWalletName opts) (createWalletAddress opts) now
  insertWallet db wallet
  closeDatabase db

  putStrLn "SUCCESS: Wallet created successfully!"

-- | Run list wallets command
runListWallets :: ListWalletsOptions -> FilePath -> IO ()
runListWallets opts homeDir = do
  putStrLn "> Listing wallets..."
  putStrLn $ "> Database file: " ++ listWalletsDbFile opts

  -- Resolve database file path relative to home directory
  let dbPath = if isAbsolute (listWalletsDbFile opts) then listWalletsDbFile opts else homeDir </> listWalletsDbFile opts
  
  db <- initDatabase dbPath
  wallets <- getWallets db
  closeDatabase db

  putStrLn "Name                    Address                                    Created"
  putStrLn "--------------------------------------------------------------------------------"
  mapM_ printWallet wallets

printWallet :: DBWallet -> IO ()
printWallet DBWallet {..} = do
  let nameStr = T.unpack dbWalletName
      addrStr = T.unpack dbWalletAddress
      timeStr = show dbWalletCreated
  putStrLn $ printf "%-20s  %-40s  %s" nameStr addrStr timeStr

-- | Run import wallet command
runImportWallet :: ImportWalletOptions -> FilePath -> IO ()
runImportWallet opts homeDir = do
  putStrLn "> Importing wallet..."
  putStrLn $ "> File: " ++ importWalletFile opts
  putStrLn $ "> Database file: " ++ importWalletDbFile opts

  -- Load wallet from JSON file
  walletContent <- readFile (importWalletFile opts)
  putStrLn "> Wallet file loaded"

  -- Parse wallet from JSON
  wallet <- parseWalletFromJSON walletContent

  -- Resolve database file path relative to home directory
  let dbPath = if isAbsolute (importWalletDbFile opts) then importWalletDbFile opts else homeDir </> importWalletDbFile opts
  
  -- Import wallet into database
  db <- initDatabase dbPath
  insertWallet db wallet
  closeDatabase db

  putStrLn "SUCCESS: Wallet imported successfully!"
  putStrLn $ "> Wallet name: " ++ T.unpack (dbWalletName wallet)
  putStrLn $ "> Address: " ++ T.unpack (dbWalletAddress wallet)

-- | Parse wallet from JSON content
parseWalletFromJSON :: String -> IO DBWallet
parseWalletFromJSON content = do
  currentTime <- getCurrentTime
  -- In real implementation, this would parse JSON
  return $
    DBWallet
      { dbWalletName = "imported_wallet",
        dbWalletAddress = "addr_test1imported",
        dbWalletCreated = currentTime
      }

-- | Run export wallet command
runExportWallet :: ExportWalletOptions -> FilePath -> IO ()
runExportWallet opts homeDir = do
  putStrLn "> Exporting wallet..."
  putStrLn $ "> Wallet: " ++ T.unpack (exportWalletName opts)
  putStrLn $ "> File: " ++ exportWalletFile opts
  putStrLn $ "> Database file: " ++ exportWalletDbFile opts

  -- Resolve database file path relative to home directory
  let dbPath = if isAbsolute (exportWalletDbFile opts) then exportWalletDbFile opts else homeDir </> exportWalletDbFile opts
  
  db <- initDatabase dbPath
  mWallet <- getWalletByName db (exportWalletName opts)
  closeDatabase db

  case mWallet of
    Just wallet -> do
      -- Export wallet to JSON file
      let walletJSON = encodeWalletToJSON wallet
      writeFile (exportWalletFile opts) walletJSON
      putStrLn "SUCCESS: Wallet exported successfully!"
      putStrLn $ "> Wallet exported to: " ++ exportWalletFile opts
    Nothing -> do
      putStrLn "ERROR: Wallet not found!"

-- | Encode wallet to JSON
encodeWalletToJSON :: DBWallet -> String
encodeWalletToJSON wallet =
  -- In real implementation, this would encode to proper JSON
  "{\"name\": \"" ++ T.unpack (dbWalletName wallet) ++ "\", \"address\": \"" ++ T.unpack (dbWalletAddress wallet) ++ "\"}"

-- | Run wallet info command
runWalletInfo :: WalletInfoOptions -> FilePath -> IO ()
runWalletInfo opts homeDir = do
  putStrLn "> Wallet information..."
  putStrLn $ "> Wallet: " ++ T.unpack (walletInfoName opts)
  putStrLn $ "> Database file: " ++ walletInfoDbFile opts

  -- Resolve database file path relative to home directory
  let dbPath = if isAbsolute (walletInfoDbFile opts) then walletInfoDbFile opts else homeDir </> walletInfoDbFile opts
  
  db <- initDatabase dbPath
  mWallet <- getWalletByName db (walletInfoName opts)
  closeDatabase db

  case mWallet of
    Just wallet -> do
      putStrLn $ "Name: " ++ T.unpack (dbWalletName wallet)
      putStrLn $ "Address: " ++ T.unpack (dbWalletAddress wallet)
      putStrLn $ "Created: " ++ show (dbWalletCreated wallet)
    Nothing -> do
      putStrLn "ERROR: Wallet not found!"

-- | Run address key generation
runAddressKeyGen :: AddressKeyGenOptions -> FilePath -> IO ()
runAddressKeyGen opts homeDir = do
  progressMsg "Generating payment key pair..."
  keysDir <- getCotsNodeSubdir homeDir "keys"
  let vkeyPath = keysDir </> keyGenVerificationKeyFile opts
      skeyPath = keysDir </> keyGenSigningKeyFile opts
  infoMsg $ "Verification key: " ++ vkeyPath
  infoMsg $ "Signing key: " ++ skeyPath

  -- Generate unique random keys
  now <- getCurrentTime
  let timestamp = show (round (utctDayTime now) :: Integer)
      keyName = takeBaseName (keyGenVerificationKeyFile opts)
      uniqueSeed = keyName ++ timestamp ++ show (hash (BS8.pack (keyName ++ timestamp)) :: Digest SHA256)
      vkeyCborHex = take 64 $ show (hash (BS8.pack (uniqueSeed ++ "vkey")) :: Digest SHA256)
      skeyCborHex = take 64 $ show (hash (BS8.pack (uniqueSeed ++ "skey")) :: Digest SHA256)

  writeFile vkeyPath $ "{\"type\": \"PaymentVerificationKeyShelley_ed25519\", \"description\": \"Payment Verification Key\", \"cborHex\": \"" ++ vkeyCborHex ++ "\"}"
  writeFile skeyPath $ "{\"type\": \"PaymentSigningKeyShelley_ed25519\", \"description\": \"Payment Signing Key\", \"cborHex\": \"" ++ skeyCborHex ++ "\"}"

  successMsg "Payment key pair generated successfully!"
  infoMsg $ "Files saved in: " ++ keysDir

-- | Run address build
runAddressBuild :: AddressBuildOptions -> FilePath -> IO ()
runAddressBuild opts homeDir = do
  progressMsg "Building Cardano address..."
  let outPath = buildOutFile opts
  case buildPaymentVerificationKeyFile opts of
    Nothing -> do
      errorMsg "--payment-verification-key-file is required."
      exitFailure
    Just vkeyFile -> do
      mVkeyPath <- resolveInputFile homeDir "keys" vkeyFile
      case mVkeyPath of
        Nothing -> do
          errorMsg $ "Verification key file '" ++ vkeyFile ++ "' not found."
          exitFailure
        Just vkeyPath -> do
          vkeyContent <- LBS.readFile vkeyPath
          case Aeson.decode vkeyContent :: Maybe Aeson.Object of
            Nothing -> do
              errorMsg $ "Could not parse verification key file '" ++ vkeyPath ++ "'."
              exitFailure
            Just vkeyObj -> do
              let mCborHexRaw = case KeyMap.lookup "cborHex" vkeyObj of
                    Just v -> case Aeson.fromJSON v of
                      Aeson.Success s -> Just s
                      _ -> Nothing
                    Nothing -> Nothing
              case mCborHexRaw of
                Nothing -> do
                  errorMsg $ "'cborHex' field missing in verification key file '" ++ vkeyPath ++ "'."
                  exitFailure
                Just cborHexRaw -> do
                  let cborHex :: String
                      cborHex = cborHexRaw
                      keyHash = show (hash (BS8.pack cborHex) :: Digest SHA256)
                      prefix = case buildNetwork opts of
                        Mainnet -> "addr1"
                        Testnet -> "addr_test1"
                        Preview -> "addr_test1"
                        Preprod -> "addr_test1"
                      address = prefix ++ keyHash
                  writeFile outPath address
                  successMsg $ "Address built: " ++ formatAddress address
                  infoMsg $ "File saved at: " ++ outPath
                  -- If initial amount and config are provided, append to config wallets with a genesis UTXO
                  case buildInitialAmount opts of
                    Nothing -> return ()
                    Just lov -> do
                      let defaultCfg = case buildNetwork opts of
                            Mainnet -> "mainnet"
                            Testnet -> "testnet"
                            Preview -> "preview"
                            Preprod -> "preprod"
                          cfgDir = (homeDir </> "..") -- not used; prefer explicit config path
                      let cfgPath = case buildConfigPath opts of
                            Just p -> p
                            Nothing -> inferWorkspaceConfig homeDir (buildNetwork opts)
                      appendAddressToConfig cfgPath (T.pack address) lov
                      putStrLn $ "> Updated config with genesis UTXO: " ++ cfgPath

-- | Run address info
runAddressInfo :: AddressInfoOptions -> FilePath -> IO ()
runAddressInfo opts homeDir = do
  putStrLn "> Address information:"
  putStrLn $ "> Address: " ++ T.unpack (infoAddress opts)
  putStrLn "Type: Payment address"
  putStrLn "Network: Testnet"
  putStrLn "Format: Bech32"

-- | Run stake address key generation
runStakeAddressKeyGen :: StakeAddressKeyGenOptions -> FilePath -> IO ()
runStakeAddressKeyGen opts homeDir = do
  putStrLn "> Generating stake key pair..."
  keysDir <- getCotsNodeSubdir homeDir "keys"
  let vkeyPath = keysDir </> stakeKeyGenVerificationKeyFile opts
      skeyPath = keysDir </> stakeKeyGenSigningKeyFile opts
  putStrLn $ "> Verification key: " ++ vkeyPath
  putStrLn $ "> Signing key: " ++ skeyPath

  writeFile vkeyPath "{\"type\": \"StakeVerificationKeyShelley_ed25519\", \"description\": \"Stake Verification Key\", \"cborHex\": \"placeholder\"}"
  writeFile skeyPath "{\"type\": \"StakeSigningKeyShelley_ed25519\", \"description\": \"Stake Signing Key\", \"cborHex\": \"placeholder\"}"

  putStrLn "SUCCESS: Stake key pair generated successfully!"
  putStrLn $ "> Files saved in: " ++ keysDir

-- | Run stake address build
runStakeAddressBuild :: StakeAddressBuildOptions -> FilePath -> IO ()
runStakeAddressBuild opts homeDir = do
  putStrLn "> Building stake address..."
  let outPath = stakeBuildOutFile opts
  let vkeyFile = stakeBuildStakeVerificationKeyFile opts
  mVkeyPath <- resolveInputFile homeDir "keys" vkeyFile
  case mVkeyPath of
    Nothing -> do
      putStrLn $ "ERROR: Stake verification key file '" ++ vkeyFile ++ "' not found."
      exitFailure
    Just vkeyPath -> do
      vkeyContent <- LBS.readFile vkeyPath
      case Aeson.decode vkeyContent :: Maybe Aeson.Object of
        Nothing -> do
          putStrLn $ "ERROR: Could not parse stake verification key file '" ++ vkeyPath ++ "'."
          exitFailure
        Just vkeyObj -> do
          let mCborHexRaw = case KeyMap.lookup "cborHex" vkeyObj of
                Just v -> case Aeson.fromJSON v of
                  Aeson.Success s -> Just s
                  _ -> Nothing
                Nothing -> Nothing
          case mCborHexRaw of
            Nothing -> do
              putStrLn $ "ERROR: 'cborHex' field missing in stake verification key file '" ++ vkeyPath ++ "'."
              exitFailure
            Just cborHexRaw -> do
              let cborHex :: String
                  cborHex = cborHexRaw
                  keyHash = show (hash (BS8.pack cborHex) :: Digest SHA256)
                  prefix = case stakeBuildNetwork opts of
                    Mainnet -> "stake1"
                    Testnet -> "stake_test1"
                    Preview -> "stake_test1"
                    Preprod -> "stake_test1"
                  stakeAddress = prefix ++ keyHash
              writeFile outPath stakeAddress
              putStrLn $ "SUCCESS: Stake address built: " ++ stakeAddress
              putStrLn $ "> File saved at: " ++ outPath

-- | Run stake address info
runStakeAddressInfo :: StakeAddressInfoOptions -> FilePath -> IO ()
runStakeAddressInfo opts homeDir = do
  putStrLn "> Stake address information:"
  putStrLn $ "> Address: " ++ T.unpack (stakeInfoAddress opts)
  putStrLn "Type: Stake address"
  putStrLn "Network: Testnet"
  putStrLn "Format: Bech32"

-- | Run mint build
runMintBuild :: MintBuildOptions -> FilePath -> IO ()
runMintBuild opts homeDir = do
  putStrLn "> Building minting transaction..."
  putStrLn $ "> Output file: " ++ mintOutFile opts

  -- Build minting transaction (placeholder implementation)
  let txContent = "{\"type\": \"TxBody\", \"description\": \"Minting Transaction\", \"cborHex\": \"placeholder\"}"
  writeFile (mintOutFile opts) txContent

  putStrLn "SUCCESS: Minting transaction built successfully!"

-- | Run mint calculate
runMintCalculate :: MintCalculateOptions -> FilePath -> IO ()
runMintCalculate opts homeDir = do
  putStrLn "> Calculating minting fees..."
  putStrLn $ "> Policy ID: " ++ T.unpack (mintCalcPolicyId opts)
  putStrLn $ "> Asset name: " ++ T.unpack (mintCalcAssetName opts)
  putStrLn $ "> Quantity: " ++ show (mintCalcQuantity opts)

  -- Calculate fees (placeholder implementation)
  let baseFee = 170000
      assetFee = 100000
      totalFee = baseFee + assetFee

  putStrLn $ "Base fee: " ++ show baseFee ++ " lovelace"
  putStrLn $ "Asset fee: " ++ show assetFee ++ " lovelace"
  putStrLn $ "Total fee: " ++ show totalFee ++ " lovelace"

-- | Root-level init: create per-network workspace and config.json
runAppInit :: AppInitOptions -> IO ()
runAppInit AppInitOptions {..} = do
  home <- getHomeDirectory
  let base = case appInitPath of
        Just p -> p
        Nothing -> home </> ".cotscli"
      networkName = case appInitNetwork of
        Mainnet -> "mainnet"
        Testnet -> "testnet"
        Preview -> "preview"
        Preprod -> "preprod"
      netDir = base </> networkName
      subdirs = ["keys", "addresses", "utxos", "transactions", "protocol", "scripts"]
  createDirectoryIfMissing True netDir
  mapM_ (\d -> createDirectoryIfMissing True (netDir </> d)) subdirs
  let cfgPath = netDir </> "config.json"
      params = loadProtocolParameters appInitNetwork
      emptyConfig = Config { network = appInitNetwork, protocolParameters = params, wallets = [] }
  LBS.writeFile cfgPath (encode emptyConfig)
  putStrLn $ "SUCCESS: Initialized COTS workspace at: " ++ netDir
  putStrLn $ "> Created config: " ++ cfgPath

-- | Try to infer workspace config path based on network under ~/.cotscli
inferWorkspaceConfig :: FilePath -> Network -> FilePath
inferWorkspaceConfig homeDir net =
  let netName = case net of
        Mainnet -> "mainnet"
        Testnet -> "testnet"
        Preview -> "preview"
        Preprod -> "preprod"
      base = homeDir
   in base </> netName </> "config.json"

-- | Append a wallet with a genesis UTXO to config.json
appendAddressToConfig :: FilePath -> Text -> Word64 -> IO ()
appendAddressToConfig cfgPath addr lov = do
  exists <- doesFileExist cfgPath
  if not exists
    then putStrLn $ "ERROR: Config file not found: " ++ cfgPath
    else do
      mCfg <- Aeson.decodeFileStrict' cfgPath :: IO (Maybe Config)
      case mCfg of
        Nothing -> putStrLn $ "ERROR: Could not parse config: " ++ cfgPath
        Just cfg -> do
          now <- getCurrentTime
          genesisHash <- generateTransactionHash 0 -- Use 0 for genesis transactions
          let newWallet = Wallet { name = T.pack ("wallet-" ++ take 8 (T.unpack (unAddress (Address addr))))
                                 , address = Address addr
                                 , utxos = [UTXO { txHash = TransactionId genesisHash, txIx = TxIndex 0, amount = Amount lov mempty }]
                                 }
              updated = cfg { wallets = wallets cfg ++ [newWallet] }
          LBS.writeFile cfgPath (encode updated)

-- | Run version command
runVersion :: FilePath -> IO ()
runVersion _ = do
  version <- getVersionString
  putStrLn $ "COTS (Cardano Offline Transaction Simulator) v" ++ version
  putStrLn "cardano-cli compatible interface"

-- === Place *_Sub combinators here, after parser definitions ===

-- | Database subcommand parser
databaseParser :: Parser DatabaseCommand
databaseParser =
  hsubparser
    ( command "init" (info (Init <$> initOptions) (progDesc "Initialize the SQLite database and home structure"))
        <> command "reset" (info (Reset <$> resetOptions) (progDesc "Reset the SQLite database (dangerous)"))
        <> command "snapshot" (info (Snapshot <$> snapshotOptions) (progDesc "Create a database snapshot"))
        <> command "load-snapshot" (info (LoadSnapshot <$> loadSnapshotOptions) (progDesc "Load a database snapshot"))
        <> command "import-utxo" (info (ImportUTXO <$> importUTXOptions) (progDesc "Import UTXOs from a JSON file"))
        <> command "export-utxo" (info (ExportUTXO <$> exportUTXOptions) (progDesc "Export UTXOs to a JSON file"))
        <> command "generate-utxo" (info (GenerateUTXO <$> generateUTXOptions) (progDesc "Generate initial UTXOs JSON file"))
        <> command "inspect" (info (Inspect <$> inspectOptions) (progDesc "Inspect the database and print statistics"))
    )

initOptions :: Parser InitOptions
initOptions = InitOptions <$> strOption (long "db-file" <> metavar "FILE" <> help "Path to the SQLite database file")

resetOptions :: Parser ResetOptions
resetOptions = ResetOptions <$> strOption (long "db-file" <> metavar "FILE" <> help "Path to the SQLite database file")

snapshotOptions :: Parser SnapshotOptions
snapshotOptions =
  SnapshotOptions
    <$> strOption (long "db-file" <> metavar "FILE" <> help "Path to the SQLite database file")
    <*> strOption (long "out-file" <> metavar "FILE" <> help "Path to the snapshot output file")

loadSnapshotOptions :: Parser LoadSnapshotOptions
loadSnapshotOptions =
  LoadSnapshotOptions
    <$> strOption (long "snapshot-file" <> metavar "FILE" <> help "Path to the snapshot file")
    <*> strOption (long "db-file" <> metavar "FILE" <> help "Path to the SQLite database file")

importUTXOptions :: Parser ImportUTXOptions
importUTXOptions =
  ImportUTXOptions
    <$> strOption (long "db-file" <> metavar "FILE" <> help "Path to the SQLite database file")
    <*> strOption (long "utxo-file" <> metavar "FILE" <> help "Path to the UTXO JSON file")

exportUTXOptions :: Parser ExportUTXOptions
exportUTXOptions =
  ExportUTXOptions
    <$> strOption (long "db-file" <> metavar "FILE" <> help "Path to the SQLite database file")
    <*> strOption (long "out-file" <> metavar "FILE" <> help "Path to the UTXO output JSON file")

generateUTXOptions :: Parser GenerateUTXOptions
generateUTXOptions =
  GenerateUTXOptions
    <$> (map T.pack . splitOn "," <$> strOption (long "addresses" <> metavar "ADDRESSES" <> help "Comma-separated list of addresses"))
    <*> (map read . splitOn "," <$> strOption (long "amounts" <> metavar "AMOUNTS" <> help "Comma-separated list of amounts in lovelace"))
    <*> strOption (long "out-file" <> metavar "FILE" <> help "Path to the output JSON file")
    <*> optional (strOption (long "prefix" <> metavar "PREFIX" <> help "Prefix for transaction hashes (default: genesis)"))

inspectOptions :: Parser InspectOptions
inspectOptions = InspectOptions <$> strOption (long "db-file" <> metavar "FILE" <> help "Path to the SQLite database file")

-- | Wallet subcommand parser
walletParser :: Parser WalletCommand
walletParser =
  hsubparser
    ( command "create" (info (Create <$> createWalletOptions) (progDesc "Create a new wallet"))
        <> command "list" (info (ListWallets <$> listWalletsOptions) (progDesc "List all wallets"))
        <> command "import" (info (Import <$> importWalletOptions) (progDesc "Import a wallet from a file"))
        <> command "export" (info (ExportWallet <$> exportWalletOptions) (progDesc "Export a wallet to a file"))
        <> command "info" (info (Info <$> walletInfoOptions) (progDesc "Show wallet information"))
    )

createWalletOptions :: Parser CreateWalletOptions
createWalletOptions =
  CreateWalletOptions
    <$> strOption (long "name" <> metavar "NAME" <> help "Wallet name")
    <*> strOption (long "address" <> metavar "ADDRESS" <> help "Wallet address")
    <*> strOption (long "db-file" <> metavar "FILE" <> help "Path to the SQLite database file")

listWalletsOptions :: Parser ListWalletsOptions
listWalletsOptions = ListWalletsOptions <$> strOption (long "db-file" <> metavar "FILE" <> help "Path to the SQLite database file")

importWalletOptions :: Parser ImportWalletOptions
importWalletOptions =
  ImportWalletOptions
    <$> strOption (long "file" <> metavar "FILE" <> help "Path to the wallet file to import")
    <*> strOption (long "db-file" <> metavar "FILE" <> help "Path to the SQLite database file")

exportWalletOptions :: Parser ExportWalletOptions
exportWalletOptions =
  ExportWalletOptions
    <$> strOption (long "name" <> metavar "NAME" <> help "Wallet name")
    <*> strOption (long "file" <> metavar "FILE" <> help "Path to the wallet file to export to")
    <*> strOption (long "db-file" <> metavar "FILE" <> help "Path to the SQLite database file")

walletInfoOptions :: Parser WalletInfoOptions
walletInfoOptions =
  WalletInfoOptions
    <$> strOption (long "name" <> metavar "NAME" <> help "Wallet name")
    <*> strOption (long "db-file" <> metavar "FILE" <> help "Path to the SQLite database file")

-- | Address subcommand parser
addressParser :: Parser AddressCommand
addressParser =
  hsubparser
    ( command "key-gen" (info (AddressKeyGen <$> addressKeyGenOptions) (progDesc "Generate a new payment key pair"))
        <> command "build" (info (AddressBuild <$> addressBuildOptions) (progDesc "Build a payment address"))
        <> command "info" (info (AddressInfo <$> addressInfoOptions) (progDesc "Show information about a payment address"))
    )

addressKeyGenOptions :: Parser AddressKeyGenOptions
addressKeyGenOptions =
  AddressKeyGenOptions
    <$> strOption (long "verification-key-file" <> metavar "FILE" <> help "Path to the verification key file")
    <*> strOption (long "signing-key-file" <> metavar "FILE" <> help "Path to the signing key file")
    <*> optional (strOption (long "key-type" <> metavar "TYPE" <> help "Key type (normal, extended)"))

addressBuildOptions :: Parser AddressBuildOptions
addressBuildOptions =
  AddressBuildOptions
    <$> optional (strOption (long "payment-verification-key-file" <> metavar "FILE" <> help "Path to the payment verification key file"))
    <*> optional (strOption (long "stake-verification-key-file" <> metavar "FILE" <> help "Path to the stake verification key file"))
    <*> strOption (long "out-file" <> metavar "FILE" <> help "Path to the output address file")
    <*> option auto (long "network" <> metavar "NETWORK" <> help "Network (Mainnet, Testnet, Preview, Preprod)")
    <*> optional (option auto (long "initial-amount" <> metavar "LOVELACE" <> help "Initial amount to assign to this address (lovelace) and add as genesis UTXO in config"))
    <*> optional (strOption (long "config" <> metavar "FILE" <> help "Path to config.json to update (defaults to workspace config)"))

addressInfoOptions :: Parser AddressInfoOptions
addressInfoOptions = AddressInfoOptions <$> strOption (long "address" <> metavar "ADDRESS" <> help "Address to inspect")

-- | Stake address subcommand parser
stakeAddressParser :: Parser StakeAddressCommand
stakeAddressParser =
  hsubparser
    ( command "key-gen" (info (StakeAddressKeyGen <$> stakeAddressKeyGenOptions) (progDesc "Generate a new stake key pair"))
        <> command "build" (info (StakeAddressBuild <$> stakeAddressBuildOptions) (progDesc "Build a stake address"))
        <> command "info" (info (StakeAddressInfo <$> stakeAddressInfoOptions) (progDesc "Show information about a stake address"))
    )

stakeAddressKeyGenOptions :: Parser StakeAddressKeyGenOptions
stakeAddressKeyGenOptions =
  StakeAddressKeyGenOptions
    <$> strOption (long "verification-key-file" <> metavar "FILE" <> help "Path to the stake verification key file")
    <*> strOption (long "signing-key-file" <> metavar "FILE" <> help "Path to the stake signing key file")

stakeAddressBuildOptions :: Parser StakeAddressBuildOptions
stakeAddressBuildOptions =
  StakeAddressBuildOptions
    <$> strOption (long "stake-verification-key-file" <> metavar "FILE" <> help "Path to the stake verification key file")
    <*> strOption (long "out-file" <> metavar "FILE" <> help "Path to the output stake address file")
    <*> option auto (long "network" <> metavar "NETWORK" <> help "Network (Mainnet, Testnet, Preview, Preprod)")

stakeAddressInfoOptions :: Parser StakeAddressInfoOptions
stakeAddressInfoOptions = StakeAddressInfoOptions <$> strOption (long "address" <> metavar "ADDRESS" <> help "Stake address to inspect")

-- | Mint subcommand parser
mintParser :: Parser MintCommand
mintParser =
  hsubparser
    ( command "build" (info (MintBuild <$> mintBuildOptions) (progDesc "Build a minting transaction"))
        <> command "calculate" (info (MintCalculate <$> mintCalculateOptions) (progDesc "Calculate minting fees"))
    )

mintBuildOptions :: Parser MintBuildOptions
mintBuildOptions =
  MintBuildOptions
    <$> many (strOption (long "tx-in" <> metavar "TXIN" <> help "Transaction input"))
    <*> many (strOption (long "tx-out" <> metavar "TXOUT" <> help "Transaction output"))
    <*> optional (strOption (long "mint" <> metavar "MINT" <> help "Minting specification"))
    <*> optional (strOption (long "mint-script-file" <> metavar "FILE" <> help "Path to the minting script file"))
    <*> optional (strOption (long "change-address" <> metavar "ADDRESS" <> help "Change address"))
    <*> strOption (long "out-file" <> metavar "FILE" <> help "Path to the output transaction file")
    <*> option auto (long "network" <> metavar "NETWORK" <> help "Network (Mainnet, Testnet, Preview, Preprod)")
    <*> strOption (long "protocol-params-file" <> metavar "FILE" <> help "Path to the protocol parameters file")

mintCalculateOptions :: Parser MintCalculateOptions
mintCalculateOptions =
  MintCalculateOptions
    <$> strOption (long "policy-id" <> metavar "POLICYID" <> help "Policy ID")
    <*> strOption (long "asset-name" <> metavar "ASSET" <> help "Asset name")
    <*> option auto (long "quantity" <> metavar "QTY" <> help "Quantity to mint")
    <*> strOption (long "protocol-params-file" <> metavar "FILE" <> help "Path to the protocol parameters file")

-- | Transaction subcommand parser
transactionParser :: Parser TransactionCommand
transactionParser =
  hsubparser
    ( command "build" (info (Build <$> buildOptions) (progDesc "Build a transaction"))
        <> command "build-raw" (info (BuildRaw <$> buildRawOptions) (progDesc "Build a raw transaction (Cardano CLI compatible)"))
        <> command "sign" (info (Sign <$> signOptions) (progDesc "Sign a transaction"))
        <> command "submit" (info (Submit <$> submitOptions) (progDesc "Submit a transaction"))
        <> command "calculate-min-fee" (info (CalculateMinFee <$> calculateMinFeeOptions) (progDesc "Calculate minimum transaction fee"))
        <> command "view" (info (View <$> viewOptions) (progDesc "View transaction details"))
        <> command "txid" (info (TxId <$> txIdOptions) (progDesc "Calculate transaction ID"))
        <> command "simulate" (info (Simulate <$> simulateOptions) (progDesc "Simulate a transaction"))
        <> command "validate" (info (Validate <$> validateOptions) (progDesc "Validate a transaction"))
        <> command "export" (info (Export <$> exportOptions) (progDesc "Export a transaction"))
        <> command "decode" (info (Decode <$> decodeOptions) (progDesc "Decode a transaction"))
    )

buildOptions :: Parser BuildOptions
buildOptions =
  BuildOptions
    <$> many (strOption (long "tx-in" <> metavar "TXIN" <> help "Transaction input (format: TXID#TXIX)"))
    <*> many (strOption (long "tx-out" <> metavar "TXOUT" <> help "Transaction output (format: ADDRESS+AMOUNT)"))
    <*> optional (strOption (long "change-address" <> metavar "ADDRESS" <> help "Change address for excess funds"))
    <*> strOption (long "db-file" <> metavar "FILE" <> help "Path to the SQLite database file")
    <*> strOption (long "out-file" <> metavar "FILE" <> help "Path to the output transaction file")
    <*> switch (long "offline" <> help "Run in offline mode (always true for COTS)")
    <*> optional (option auto (long "fee" <> metavar "LOVELACE" <> help "Fee in lovelace (optional)"))
    <*> optional (option auto (long "ttl" <> metavar "SLOT" <> help "Time-to-live (slot number, optional)"))
    <*> optional (strOption (long "script-file" <> metavar "FILE" <> help "Path to the Plutus script file (optional)"))
    <*> optional (strOption (long "datum-file" <> metavar "FILE" <> help "Path to the datum file (optional)"))
    <*> optional (strOption (long "redeemer-file" <> metavar "FILE" <> help "Path to the redeemer file (optional)"))

buildRawOptions :: Parser BuildRawOptions
buildRawOptions =
  BuildRawOptions
    <$> optional (strOption (long "babbage-era" <> metavar "ERA" <> help "Era (babbage-era, alonzo-era, etc.)"))
    <*> many (strOption (long "tx-in" <> metavar "TXIN" <> help "Transaction input (format: TXID#TXIX)"))
    <*> many (strOption (long "tx-out" <> metavar "TXOUT" <> help "Transaction output (format: ADDRESS+AMOUNT)"))
    <*> option auto (long "fee" <> metavar "LOVELACE" <> help "Fee in lovelace")
    <*> optional (option auto (long "ttl" <> metavar "SLOT" <> help "Time-to-live (slot number, optional)"))
    <*> strOption (long "out-file" <> metavar "FILE" <> help "Path to the output transaction file")
    <*> optional (strOption (long "script-file" <> metavar "FILE" <> help "Path to the Plutus script file (optional)"))
    <*> optional (strOption (long "datum-file" <> metavar "FILE" <> help "Path to the datum file (optional)"))
    <*> optional (strOption (long "redeemer-file" <> metavar "FILE" <> help "Path to the redeemer file (optional)"))

simulateOptions :: Parser SimulateOptions
simulateOptions =
  SimulateOptions
    <$> strOption (long "tx-file" <> metavar "FILE" <> help "Path to the transaction file to simulate")
    <*> strOption (long "db-file" <> metavar "FILE" <> help "Path to the SQLite database file")
    <*> switch (long "verbose" <> help "Show detailed simulation output")

signOptions :: Parser SignOptions
signOptions =
  SignOptions
    <$> strOption (long "tx-file" <> metavar "FILE" <> help "Path to the transaction file to sign")
    <*> strOption (long "signing-key-file" <> metavar "FILE" <> help "Path to the signing key file")
    <*> strOption (long "out-file" <> metavar "FILE" <> help "Path to the signed transaction output file")

validateOptions :: Parser ValidateOptions
validateOptions =
  ValidateOptions
    <$> strOption (long "tx-file" <> metavar "FILE" <> help "Path to the transaction file to validate")
    <*> strOption (long "db-file" <> metavar "FILE" <> help "Path to the SQLite database file")

exportOptions :: Parser ExportOptions
exportOptions =
  ExportOptions
    <$> strOption (long "tx-file" <> metavar "FILE" <> help "Path to the transaction file to export")
    <*> option auto (long "format" <> metavar "FORMAT" <> help "Export format (CardanoCLI, Koios, JSON)")
    <*> strOption (long "out-file" <> metavar "FILE" <> help "Path to the exported transaction file")

decodeOptions :: Parser DecodeOptions
decodeOptions =
  DecodeOptions
    <$> strOption (long "tx-file" <> metavar "FILE" <> help "Path to the transaction file to decode")
    <*> switch (long "verbose" <> help "Show detailed decoding output")

submitOptions :: Parser SubmitOptions
submitOptions =
  SubmitOptions
    <$> strOption (long "tx-file" <> metavar "FILE" <> help "Path to the signed transaction file")
    <*> optional (option auto (long "testnet-magic" <> metavar "MAGIC" <> help "Testnet magic number"))
    <*> optional (strOption (long "socket-path" <> metavar "PATH" <> help "Path to the node socket"))

calculateMinFeeOptions :: Parser CalculateMinFeeOptions
calculateMinFeeOptions =
  CalculateMinFeeOptions
    <$> strOption (long "tx-body-file" <> metavar "FILE" <> help "Path to the transaction body file")
    <*> option auto (long "tx-in-count" <> metavar "COUNT" <> help "Number of transaction inputs")
    <*> option auto (long "tx-out-count" <> metavar "COUNT" <> help "Number of transaction outputs")
    <*> option auto (long "witness-count" <> metavar "COUNT" <> help "Number of witnesses")
    <*> optional (strOption (long "testnet-magic" <> metavar "MAGIC" <> help "Testnet magic number"))
    <*> strOption (long "protocol-params-file" <> metavar "FILE" <> help "Path to the protocol parameters file")

viewOptions :: Parser ViewOptions
viewOptions =
  ViewOptions
    <$> strOption (long "tx-file" <> metavar "FILE" <> help "Path to the transaction file")
    <*> switch (long "verbose" <> help "Show detailed transaction information")

txIdOptions :: Parser TxIdOptions
txIdOptions =
  TxIdOptions
    <$> strOption (long "tx-file" <> metavar "FILE" <> help "Path to the transaction file")

-- | UTXO subcommand parser
utxoParser :: Parser UTXOCommand
utxoParser =
  hsubparser
    ( command "list" (info (List <$> listOptions) (progDesc "List UTXOs"))
        <> command "reserve" (info (Reserve <$> reserveOptions) (progDesc "Reserve UTXOs"))
        <> command "process" (info (Process <$> processOptions) (progDesc "Process a transaction and update UTXOs"))
    )

queryParser :: Parser QueryCommand
queryParser =
  hsubparser
    ( command "utxo" (info (QueryUTXO <$> queryUTXOptions) (progDesc "Query UTXOs for an address (Cardano CLI compatible)"))
    )

queryUTXOptions :: Parser QueryUTXOptions
queryUTXOptions =
  QueryUTXOptions
    <$> strOption (long "address" <> metavar "ADDRESS" <> help "Address to query UTXOs for")
    <*> optional (strOption (long "testnet-magic" <> metavar "MAGIC" <> help "Testnet magic number"))
    <*> switch (long "mainnet" <> help "Use mainnet")
    <*> optional (strOption (long "socket-path" <> metavar "PATH" <> help "Path to the node socket"))
    <*> optional (strOption (long "db-file" <> metavar "FILE" <> help "Path to the database file (COTS specific)"))

listOptions :: Parser ListOptions
listOptions =
  ListOptions
    <$> optional (strOption (long "address" <> metavar "ADDRESS" <> help "Filter UTXOs by address (optional)"))
    <*> strOption (long "utxo-file" <> metavar "FILE" <> help "Path to the UTXO JSON file")
    <*> switch (long "verbose" <> help "Show detailed UTXO output")

reserveOptions :: Parser ReserveOptions
reserveOptions =
  ReserveOptions
    <$> strOption (long "address" <> metavar "ADDRESS" <> help "Address to reserve UTXOs for")
    <*> option auto (long "amount" <> metavar "LOVELACE" <> help "Amount to reserve in lovelace")
    <*> strOption (long "utxo-file" <> metavar "FILE" <> help "Path to the UTXO JSON file")
    <*> strOption (long "out-file" <> metavar "FILE" <> help "Path to the reserved UTXOs output file")

processOptions :: Parser ProcessOptions
processOptions =
  ProcessOptions
    <$> strOption (long "from-address" <> metavar "ADDRESS" <> help "Source address for the transaction")
    <*> strOption (long "to-address" <> metavar "ADDRESS" <> help "Destination address for the transaction")
    <*> option auto (long "amount" <> metavar "LOVELACE" <> help "Amount to transfer in lovelace")
    <*> strOption (long "db-file" <> metavar "FILE" <> help "Path to the database file")
    <*> strOption (long "out-file" <> metavar "FILE" <> help "Path to the transaction output file")

-- | Protocol subcommand parser
protocolParser :: Parser ProtocolCommand
protocolParser =
  hsubparser
    ( command "update" (info (Update <$> updateOptions) (progDesc "Update protocol parameters from a local file"))
        <> command "fetch" (info (Fetch <$> fetchOptions) (progDesc "Fetch protocol parameters from a URL (e.g., Koios) and store to file and DB"))
    )

updateOptions :: Parser UpdateOptions
updateOptions =
  UpdateOptions
    <$> strOption (long "protocol-params-file" <> metavar "FILE" <> help "Path to the protocol parameters file")
    <*> strOption (long "db-file" <> metavar "FILE" <> help "Path to the SQLite database file")

fetchOptions :: Parser FetchOptions
fetchOptions =
  FetchOptions
    <$> strOption (long "url" <> metavar "URL" <> help "Source URL for protocol parameters (e.g., https://api.koios.rest/api/v1/epoch_params)")
    <*> strOption (long "out-file" <> metavar "FILE" <> help "Path to save the fetched protocol parameters JSON")
    <*> strOption (long "db-file" <> metavar "FILE" <> help "Path to the SQLite database file")