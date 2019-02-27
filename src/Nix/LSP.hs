{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Nix.LSP
 ( main
 ) where

import Control.Concurrent
import Control.Concurrent.STM.TChan
import qualified Control.Exception as Exception
import Control.Monad
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.STM
import Data.Maybe
import Data.Default
import Data.Semigroup
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as T
import qualified Language.Haskell.LSP.Control as LSP.Control
import qualified Language.Haskell.LSP.Core as LSP.Core
import Language.Haskell.LSP.Diagnostics
import Language.Haskell.LSP.Messages
import qualified Language.Haskell.LSP.Types as LSP
import qualified Language.Haskell.LSP.Types.Lens as LSP
import qualified Language.Haskell.LSP.Utility as LSP
import qualified Nix.Expr
import qualified Nix.Parser
import qualified Nix.Pretty
import Text.Megaparsec (errorPos, parse, SourcePos(..), unPos, parseErrorTextPretty)
import Text.PrettyPrint.ANSI.Leijen (renderPretty, displayS)
import System.Exit
import qualified System.Log.Logger as L
import Options.Applicative
import Protolude hiding (sourceLine, sourceColumn)


data CommandLineOptions = CommandLineOptions
   { verbose :: Bool
   }

commandLineOptionsParser :: Parser CommandLineOptions
commandLineOptionsParser = CommandLineOptions
   <$> switch
       ( long "verbose"
       <> short 'v'
       <> help "Debug logging"
       )

commandLineOptions :: ParserInfo CommandLineOptions
commandLineOptions = info (commandLineOptionsParser <**> helper)
   ( fullDesc
   <> header "hnix-lsp"
   <> progDesc "A Language Server Protocol Implementation for the Nix Language (see https://nixos.org/nix/)"
   )

main :: IO ()
main = do
 opts <- execParser commandLineOptions
 exitcode <- run opts
 case exitcode of
   0 -> exitSuccess
   c -> exitWith . ExitFailure $ c

run :: CommandLineOptions -> IO Int
run _ = flip Exception.catches handlers $ do
   rin <- atomically newTChan :: IO (TChan ReactorInput)
   let dp lf = do
         _rpid <- forkIO $ reactor lf rin
         return Nothing
   flip Exception.finally L.removeAllHandlers $ do
       LSP.Core.setupLogger Nothing [] L.DEBUG
       LSP.Control.run
           (return (Right ()), dp)
           (lspHandlers rin)
           lspOptions
           Nothing
 where
   handlers =
     [ Exception.Handler ioExcept
     , Exception.Handler someExcept]
   ioExcept (e :: Exception.IOException) = print e >> return 1
   someExcept (e :: Exception.SomeException) = print e >> return 1


   lspOptions :: LSP.Core.Options
   lspOptions = def
     { LSP.Core.textDocumentSync = Just syncOptions }

   syncOptions :: LSP.TextDocumentSyncOptions
   syncOptions = LSP.TextDocumentSyncOptions
      { LSP._openClose = Just True
      , LSP._change = Just LSP.TdSyncNone
      , LSP._willSave = Just False
      , LSP._willSaveWaitUntil = Just False
      , LSP._save = Just $ LSP.SaveOptions $ Just True
      }

   lspHandlers :: TChan ReactorInput -> LSP.Core.Handlers
   lspHandlers rin = def
      { LSP.Core.initializedHandler = Just $ passHandler NotInitialized
      , LSP.Core.didSaveTextDocumentNotificationHandler = Just $ passHandler NotDidSaveTextDocument
      , LSP.Core.didOpenTextDocumentNotificationHandler = Just $ passHandler NotDidOpenTextDocument
      , LSP.Core.documentFormattingHandler = Just $ passHandler ReqDocumentFormatting
      }
     where
       passHandler :: (a -> FromClientMessage) -> LSP.Core.Handler a
       passHandler c notification =
         atomically $ writeTChan rin (HandlerRequest (c notification))

-- The reactor is a process that serialises and buffers all requests from the
-- LSP client, so they can be sent to the backend compiler one at a time, and a
-- reply sent.
newtype ReactorInput =
  HandlerRequest FromClientMessage -- ^ injected into the reactor input by each of the individual callback handlers

-- | The monad used in the reactor
type R c a = ReaderT (LSP.Core.LspFuncs c) IO a

-- | The single point that all events flow through, allowing management of state
-- to stitch replies and requests together from the two asynchronous sides: lsp
-- server and backend compiler
reactor :: LSP.Core.LspFuncs () -> TChan ReactorInput -> IO ()
reactor lf inp =
   flip runReaderT lf $ forever $ do
       inval <- liftIO $ atomically $ readTChan inp
       case inval of
           HandlerRequest (NotInitialized _) ->
             return ()

           HandlerRequest (NotDidSaveTextDocument req) -> do
             liftIO $ LSP.Core.flushDiagnosticsBySourceFunc lf 200 (Just "hnix")
             parseWithDiagnostics (reqToURI req) (\_ -> return ())

           HandlerRequest (NotDidOpenTextDocument req) ->
             parseWithDiagnostics (reqToURI req) (\_ -> return ())

           -- TODO: range, type
           HandlerRequest (ReqDocumentFormatting req) ->
             parseWithDiagnostics (reqToURI req) $ \(expr, txt) -> do
              -- TODO: get pretty printing options from request
              let out = displayS (renderPretty 0.4 80 (Nix.Pretty.prettyNix (Nix.Expr.stripAnnotation expr))) ""
                  range = LSP.Range (LSP.Position 0 0)
                                    (LSP.Position (length (T.lines txt)) 0)
                  resp = LSP.List [LSP.TextEdit range (toS out)]
              reactorSend $ RspDocumentFormatting $ LSP.Core.makeResponseMessage req resp

           HandlerRequest req ->
             liftIO $ LSP.logs $ "reactor: unhandled HandlerRequest:" ++ show req

parseWithDiagnostics :: LSP.Uri -> ((Nix.Expr.NExprLoc, Text) -> R () ()) -> R () ()
parseWithDiagnostics uri f = do
  lf <- ask
  let Just fp = LSP.uriToFilePath uri
  txt <- liftIO $ readFile fp
  case parse Nix.Parser.nixToplevelForm fp txt of
    Left err -> do
      let [pos] = NonEmpty.toList (errorPos err)
          position = LSP.Position (unPos (sourceLine pos)) (unPos (sourceColumn pos))
          diag = LSP.Diagnostic
                  (LSP.Range position position)
                  (Just LSP.DsError)  -- severity
                  Nothing  -- code
                  (Just "hnix") -- source
                  (toS (parseErrorTextPretty err))
                  (Just (LSP.List []))
      liftIO $ LSP.logs $ show diag
      liftIO $ LSP.Core.publishDiagnosticsFunc lf 100 uri (Just 0) (partitionBySource [diag])
    Right expr -> f (expr, txt)

reqToURI :: ( LSP.HasParams s a1
            , LSP.HasTextDocument a1 a2
            , LSP.HasUri a2 a3
            ) => s -> a3
reqToURI req =
    req ^.
    LSP.params .
    LSP.textDocument .
    LSP.uri

reactorSend :: FromServerMessage -> R () ()
reactorSend msg = do
  lf <- ask
  liftIO $ LSP.Core.sendFunc lf msg
