{-# LANGUAGE FlexibleContexts #-}

module XMonad.User.Bindings.Util ( terminalFromConf, inTerminalFromConf ) where

import XMonad

terminalFromConf :: (MonadIO m, MonadReader XConf m) => m String
terminalFromConf = reader $ terminal . config

inTerminalFromConf :: (MonadIO m, MonadReader XConf m) => String -> m String
inTerminalFromConf prog = do
    terminalEmulator <- terminalFromConf
    return $ terminalEmulator <> " --title " <> prog <> " -e " <> prog
