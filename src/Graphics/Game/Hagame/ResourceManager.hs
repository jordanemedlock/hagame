{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Graphics.Game.Hagame.ResourceManager where

import RIO
import qualified RIO.HashMap as HM


class HasResources c v where
    resourceL :: String -> Lens' c (Maybe v)

instance HasResources (HM.HashMap String a) a where
    resourceL :: String -> Lens' (HM.HashMap String a) (Maybe a)
    resourceL key = lens getter setter
        where 
            setter m mv = case mv of
                Nothing -> HM.delete key m
                Just v -> HM.insert key v m
            getter m = HM.lookup key m

setResource :: (MonadIO m, HasResources c v) => String -> Maybe v -> IORef c -> m ()
setResource key mvalue ref = modifyIORef ref (set (resourceL key) mvalue)

getResource :: (MonadIO m, HasResources c v) => String -> IORef c -> m (Maybe v)
getResource key ref = view (resourceL key) <$> readIORef ref