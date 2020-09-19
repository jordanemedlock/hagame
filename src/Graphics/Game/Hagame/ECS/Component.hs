module Graphics.Game.Hagame.ECS.Component where

import RIO
import qualified RIO.Vector.Storable as VS


data ComponentManager a =
    ComponentManager    { cmName :: String
                        , cmArray :: VS.Vector (Maybe a) 
                        }

class ComponentManagerClass cm a where
    getComponentById :: Int -> cm a -> Maybe a 
    

class HasComponentManager env a where
    getComponentManager :: (ComponenentManagerClass cm a) => String -> env -> cm a


instance HasComponentManager (ComponentManager a) a where
    getComponentManager :: String -> ComponentManager a -> ComponentManager a
    getComponentManager _ = id

