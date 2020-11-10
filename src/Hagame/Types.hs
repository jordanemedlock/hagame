{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances #-}

module Hagame.Types (
    HasZero(..), 
    HasEdges(..),
    CanCollide(..),
    Size(Size), width, height,
    Position(Position), x, y,
    Velocity(Velocity ), move,
    Rectangle(Rectangle), size, position,
    Color(Color), r, g, b, a,
    Window(Windowed), name, 
    GameOptions(GameOptions), window, backgroundColor, fps, shouldClose,
    GameDisplay,
    KeyState(..),
    GameInput(GameInput), keyboardState,
    DrawRect(DrawRect), color, queueName, rectangle

) where

import Control.Lens hiding (element)
import Control.Lens.TH
import qualified Graphics.Rendering.OpenGL as GL

class HasZero a where
    zero :: a

class HasEdges a where
    top :: Lens' a Double
    bottom :: Lens' a Double 
    bottom = top
    left :: Lens' a Double
    right :: Lens' a Double
    right = left

class CanCollide a b where
    collidesWith :: a -> b -> Bool


data Size = Size 
    { _sizeWidth :: Double
    , _sizeHeight :: Double
    } deriving (Show)

$(makeFields ''Size)

instance HasZero Size where
    zero = Size 0 0 

instance HasEdges Size where
    top = lens (const 0) const
    left = lens (const 0) const

    bottom = height
    right = width

data Position = Position 
    { _positionX :: Double
    , _positionY :: Double
    } deriving (Show)

$(makeFields ''Position)

instance HasZero Position where
    zero = Position 0 0 

instance HasEdges Position where
    top = y
    left = x

instance Num Position where
    p1 + p2 = Position (p1^.x + p2^.x) (p1^.y + p2^.y)
    p1 * p2 = Position (p1^.x * p2^.x) (p1^.y * p2^.y)
    p1 - p2 = Position (p1^.x - p2^.x) (p1^.y - p2^.y)
    abs p = Position (abs $ p^.x) (abs $ p^.y)
    signum p = Position (signum $ p^.x) (signum $ p^.y)
    fromInteger i = Position (fromInteger i) (fromInteger i)

    

data Velocity = Velocity 
    { _velocityX :: Double
    , _velocityY :: Double
    } deriving (Show)

$(makeFields ''Velocity)

instance HasZero Velocity where
    zero = Velocity 0 0 

move (Velocity x y) = Position x y

data Rectangle = Rectangle
    { _rectangleSize :: Size
    , _rectanglePosition :: Position
    } deriving (Show)

$(makeFields ''Rectangle)

instance HasZero Rectangle where
    zero = Rectangle zero zero

instance HasEdges Rectangle where
    top = position.y
    left = position.x

    bottom = lens (\r -> r^.top + r^.size.height) (\r v -> r & top .~ (v - r^.size.height))
    right = lens (\r -> r^.left + r^.size.width) (\r v -> r & left .~ (v - r^.size.width))

instance CanCollide Rectangle Rectangle where
    collidesWith (Rectangle (Size w1 h1) (Position x1 y1)) (Rectangle (Size w2 h2) (Position x2 y2)) = response
        where
            response = x1 < x2 + w2 && x1 + w1 > x2 && y1 < y2 + w2 && y1 + w1 > y2

data Color = Color
    { _colorR :: Double
    , _colorG :: Double
    , _colorB :: Double
    , _colorA :: Double
    } deriving (Show)

$(makeFields ''Color)

instance HasZero Color where
    zero = Color 0 0 0 0

instance GL.Uniform Color where
    uniform loc = GL.makeStateVar getter setter
        where 
            getter = do
                (GL.Color4 r g b a) <- GL.get $GL.uniform loc :: IO (GL.Color4 Float) -- TODO: losing precision
                return $ Color (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a) 
            setter (Color r g b a) = do
                GL.uniform loc GL.$= (GL.Color4 (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a) :: GL.Color4 Float)
    uniformv loc size ptr = error "uniformv unimplemented" -- TODO: probably not going to need to set this



data Window = Windowed 
    { _windowName :: String
    , _windowSize :: Size 
    , _windowPosition :: Position
    } deriving (Show)

$(makeFields ''Window)

data GameOptions = GameOptions
    { _window :: Window
    , _backgroundColor :: Color
    , _fps :: Int
    , _shouldClose :: Bool
    } deriving (Show)

$(makeLenses ''GameOptions)

data GameDisplay 
-- TODO: not sure how to implement this yet
-- maybe I can implement this like :: IO ()
-- and then just compose them
-- add a Renderable class that includes
-- initialize, render, and maybe like queueName


data KeyState = Pressed | Unpressed

-- This is essentially read only thats why I'm not adding lens'
data GameInput = GameInput
    { _keyboardState :: (Char, KeyState)
    }

$(makeLenses ''GameInput)

data Assets
-- TODO: Implement this


data DrawRect = DrawRect 
    { _drawRectQueueName :: String
    , _drawRectRectangle :: Rectangle
    , _drawRectColor :: Color
    }

$(makeFields ''DrawRect)