{-# LANGUAGE RecordWildCards, TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}

import Hagame
import Control.Lens 



data Ball = Ball 
    { _rect :: Rectangle
    , _velocity :: Velocity
    }

$(makeLenses ''Ball)

instance (CanCollide Rectangle b) => CanCollide Ball b where
    b `collidesWith` x = (b^.rect) `collidesWith` x

instance HasEdges Ball where
    left = rect.left
    right = rect.right
    top = rect.top
    bottom = rect.bottom

data Scores = Scores { _leftScore :: Int, _rightScore :: Int }

$(makeLenses ''Scores)

instance Semigroup Scores where
    (Scores l1 r1) <> (Scores l2 r2) = Scores (l1 + l2) (r1 + r2)
instance Monoid Scores where
    mempty = Scores 0 0 

data PlayState = NotStarted | Playing { _scores :: Scores } | Paused { _scores :: Scores } | Ended { _scores :: Scores } | RoundEnd { _scores :: Scores }

$(makeLenses ''PlayState)

data GameState = GameState 
    { _leftPaddle :: Rectangle
    , _rightPaddle :: Rectangle
    , _ball :: Ball
    , _playState :: PlayState
    }

$(makeLenses ''GameState)

screenSize = Size 640 480
paddleSpeed = 100 -- TODO: Just a guess
                    -- TODO: also I wan't for you to be able to configure this at runtime.
                    -- maybe like a way to edit all variables at runtime, idk

options = GameOptions (Windowed "Pong" (Size 640 480) (Position 0 0)) (Color 0 0 0 1) 60 False

main = runFixedGame options initialState handleInput render

initialState = GameState left right ball state
    where 
        left = Rectangle (Size 40 160) (Position 40 (screenSize^.height / 2))
        right = Rectangle (Size 40 160) (Position (screenSize^.width - 40) (screenSize^.height / 2))
        ball = Ball (Rectangle (Size 40 40) (Position (screenSize^.width / 2) (screenSize^.height / 2))) (Velocity 0 0)
        state = NotStarted

handleInput :: GameInput -> GameOptions -> GameState -> (GameOptions, GameState)
handleInput gi go gs = case gs^.playState of
    NotStarted -> notStarted gi go gs
    Playing scores -> playing gi go gs
    Paused scores -> paused gi go gs
    _ -> (go, gs)

notStarted :: GameInput -> GameOptions -> GameState -> (GameOptions, GameState)
notStarted gi go gs = case gi^.keyboardState of
    (' ', Pressed) -> (go, gs & playState .~ Playing (Scores 0 0))
    _ -> (go, gs)

paused :: GameInput -> GameOptions -> GameState -> (GameOptions, GameState)
paused gi go gs = case gi^.keyboardState of 
    (' ', Pressed) -> (go, gs & playState .~ (Playing (gs^.(playState.scores))))
    _ -> (go, gs)

playing :: GameInput -> GameOptions -> GameState -> (GameOptions, GameState)
playing gi go gs = case gi^.keyboardState of 
    (' ', Pressed) -> (go, gs & playState .~ Paused (gs^.playState.scores))
    ('W', Pressed) -> (go, gs & leftPaddle.position +~ Position 0 (-paddleSpeed)) -- TODO: gonna have to figure out how to clamp this
    ('S', Pressed) -> (go, gs & leftPaddle.position +~ Position 0 paddleSpeed) -- TODO: same here
    _ -> (go, moveBall gs)

moveBall :: GameState -> GameState
moveBall gs 
    | newBall^.top <= screenSize^.top       = gs&ball .~ (newBall & flipY)
    | newBall^.bottom >= screenSize^.bottom = gs&ball .~ (newBall & flipY)

    | newBall^.left <= screenSize^.left     = gs&playState .~ RoundEnd (gs^.playState.scores & leftScore +~ 1) -- (leftWin $ scores playState)
    | newBall^.right >= screenSize^.right   = gs&playState .~ RoundEnd (gs^.playState.scores & rightScore +~ 1)

    | newBall `collidesWith` (gs^.leftPaddle) = gs&ball .~ (newBall & flipX)
    | newBall `collidesWith` (gs^.rightPaddle) = gs&ball .~ (newBall & flipX)

    | otherwise = gs&ball .~ newBall 

    where 
        newBall = gs^.ball & rect.position +~ (move $ gs^.ball.velocity)

flipX ball = ball & velocity.x .~ (- ball^.velocity.x)
flipY ball = ball & velocity.y .~ (- ball^.velocity.y)

render gs = [ DrawRect "rects" (gs^.leftPaddle) (Color 1 1 1 1)
            , DrawRect "rects" (gs^.rightPaddle) (Color 1 1 1 1)
            , DrawRect "rects" (gs^.ball.rect) (Color 1 1 1 1)
            ]
