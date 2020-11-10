module Hagame.Utils where

import Hagame.Types
import qualified Data.Matrix as M
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Control.Lens

-- | Transform object
-- data Transform = 
--     Transform   { position :: GL.Vector3 Float -- ^ Position
--                 , size :: GL.Vector3 Float -- ^ Size
--                 , rotation2D :: Float -- ^ Rotation
--                 }

-- | Create an OpenGL matrix from the Matrix
toGLMatrix  :: GL.MatrixComponent a
            => M.Matrix a  -- ^ Input Matrix
            -> IO (GL.GLmatrix a) -- ^ Output GLmatrix
toGLMatrix mat = GL.newMatrix GL.RowMajor (M.toList mat)


-- | Creates a 4x4 transform matrix representing a translation 
translationMat :: Position -> M.Matrix Float
translationMat (Position x y) = mat
    where mat = M.fromList 4 4 $ map realToFrac 
                                    [ 1.0, 0.0, 0.0, x 
                                    , 0.0, 1.0, 0.0, y
                                    , 0.0, 0.0, 1.0, 0 -- TODO: implement z axis
                                    , 0.0, 0.0, 0.0, 1.0
                                    ]


-- | Creates a 4x4 transform matrix representing a scaling 
scaleMat :: Size -> M.Matrix Float
scaleMat (Size width height) = mat
    where mat = M.fromList 4 4 $ map realToFrac
                                    [ width,  0.0, 0.0, 0.0 
                                    , 0.0, height, 0.0, 0.0 
                                    , 0.0, 0.0,    1.0, 0.0 -- TODO: implement z axis
                                    , 0.0, 0.0,    0.0, 1.0
                                    ]


-- | Creates a 4x4 transform matrix representing a rotation 
rotationMat2D   :: Float -- ^ Rotation in rads
                -> M.Matrix Float
rotationMat2D rot = mat
    where mat = M.fromList 4 4 [ cos rot, -(sin rot), 0.0, 0.0 
                               , sin rot, cos rot,    0.0, 0.0 
                               , 0.0, 0.0,            1.0, 0.0 
                               , 0.0, 0.0,            0.0, 1.0
                               ]


-- | Creats an orthogrphic projection matrix
orthographic :: Float -> Float -> Float -> Float -> Float -> Float -> M.Matrix Float
orthographic left right bottom top near far = mat
    where mat = M.fromList 4 4 [ 2.0 / (right - left), 0.0, 0.0, - (right + left) / (right - left) 
                               , 0.0, 2.0 / (top - bottom), 0.0, - (top + bottom) / (top - bottom)
                               , 0.0, 0.0, - 2.0 / (far - near), - (far + near) / (far - near)
                               , 0.0, 0.0, 0.0, 1.0
                               ]



-- | Creates the sprites VAO and adds the goemetry
createSquareVAO :: IO GL.VertexArrayObject
createSquareVAO = do
    
    let verticesL = [ (-0.5), (-0.5),   0.0, 0.0
                    , 0.5, 0.5,         1.0, 1.0
                    , (-0.5), 0.5,      0.0, 1.0
                    
                    , (-0.5), (-0.5),   0.0, 0.0
                    , 0.5, (-0.5),      1.0, 0.0
                    , 0.5, 0.5,         1.0, 1.0
                    ] :: [Float]
    vertices <- newArray verticesL
    let verticesSize = fromIntegral $ sizeOf (0.0 :: Float) * length verticesL

    vao <- GL.genObjectName :: IO GL.VertexArrayObject
    vbo <- GL.genObjectName :: IO GL.BufferObject

    GL.bindVertexArrayObject $= Just vao

    GL.bindBuffer GL.ArrayBuffer $= Just vbo
    GL.bufferData GL.ArrayBuffer $= (verticesSize, vertices, GL.StaticDraw)

    GL.vertexAttribPointer (GL.AttribLocation 0) $= (GL.ToFloat, GL.VertexArrayDescriptor 4 GL.Float (fromIntegral $ 4 * sizeOf (0.0 :: Float)) nullPtr)
    GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled

    GL.bindBuffer GL.ArrayBuffer $= Nothing
    GL.bindVertexArrayObject $= Nothing
    GL.deleteObjectName vbo

    return vao

-- | Create a 4x4 transformation matrix from Transform object
getTransformMatrix :: Rectangle -> M.Matrix Float
getTransformMatrix rect = model
    where 
        translate = translationMat $ rect^.position
        rotate = rotationMat2D (pi / 180 * 0) -- TODO: haven't added rotations to rectangles
        scale = scaleMat $ rect^.size
        model = translate * rotate * scale