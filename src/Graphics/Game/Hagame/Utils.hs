module Graphics.Game.Hagame.Utils where


import qualified Data.Matrix as M
import qualified Graphics.Rendering.OpenGL as GL


toGLMatrix :: GL.MatrixComponent a => M.Matrix a -> IO (GL.GLmatrix a)
toGLMatrix mat = GL.newMatrix GL.RowMajor (M.toList mat)

translationMat :: GL.Vector3 Float -> M.Matrix Float
translationMat (GL.Vector3 x y z) = mat
    where mat = M.fromList 4 4 [ 1.0, 0.0, 0.0, x 
                               , 0.0, 1.0, 0.0, y
                               , 0.0, 0.0, 1.0, z
                               , 0.0, 0.0, 0.0, 1.0
                               ]


scaleMat :: GL.Vector3 Float -> M.Matrix Float
scaleMat (GL.Vector3 width height depth) = mat
    where mat = M.fromList 4 4 [ width,  0.0, 0.0, 0.0 
                               , 0.0, height, 0.0, 0.0 
                               , 0.0, 0.0, depth,  0.0 
                               , 0.0, 0.0,    0.0, 1.0
                               ]


rotationMat2D :: Float -> M.Matrix Float
rotationMat2D rot = mat
    where mat = M.fromList 4 4 [ cos rot, -(sin rot), 0.0, 0.0 
                               , sin rot, cos rot,    0.0, 0.0 
                               , 0.0, 0.0,            1.0, 0.0 
                               , 0.0, 0.0,            0.0, 1.0
                               ]

orthographic :: Float -> Float -> Float -> Float -> Float -> Float -> M.Matrix Float
orthographic left right bottom top near far = mat
    where mat = M.fromList 4 4 [ 2.0 / (right - left), 0.0, 0.0, - (right + left) / (right - left) 
                               , 0.0, 2.0 / (top - bottom), 0.0, - (top + bottom) / (top - bottom)
                               , 0.0, 0.0, - 2.0 / (far - near), - (far + near) / (far - near)
                               , 0.0, 0.0, 0.0, 1.0
                               ]