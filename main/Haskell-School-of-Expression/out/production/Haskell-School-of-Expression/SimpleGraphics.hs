module SimpleGraphics where

import Graphics.SOE

main0 = runGraphics(
          do w <- openWindow "My First Graphics Program" (300, 300)
             drawInWindow w (text(100,200) "HelloGraphicsWorld")
             k <- getKey w
             closeWindow w)
       
spaceClose :: Window -> IO ()
spaceClose w
            = do k <- getKey w
                 if k == ' ' then closeWindow w
                             else spaceClose w
                             
main1 = runGraphics(
          do w <- openWindow
                    "My Second Graphics Program" (300, 300)
             drawInWindow w (text(100,200) "Hello Graphics World!")
             spaceClose w) 
             
pic1 = withColor Red (ellipse (150, 150) (300, 300))
pic2 = withColor Blue (polyline [(100,50), (200, 50), (200,250), (100,250), (100,50)])

main2 = runGraphics(
        do w <- openWindow
                  "Some Graphics Figures" (300, 300)
           drawInWindow w pic1
           drawInWindow w pic2
           spaceClose w)
           
fillTri :: Window -> Int -> Int -> Int -> IO ()
fillTri w x y size = drawInWindow w (withColor Blue (polygon [(x,y), (x + size, y), (x,y - size)]))

minSize :: Int
minSize = 8
  
sierpinkskiTri :: Window -> Int -> Int -> Int -> IO ()
sierpinkskiTri w x y size
                = if size <= minSize
                  then fillTri w x y size
                  else let size2 = size `div` 2
                    in do sierpinkskiTri w x y size2
                          sierpinkskiTri w x (y - size2) size2
                          sierpinkskiTri w (x + size2) y size2
                          
main3
  = runGraphics(
    do w <- openWindow "Sierpinski's Triangle" (400, 400)
       sierpinkskiTri w 50 300 256
       spaceClose w) 


