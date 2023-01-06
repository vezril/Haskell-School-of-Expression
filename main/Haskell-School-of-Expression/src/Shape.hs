module Shape(Shape (..),
             Radius, Side, Vertex,
             square, circle, distBetween, area) where

  type Radius = Float
  type Side = Float
  type Vertex = (Float, Float)
  
  data Shape = Rectangle Side Side
             | Ellipse Radius Radius
             | RtTriangle Side Side
             | Polygon [Vertex]
           deriving Show
           
  square = Rectangle s s
  circle r = Ellipse r r
       
  distBetween :: Vertex -> Vertex -> Float
  distBetween (x1, y1) (x2, y2)
                = sqrt ((x1-x2)^2 + (y1-y2)^2)
       
  triArea :: Vertex -> Vertex -> Vertex -> Float
  triArea v1 v2 v3 = let a = distBetween v1 v2
                         b = distBetween v2 v3
                         c = distBetween v3 v1
                         s = 0.5 * (a+b+c)
                     in sqrt (s * (s-a) * (s-b) * (s-c))
      
  area :: Shape -> Float
  area (Rectangle s1 s2) = s1 * s2
  area (Ellipse r1 r2) = pi * r1 * r2
  area (RtTriangle s1 s2) = s1 * s2/2
  area (Polygon (v1 : vs)) = polyArea vs
    where polyArea :: [Vertex] -> Float
          polyArea (v2 : v3 : vs') = triArea v1 v2 v3
                                        + polyArea (v3 : vs')
          polyArea _ = 0
         
  crossProduct :: Vertex -> Vertex -> Vertex -> Float
  crossProduct (x1, y1) (y1, y2) (z1, z2) = (x2 - x1) * (y3 - y2) - (y2 - y1) * (x3 - x2)
     
  convex :: Shape -> Bool
  convex (Rectangle s1 s2) = True
  convex (Ellipse r1 r2) = True
  convex (RtTriangle s1 s2) = True
  convex (Polygon [v1, v2, v3]) = True
  convex (Polygon (v1, v2, v3, vs))
    = let sign = crossProduct v1 v2 v3
      in polyConvex sign v2 (v3 : vs)
        where polyConvex s v1 (v2 : v3 : vs')
                = let newSign = crossProduct v1 v2 v3
                  in (newSign * s) >= 0 && polyConvex s v2 (v3 : vs')
              polyConvex s vn [vf]
                = let newSign = crossProduct vn vl v1
                  in (newSign * s) >= 0 && polyConvex s vl []
              polyConvex s vl []
                = let newSign = crossProduct vl v1 v2
                  in newSign * s > 0
            
  
  