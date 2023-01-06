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
           
  square s = Rectangle s s
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
  crossProduct (x1, y1) (x2, y2) (x3, y3) = (x2 - x1) * (y3 - y2) - (y2 - y1) * (x3 - x2)
     
  convex :: Shape -> Bool
  convex (Rectangle s1 s2) = True
  convex (Ellipse r1 r2) = True
  convex (RtTriangle s1 s2) = True
  convex (Polygon [v1, v2, v3]) = True
  convex (Polygon (vfirst : vsecond : vthird : vs))
      = let sign = crossProduct vfirst vsecond vthird
        in polyConvex sign vsecond (vthird : vs)
            where polyConvex s v1 (v2 : v3 : vs')
                      = let newSign = crossProduct v1 v2 v3
                        in (newSign * s) >= 0 && polyConvex s v2 (v3 : vs')
                  polyConvex s vn [vlast]
                      = let newSign = crossProduct vn vlast vfirst
                        in (newSign * s) >= 0 && polyConvex s vlast []
                  polyConvex s vlast []
                      = let newSign = crossProduct vlast vfirst vsecond
                        in newSign * s > 0
            
  
  