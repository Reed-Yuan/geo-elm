{-
    TMS Global Mercator Profile
    ---------------------------

    Functions necessary for generation of tiles in Spherical Mercator projection,
    EPSG:900913 (EPSG:gOOglE, Google Maps Global Mercator), EPSG:3785, OSGEO:41001.

    Such tiles are compatible with Google Maps, Microsoft Virtual Earth, Yahoo Maps,
    UK Ordnance Survey OpenSpace API, ...
    and you can overlay them on top of base maps of those web mapping applications.
    
    Pixel and tile coordinates are in TMS notation (origin [0,0] in bottom-left).

    What coordinate conversions do we need for TMS Global Mercator tiles::

         LatLon      <->       Meters      <->     Pixels    <->       Tile     

     WGS84 coordinates   Spherical Mercator  Pixels in pyramid  Tiles in pyramid
         lat/lon            XY in metres     XY pixels Z zoom      XYZ from TMS 
        EPSG:4326           EPSG:900913                                         
         .----.              ---------               --                TMS      
        /      \     <->     |       |     <->     /----/    <->      Google    
        \      /             |       |           /--------/          QuadTree   
         -----               ---------         /------------/                   
       KML, public         WebMapService         Web Clients      TileMapService

    What is the coordinate extent of Earth in EPSG:900913?

      [-20037508.342789244, -20037508.342789244, 20037508.342789244, 20037508.342789244]
      Constant 20037508.342789244 comes from the circumference of the Earth in meters,
      which is 40 thousand kilometers, the coordinate origin is in the middle of extent.
      In fact you can calculate the constant as: 2 * math.pi * 6378137 / 2.0
      $ echo 180 85 | gdaltransform -s_srs EPSG:4326 -t_srs EPSG:900913
      Polar areas with abs(latitude) bigger then 85.05112878 are clipped off.

    What are zoom level constants (pixels/meter) for pyramid with EPSG:900913?

      whole region is on top of pyramid (zoom=0) covered by 256x256 pixels tile,
      every lower zoom level resolution is always divided by two
      initialResolution = 20037508.342789244 * 2 / 256 = 156543.03392804062

    What is the difference between TMS and Google Maps/QuadTree tile name convention?

      The tile raster itself is the same (equal extent, projection, pixel size),
      there is just different identification of the same raster tile.
      Tiles in TMS are counted from [0,0] in the bottom-left corner, id is XYZ.
      Google placed the origin [0,0] to the top-left corner, reference is XYZ.
      Microsoft is referencing tiles by a QuadTree name, defined on the website:
      http://msdn2.microsoft.com/en-us/library/bb259689.aspx

    The lat/lon coordinates are using WGS84 datum, yeh?

      Yes, all lat/lon we are mentioning should use WGS84 Geodetic Datum.
      Well, the web clients like Google Maps are projecting those coordinates by
      Spherical Mercator, so in fact lat/lon coordinates on sphere are treated as if
      the were on the WGS84 ellipsoid.
     
      From MSDN documentation:
      To simplify the calculations, we use the spherical form of projection, not
      the ellipsoidal form. Since the projection is used only for map display,
      and not for displaying numeric coordinates, we don't need the extra precision
      of an ellipsoidal projection. The spherical projection causes approximately
      0.33 percent scale distortion in the Y direction, which is not visually noticable.

    How do I create a raster in EPSG:900913 and convert coordinates with PROJ.4?

      You can use standard GIS tools like gdalwarp, cs2cs or gdaltransform.
      All of the tools supports -t_srs 'epsg:900913'.

      For other GIS programs check the exact definition of the projection:
      More info at http://spatialreference.org/ref/user/google-projection/
      The same projection is degined as EPSG:3785. WKT definition is in the official
      EPSG database.

      Proj4 Text:
        +proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0
        +k=1.0 +units=m +nadgrids=@null +no_defs

      Human readable WKT format of EPGS:900913:
         PROJCS["Google Maps Global Mercator",
             GEOGCS["WGS 84",
                 DATUM["WGS_1984",
                     SPHEROID["WGS 84",6378137,298.2572235630016,
                         AUTHORITY["EPSG","7030"]],
                     AUTHORITY["EPSG","6326"]],
                 PRIMEM["Greenwich",0],
                 UNIT["degree",0.0174532925199433],
                 AUTHORITY["EPSG","4326"]],
             PROJECTION["Mercator_1SP"],
             PARAMETER["central_meridian",0],
             PARAMETER["scale_factor",1],
             PARAMETER["false_easting",0],
             PARAMETER["false_northing",0],
             UNIT["metre",1,
                 AUTHORITY["EPSG","9001"]]]
-}

module Proj where

import Bitwise
import Debug

tileSize=256

initialResolution = 2 * pi * 6378137 / (toFloat tileSize)
-- 156543.03392804062 for tileSize 256 pixels

originShift = pi * 6378137
-- 20037508.342789244

-- Converts given lat/lon in WGS84 Datum to XY in Spherical Mercator EPSG:900913
latLonToMeters  (lat, lon) = let
        mx = lon * originShift / 180.0
        my_ = tan ((90 + lat) * pi / 360.0 ) |> logBase e |> flip (/) (pi / 180.0)

        my = my_ * originShift / 180.0
    in (mx, my)

--Converts XY point from Spherical Mercator EPSG:900913 to lat/lon in WGS84 Datum
metersToLatLon (mx, my) = let
        lon = (mx / originShift) * 180.0
        lat_ = (my / originShift) * 180.0
        x = lat_ * pi / 180.0

        lat = 180 / pi * (2 * (atan (e ^ x)) - pi / 2.0)
    in (lat, lon)

--Converts pixel coordinates in given zoom level of pyramid to EPSG:900913"
pixelsToMeters : (Int, Int) -> Int -> (Float, Float)
pixelsToMeters (px, py) zoom = let
        res = resolution zoom 
        mx = (toFloat px) * res - originShift
        my = (toFloat py) * res - originShift
    in (mx, my)
    
--Converts EPSG:900913 to pyramid pixel coordinates in given zoom level
metersToPixels (mx, my) zoom = let
        res = resolution zoom 
        px = (mx + originShift) / res |> round
        py = (my + originShift) / res |> round
    in (px, py)
    
-- Returns a tile covering region in given pixel coordinates
pixelsToTile : (Int, Int) -> (Int, Int)
pixelsToTile (px, py) = let
        tx =  floor ( (toFloat px) / (toFloat tileSize)) 
        ty =  floor ( (toFloat py) / (toFloat tileSize)) 
    in (tx, ty)
    
-- Move the origin of pixel coordinates to top-left corner
pixelsToRaster (px, py) zoom = let
        mapSize = Bitwise.shiftLeft tileSize zoom
    in (px, mapSize - py)

-- Returns tile for given mercator coordinates
metersToTile : (Float, Float) -> Int -> (Int, Int)
metersToTile (mx, my) zoom = let
        (px, py) = metersToPixels (mx, my) zoom
    in pixelsToTile ( px, py)

-- Returns bounds of the given tile in EPSG:900913 coordinates
tileBounds (tx, ty) zoom = let
        (minx, miny) = pixelsToMeters ( tx * tileSize, ty * tileSize) zoom
        (maxx, maxy) = pixelsToMeters ((tx+1) * tileSize, (ty+1) * tileSize) zoom 
    in ( minx, miny, maxx, maxy )

-- Returns bounds of the given tile in latutude/longitude using WGS84 datum
tileLatLonBounds (tx, ty) zoom = let 
        (minx, miny, maxx, maxy ) = tileBounds (tx, ty) zoom
        (minLat, minLon) = metersToLatLon (minx, miny)
        (maxLat, maxLon) = metersToLatLon (maxx, maxy)
    in ( minLat, minLon, maxLat, maxLon )
    
--Resolution (meters/pixel) for given zoom level (measured at Equator)"
resolution : Int -> Float
resolution zoom = 
    --return (2 * math.pi * 6378137) / (self.tileSize * 2**zoom)
    initialResolution / (toFloat (2 ^ zoom))
    
-- Maximal scaledown zoom of the pyramid closest to the pixelSize.    
zoomForPixelSize pixelSize = let
        nonNeg x = if x < 0 then 0 else x
        f i = if pixelSize > (resolution i) then (i - 1 |> nonNeg)
                else if i > 30 then 30
                else f (i + 1)
    in f 0

            
-- Converts TMS tile coordinates to Google Tile coordinates
googleTile : (Int, Int) -> Int -> (Int, Int)
googleTile (tx, ty) zoom = 
    -- coordinate origin is moved from bottom-left to top-left corner of the extent
    (tx, (2 ^ zoom - 1) - ty)

{- Not implemented yet   
def QuadTree(self, tx, ty, zoom ):
    "Converts TMS tile coordinates to Microsoft QuadTree"
    
    quadKey = ""
    ty = (2**zoom - 1) - ty
    for i in range(zoom, 0, -1):
        digit = 0
        mask = 1 << (i-1)
        if (tx & mask) != 0:
            digit += 1
        if (ty & mask) != 0:
            digit += 2
        quadKey += str(digit)
        
    return quadKey
-}

-- Convert (lat, lon) to TMS coordinates
latLon2Tile : (Float, Float) -> Int -> (Int, Int)
latLon2Tile (lat, lon) zoom = let
        (mx, my) = latLonToMeters (lat, lon)
    in metersToTile (mx, my) zoom
    
-- Convert (lat, lon) to Google Tile coordinates
latLonToGoogleTile : (Float, Float) -> Int -> (Int, Int)
latLonToGoogleTile (lat, lon) zoom = let 
        (x_, y_) = latLon2Tile (lat,lon) zoom
    in googleTile (x_, y_) zoom

-- Convert (lat, lon) to pixel
latLonToPixel (lat, lon) zoom = let
        (mx, my) = latLonToMeters (lat, lon)
    in metersToPixels (mx, my) zoom
    
{- Deprecated functions     
lon2tilex lon z = floor ((lon + 180.0) / 360.0 * (2.0 ^ z))
lat2tiley lat z = floor ((1.0 - logBase e (tan (lat * pi/180.0) 
                + 1.0 / cos (lat * pi/180.0)) / pi) / 2.0 * (2.0 ^ z))
tilex2lon x z = x / (2.0 ^ z) * 360.0 - 180
tiley2lat y z = let n = pi - 2.0 * pi * y / (2.0 ^ z) in 180.0 / pi * atan (0.5 * (e ^ n) - (e ^ -n))
-}
