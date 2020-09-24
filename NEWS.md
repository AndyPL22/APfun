# APfun 0.1.5

Fixed `APpolygonize` so it uses Python3 in OSGeo's Windows installation instead of Python 2

# APfun 0.1.4

Variety of small bug fixes required for other libraries

# APfun 0.1.3

`APcontours` - Added ability to control maximum number of segments per contour line. Ensured that output object's 'levels' attribute was numeric instead of a string.
`APpolygonize` - Added 'outFile' argument
`AProunder` - Allow input of 'Extent' objects

# APfun 0.1.2

Added following functions to facilitate reading and writing of Shapefiles

`APSHPread` - Read a Shapefile
`APSHPsave` - Save a Shapefile
`APKML2SHP` - Convert a KML to a Shapefile
`APSHPfiles` - List all files associated with a Shapefile (ex.: yourfile.shp, yourfile.xml, yourfile.prj, etc.)
`APSHPdel` - Delete Shapefile and all associated files

In addition, the 'APpolyMerge' function was added, which facilitates merging multiple polygonal Spatial objects.

# APfun 0.1.1

Added following functions:

`APcontours` - Generates elevation contours at 'tidy' height values
`APprecise` - Prints a number with all decimal values (i.e. no rounding)
`APopen` - Opens a folder in Windows Explorer
`APpolygonize` - Converts a raster into polygons using a GDAL utility

# APfun 0.0.0.9001

* Initial release of APfun



