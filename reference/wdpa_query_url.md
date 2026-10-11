# Default WDPA ArcGIS FeatureServer query endpoint

The public ArcGIS `FeatureServer` layer that serves the World Database
on Protected Areas (WDPA) polygons, used by
[`wdpa_areas()`](https://lucasbarreirageo.github.io/mappingAS/reference/wdpa_areas.md)
as a global alternative to the Brazil-only ICMBio Conservation Units.
This is the layer behind the ArcGIS Living Atlas "WDPA" item and returns
GeoJSON on demand, so no Google Earth Engine or bulk download is
required.

## Usage

``` r
wdpa_query_url()
```

## Value

A length-1 character URL (the `/query` endpoint).

## See also

<https://www.protectedplanet.net/en>,
[`wdpa_areas()`](https://lucasbarreirageo.github.io/mappingAS/reference/wdpa_areas.md)
