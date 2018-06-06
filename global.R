# Global script

# data for polygons (municipis, comarques, vegueries i provincies)

################################################################################
# CREATION data
# 
# polygons_municipis <- readOGR('shapefiles', 'bm1000mv33sh1fpm1r170',
#                               GDAL1_integer64_policy = FALSE) %>%
#   spTransform(CRS("+proj=longlat +datum=WGS84"))
# 
# polygons_comarques <- readOGR('shapefiles', 'bm1000mv33sh1fpc1r170',
#                               GDAL1_integer64_policy = FALSE) %>%
#   spTransform(CRS("+proj=longlat +datum=WGS84"))
# 
# polygons_vegueries <- readOGR('shapefiles', 'bm500mv20sh0tpv1_20180101_0',
#                               GDAL1_integer64_policy = FALSE) %>%
#   spTransform(CRS("+proj=longlat +datum=WGS84"))
# 
# polygons_provincies <- readOGR('shapefiles', 'bm1000mv33sh1fpp1r170',
#                                GDAL1_integer64_policy = FALSE) %>%
#   spTransform(CRS("+proj=longlat +datum=WGS84"))
# 
# save(
#   polygons_municipis, polygons_comarques,
#   polygons_vegueries, polygons_provincies,
#   file = 'shapefiles/polygons.RData'
# )
################################################################################

load('shapefiles/polygons.RData')

oracle_ifn <- dbPool(
  drv = RPostgreSQL::PostgreSQL(),
  user = 'ifn',
  password = rstudioapi::askForPassword('Password for ifn'),
  dbname = 'oracle_ifn',
  idleTimeout = 3600000
)

## On.Stop routine ####
onStop(
  function() {
    poolClose(oracle_ifn)
  }
)
