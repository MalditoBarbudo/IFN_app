# Global script

# data for polygons (municipis, comarques, vegueries i provincies)

################################################################################
# CREATION data
# 
# 
# polygons_municipis <- readOGR('shapefiles', 'bm5mv20sh0tpm1_20180101_0',
#                               GDAL1_integer64_policy = TRUE) %>%
#   spTransform(CRS("+proj=longlat +datum=WGS84"))
# 
# polygons_comarques <- readOGR('shapefiles', 'bm5mv20sh0tpc1_20180101_0',
#                               GDAL1_integer64_policy = TRUE) %>%
#   spTransform(CRS("+proj=longlat +datum=WGS84"))
# 
# polygons_vegueries <- readOGR('shapefiles', 'bm5mv20sh0tpv1_20180101_0',
#                               GDAL1_integer64_policy = TRUE) %>%
#   spTransform(CRS("+proj=longlat +datum=WGS84"))
# 
# polygons_provincies <- readOGR('shapefiles', 'bm5mv20sh0tpp1_20180101_0',
#                               GDAL1_integer64_policy = TRUE) %>%
#   spTransform(CRS("+proj=longlat +datum=WGS84"))
# 
# save(
#   polygons_municipis, polygons_comarques,
#   polygons_vegueries, polygons_provincies,
#   file = 'shapefiles/polygons.RData'
# )
################################################################################

# load('shapefiles/polygons.RData')

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
