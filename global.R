# Global script

# data for polygons (municipis, comarques, vegueries i provincies)

################################################################################
# CREATION data
# 
# 
# polygons_municipis <- readOGR('shapefiles', 'bm5mv20sh0tpm1_20180101_0') %>%
#   spTransform(CRS("+proj=longlat +datum=WGS84"))
# 
# polygons_comarques <- readOGR('shapefiles', 'bm5mv20sh0tpc1_20180101_0') %>%
#   spTransform(CRS("+proj=longlat +datum=WGS84"))
# 
# polygons_vegueries <- readOGR('shapefiles', 'bm5mv20sh0tpv1_20180101_0') %>%
#   spTransform(CRS("+proj=longlat +datum=WGS84"))
# 
# polygons_provincies <- readOGR('shapefiles', 'bm5mv20sh0tpp1_20180101_0') %>%
#   spTransform(CRS("+proj=longlat +datum=WGS84"))
# 
# save(
#   polygons_municipis, polygons_comarques,
#   polygons_vegueries, polygons_provincies,
#   file = 'shapefiles/polygons.RData'
# )
################################################################################

load('shapefiles/polygons.RData')

# data connection
oracle_ifn <- DBI::dbConnect(
  RPostgreSQL::PostgreSQL(),
  user = 'ifn',
  password = rstudioapi::askForPassword('Password for ifn'),
  dbname = 'oracle_ifn'
)

# data_parcelas <- tbl(oracle_ifn, 'parcelaifn2_clima') %>%
#   select(idparcela, precipitacioanual, temperaturamitjanaanual) %>%
#   inner_join(tbl(oracle_ifn, 'parcelaifn2_sig'), by = 'idparcela') %>%
#   collect()
# 
# coordinates_parcelas <- data_parcelas[,c('idparcela', 'utm_x', 'utm_y')]
# coordinates(coordinates_parcelas) <- ~utm_x+utm_y
# proj4string(coordinates_parcelas) <- CRS("+init=epsg:25831")
# 
# coordinates_par_transf <- spTransform(
#   coordinates_parcelas, CRS("+proj=longlat +datum=WGS84")
# )
# 
# data_parcelas <- data_parcelas %>%
#   mutate(long = coordinates_par_transf@coords[,1],
#          lat = coordinates_par_transf@coords[,2])


