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


### Dictionaries ###############################################################
proteccion_dictionary <- list(
  proteccio = list(
    general = c(
      "Tots", "Només protegits", "Sense protecció"
    ),
    espais = c(
      "Paratge Natural d'Interès Nacional", "Parc Nacional", "Parc Natural",
      "Reserva Natural de Fauna Salvatge", "Reserva Natural Parcial",
      "Zona de Protecció"
    )
  ),
  nomein = list(
    general = c(
      "Tots", "Només protegits", "Sense Pein"
    ),
    espais = c(
      "Aiguabarreig Segre-Noguera Pallaresa", "Aiguabarreig Segre-Noguera Ribagorçana",
      "Aiguamolls de l'Alt Empordà", "Aigüestortes", "Alta Garrotxa, l'",
      "Alt Pirineu, l'", "Ancosa-Montagut l'", "Artiga de Lin, Era", "Bessons, els",
      "Capçalera de la Noguera Ribagorçana", "Capçaleres del Ter i del Freser",
      "Cap de Creus", "Castell-Cap Roig", "Cingles de Bertí", "Collegats",
      "Collsacabra", "Conreria-Sant Mateu-Céllecs, la", "Costoja", "Estany de Banyoles",
      "Estany de Sils", "Eth Portilhon", "Faiada de Malpàs, la", "Foix, el", "Gallifa",
      "Gavarres, les", "Gelada", "Guilleries, les", "Marimanha", "Massís de l'Albera",
      "Massís de les Cadiretes", "Massís de les Salines", "Massís del Garraf",
      "Massís del Montseny", "Miracle, el", "Moianès, el", "Montanhes de Les e Bossòst",
      "Montesquiu", "Montgrí, el", "Montmell, el", "Montserrat", "Muntanyes de Begur",
      "Muntanyes de l'Ordal", "Muntanyes de Prades", "Muntanyes de Rocacorba",
      "Muntanyes de Tivissa-Vandellòs", "Naut Aran", "Obagues del Riu Corb", "Olèrdola",
      "Penya-segats de la Muga", "Pinya de Rosa", "Plana de Sant Jordi, la",
      "Ports, els", "Puig de la Banya del Boc", "Riba-roja", "Ribera Salada",
      "Riera de Merlès", "Riera de Navel", "Riera de Sorreigs", "Roques Blanques",
      "Saburella", "Sant Joan de Toran", "Sant Llorenç del Munt i l'Obac",
      "Savassona", "Serra Cavallera", "Serra d'Aubenç", "Serra de Bellmunt",
      "Serra de Boumort", "Serra de Carreu", "Serra de Castelltallat",
      "Serra de Collserola", "Serra del Catllaràs", "Serra de Llaberia",
      "Serra del Montsant", "Serra del Montsec", "Serra del Verd",
      "Serra de Miralles-Queralt", "Serra de Montgrony", "Serra de Montsià",
      "Serra d'Ensija-els Rasos de Peguera", "Serra de Picancel", "Serra de Queralt",
      "Serra de Sant Gervàs", "Serra de Turp", "Serra Llarga", "Serra Mitjana",
      "Serres de Busa-els Bastets-Lord", "Serres de Cardó-el Boix", "Serres del Cadí-el Moixeró",
      "Serres de Milany-Santa Magdalena i Puigsacalm-Bellmunt", "Serres de Montnegre-el Corredor",
      "Serres de Pàndols-Cavalls", "Serres de Pradell-l'Argentera",
      "Serres d'Odèn-Port del Comte", "Tossal Gros de Miramar", "Tossals d'Almatret",
      "Tossa Plana de Lles-Puigpedrós", "Tres Hereus, els", "Turons de Maçanet",
      "Turons de la Plana Ausetana", "Vall Alta de Serradell", "Vall del Riu Llobregós",
      "Volcà de la Crosa", "Zona Volcànica de la Garrotxa"
    )
  ),
  enpes = list(
    general = c(
      "Tots", "Només protegits", "Sense protecció"
    ),
    espais = c(
      "Paratge natural d'interès nacional de cap Gros-cap de Creus",
      "Paratge natural d'interès nacional de la vall del monestir de Poblet",
      "Paratge natural d'interès nacional de la Serra de Rodes",
      "Paratge natural d'interès nacional del massís de l'Albera",
      "Paratge natural d'interès nacional del Massís de Pedraforca",
      "Paratge natural d'interès nacional de Pinya de Rosa",
      "Parc nacional d'Aigüestortes i Estany de Sant Maurici",
      "Parc natural de Cap de Creus",
      "Parc natural de l'Alt Pirineu",
      "Parc natural de la Muntanya de Montserrat",
      "Parc natural de la Zona Volcànica de la Garrotxa",
      "Parc natural del Cadí-Moixeró",
      "Parc natural del Massís de Sant Llorenç del Munt i Serra de l'Obac",
      "Parc Natural del Montsant",
      "Parc natural dels Aiguamolls de l'Empordà",
      "Parc natural dels Ports",
      "Parc Natural Massís del Montseny",
      "Reserva Natural de Fauna Salvatge de l'Aiguabarreig Segre-Noguera Pallaresa",
      "Reserva natural parcial de Baish Aran",
      "Reserva natural parcial de la Capçalera de l'Orlina",
      "Reserva natural parcial de la Fageda de Jordà",
      "Reserva natural parcial de la Llosa",
      "Reserva natural parcial de l'Alt Àneu",
      "Reserva natural parcial de la Muntanya Montserrat",
      "Reserva natural parcial del Barranc de la Trinitat",
      "Reserva natural parcial del Barranc del Titllar",
      "Reserva natural parcial de les Fagedes dels Ports",
      "Reserva natural parcial  del volcà Aiguanegra",
      "Reserva natural parcial  del volcà  Croscat",
      "Reserva natural parcial  del volcà de Santa Margarida",
      "Reserva natural parcial  del volca Montolivet",
      "Reserva natural parcial  del volcà Puig Astrol",
      "Reserva natural parcial de Noguera Pallaresa-Bonaigua",
      "Reserva natural parcial de Riera de Merlès",
      "Reserva natural parcial de St Quirze de Colera",
      "Zona perifèrica de protecció del parc nacional",
      "Zona perifèrica de protecció del Parc natural de la Muntanya de Montserrat"
    )
  ),
  nomxarxa2000 = list(
    general = c(
      "Tots", "Només protegits", "SenseXarxa"
    ),
    espais = c(
      "Aiguabarreig Segre- Noguera Pallaresa", "Aiguabarreig Segre-Noguera Ribagorçana",
      "Aiguamolls de l'Alt Empordà", "Aigüestortes",
      "Alta Garrotxa-Massís de les Salines", "Alt Pallars", "Baish Aran",
      "Barranc de Santes Creus", "Bellmunt-Almenara", "Beneïdor", "Capçaleres del Foix",
      "Capçaleres del Ter i del Freser", "Cap de Creus", "El Montgr-Les Medes-El Baix Ter",
      "El Montmell-Marmellar", "Els Bessons", "Era Artiga de Lin-Eth Portilhon",
      "Estany de Banyoles", "Estany de Sils-Riera de Santa Coloma",
      "Gallifa-Cingles de Bertí", "Garriga d'Empordà", "Granyena",
      "La Faiada de Malpàs i Cambatiri", "L'Albera", "Les Gavarres", "Les Guilleries",
      "Litoral del Baix Empordà", "Massís de Bonastre", "Massís de les Cadiretes",
      "Massís del Montseny", "Montgrony", "Montserrat-Roques Blanques- riu Llobregat",
      "Muntanyes de Prades", "Muntanyes de Rocacorba-Puig de la Baya del Boc",
      "Obagues de la riera de Madrona", "Obagues del riu Corb", "Prepirineu Central català",
      "Rasos de Tubau", "Ribera de l'Algars", "Ribera Salada", "Riberes de l'Alt Segre",
      "Riberes de l'Alt Ter", "Riberes del Baix Ter", "Riera de Clariana",
      "Riera de la Goda", "Riera de Merlès", "Riera de Sorreigs", "Riu Brugent",
      "Riu de la Llosa", "Riu Fluvià", "Riu Gaià", "Riu Llobregat d'Empordà-Riera de Torrelles",
      "Riu Siurana i planes del Priorat", "Sant Llorenç del Munt i l'Obac",
      "Secans de la Noguera", "Serra Cavallera",
      "Serra d'Aubenç i Roc de Cogul", "Serra de Boumort- Collegats",
      "Serra de Castelltallat", "Serra de Catllaràs", "Serra de Collserola",
      "Serra de Montsant-Pas de l'Ase", "Serra de Montsià", "Serra de Prada-Castellàs",
      "Serra de Turp i Mora Condal-Valldan", "Serres de Cardó - El Boix",
      "Serres del Litoral central", "Serres del litoral septentrional",
      "Serres del Montsec, Sant Mamet i Mitjana",
      "Serres de Queralt i Els Tossals-Aigua d'Ora", "Sistema prelitoral central",
      "Sistema prelitoral meridional", "Sistema transversal Català",
      "Tivissa-Vandellós-Llaberia", "Tossal de Montagut", "Tossals d'Almatret i Riba-roja",
      "Tossa Plana de Lles-Puigpedrós", "Vall Alta de Serradell - Serra de Sant Gervàs",
      "Vall la Vinaixa", "Valls de l'Anoia", "Valls del Sió-Llobregós",
      "Vessants de la Noguera Ribagorçana", "Zona exclosa", "Zona Volcànica de la Garrotxa"
    )
  )
)


################################################################################

## On.Stop routine ####
onStop(
  function() {
    poolClose(oracle_ifn)
  }
)
