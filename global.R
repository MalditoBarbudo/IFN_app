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
# polygons_enpe <- readOGR('shapefiles', 'enpe_2017',
#                          GDAL1_integer64_policy = FALSE) %>%
#   spTransform(CRS("+proj=longlat +datum=WGS84"))
# 
# polygons_pein <- readOGR('shapefiles', 'pein_2017',
#                          GDAL1_integer64_policy = FALSE) %>%
#   spTransform(CRS("+proj=longlat +datum=WGS84"))
# 
# polygons_xn2000 <- readOGR('shapefiles', 'xn2000_2017',
#                          GDAL1_integer64_policy = FALSE) %>%
#   spTransform(CRS("+proj=longlat +datum=WGS84"))
# 
# save(
#   polygons_municipis, polygons_comarques,
#   polygons_vegueries, polygons_provincies,
#   polygons_enpe, polygons_pein, polygons_xn2000,
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
      "Tots", "Només espais d'interès nacional", "Sense Pein"
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
      "Tots", "Només espai de protecció especial", "Sense protecció"
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
      "Tots", "Només en Xarxa Natura 2000", "SenseXarxa"
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

clima_vars_dictionary <- c(
  'radiacioanual', "radiaciogener", "radiaciofebrer", "radiaciomarç", "radiacioabril",
  "radiaciomaig", "radiaciojuny", "radiaciojuliol", "radiacioagost", "radiaciosetembre",
  "radiaciooctubre", "radiacionovembre", "radiaciodesembre", "temperaturaminimaanual",
  "temperaturaminimagener", "temperaturaminimafebrer", "temperaturaminimamarç",
  "temperaturaminimaabril", "temperaturaminimamaig", "temperaturaminimajuny",
  "temperaturaminimajuliol", "temperaturaminimaagost", "temperaturaminimasetembre",
  "temperaturaminimaoctubre", "temperaturaminimanovembre", "temperaturaminimadesembre",
  "temperaturamitjanaanual", "temperaturamitjanagener", "temperaturamitjanafebrer",
  "temperaturamitjanamarç", "temperaturamitjanaabril", "temperaturamitjanamaig",
  "temperaturamitjanajuny", "temperaturamitjanajuliol", "temperaturamitjanaagost",
  "temperaturamitjanasetembre", "temperaturamitjanaoctubre", "temperaturamitjananovembre",
  "temperaturamitjanadesembre", "temperaturamaximaanual", "temperaturamaximagener",
  "temperaturamaximafebrer", "temperaturamaximamarç", "temperaturamaximaabril",
  "temperaturamaximamaig", "temperaturamaximajuny", "temperaturamaximajuliol",
  "temperaturamaximaagost", "temperaturamaximasetembre", "temperaturamaximaoctubre",
  "temperaturamaximanovembre", "temperaturamaximadesembre", "precipitacioanual",
  "precipitaciogener", "precipitaciofebrer", "precipitaciomarç", "precipitacioabril",
  "precipitaciomaig", "precipitaciojuny", "precipitaciojuliol", "precipitacioagost",
  "precipitaciosetembre", "precipitaciooctubre", "precipitacionovembre",
  "precipitaciodesembre", "etp_gener", "etp_febrer", "etp_març", "etp_abril",
  "etp_maig", "etp_juny", "etp_juliol", "etp_agost", "etp_setembre", "etp_octubre",
  "etp_novembre", "etp_desembre", "etr_p_gener", "etr_p_febrer", "etr_p_març",
  "etr_p_abril", "etr_p_maig", "etr_p_juny", "etr_p_juliol", "etr_p_agost",
  "etr_p_setembre", "etr_p_octubre", "etr_p_novembre", "etr_p_desembre",
  "npp_p", "etr_s_gener", "etr_s_febrer", "etr_s_març", "etr_s_abril", "etr_s_maig",
  "etr_s_juny", "etr_s_juliol", "etr_s_agost", "etr_s_setembre", "etr_s_octubre",
  "etr_s_novembre", "etr_s_desembre", "npp_s"
)

polygons_dictionary <- list(
  
  ## admin divs
  
  provincia = list(
    polygon = 'polygons_provincies',
    group = 'provincia',
    label = ~NOM_PROV,
    label_new = ~provincia,
    layerId = 'nom_provincies',
    # color_var = ~pal(NOM_PROV),
    label_chr = 'NOM_PROV'
  ),
  
  vegueria = list(
    polygon = 'polygons_vegueries',
    group = 'vegueria',
    label = ~NOMVEGUE,
    label_new = ~vegueria,
    layerId = 'nom_vegueries',
    # color_var = ~pal(NOMVEGUE),
    label_chr = 'NOMVEGUE'
  ),
  
  comarca = list(
    polygon = 'polygons_comarques',
    group = 'comarca',
    label = ~NOM_COMAR,
    label_new = ~comarca,
    layerId = 'nom_comarques',
    # color_var = ~pal(NOM_COMAR),
    label_chr = 'NOM_COMAR'
  ),
  
  municipi = list(
    polygon = 'polygons_municipis',
    group = 'municipi',
    label = ~NOM_MUNI,
    label_new = ~municipi,
    layerId = 'nom_municipis',
    # color_var = ~pal(NOM_MUNI),
    label_chr = 'NOM_MUNI'
  ),
  
  ## espai tipus
  
  nomein = list(
    polygon = 'polygons_pein',
    group = 'nomein',
    label = ~nom,
    layerId = 'nom_pein'
  ),
  
  enpes = list(
    polygon = 'polygons_enpe',
    group = 'enpes',
    label = ~nom,
    layerId = 'nom_enpe'
  ),
  
  nomxarxa2000 = list(
    polygon = 'polygons_xn2000',
    group = 'nomxarxa2000',
    label = ~nom_n2,
    layerId = 'nom_xn2000'
  )
  
)

### inputs choices #############################################################
# inputs choices
ifns <- c(
  'IFN 2' = 'ifn2',
  'IFN 3' = 'ifn3',
  'IFN 4' = 'ifn4',
  'IFN 3 respecte a IFN 2' = 'ifn3_ifn2',
  'IFN 4 respecte a IFN 3' = 'ifn3_ifn4'
)

admin_divs <- c(
  Catalunya = '', Provincies = 'provincia', Vegueries = 'vegueria',
  Comarques = 'comarca', Municipis = 'municipi'
)

noms_divs = list(
  comarca = c('Totes' = '', sort(as.character(polygons_comarques@data$NOM_COMAR))),
  municipi = c('Tots' = '', sort(as.character(polygons_municipis@data$NOM_MUNI))),
  vegueria = c('Totes' = '', sort(as.character(polygons_vegueries@data$NOMVEGUE))),
  provincia = c('Totes' = '', sort(as.character(polygons_provincies@data$NOM_PROV)))
)

espai_tipus <- c(
  'Nivell de protecció' = 'proteccio',
  "Espai d'interès Nacional" = 'nomein',
  "Espai de protecció especial" = 'enpes',
  "Xarxa Natura 2000" = 'nomxarxa2000'
)

agg_levels <- list(
  
  "Parcel·les" = list(
    'Parcel·les' = 'parcela',
    'Parcel·les desglossat per Espècie' = 'especie',
    'Parcel·les desglossat per Espècie simplificat' = 'espsimple',
    'Parcel·les desglossat per Gènere' = 'genere',
    'Parcel·les desglossat per Conífera/Caducifoli/Esclerofil·le' = 'cadesclcon',
    'Parcel·les desglossat per Conífera/Planifoli' = 'plancon'
  ),
  
  "Grups funcionals" = list(
    'Espècie' = 'especie_rt',
    'Espècie simplificat' = 'espsimple_rt',
    'Gènere' = 'genere_rt',
    'Conífera/Caducifoli/Esclerofil·le' = 'cadesclcon_rt',
    'Conífera/Planifoli' = 'plancon_rt'
  ),
  
  'Administratiu' = list(
    "Divisions seleccionats" = 'territori_parcela_rt',
    "Divisions desglossat per Espècie" = 'territori_especie_rt',
    "Divisions desglossat per Espècie simplificat" = 'territori_espsimple_rt',
    "Divisions desglossat per Génere" = 'territori_genere_rt',
    "Divisions desglossat per Conífera/Caducifoli/Esclerofil·le" = 'territori_cadesclcon_rt',
    "Divisions desglossat per Conífera/Planifoli" = 'territori_plancon_rt'
  )
)

#### ggplot theme for infoPanel ################################################
theme_infoPanel <- theme_void() + theme(
  
  # line general
  # line = element_line(
  #   colour = 'black', size = 0.5, linetype = 1, lineend = 'square', arrow = NULL,
  #   
  # )
  
  # y axis (line, ticks, text)
  axis.line.y = element_line(
    colour = 'black', size = 1, linetype = 1, lineend = 'square',
    inherit.blank = FALSE
  ),
  axis.ticks.y = element_line(
    colour = 'black', size = 1, linetype = 1, lineend = 'square',
    inherit.blank = FALSE
  ),
  axis.ticks.length = unit(2, 'mm'),
  axis.text.y = element_text(
    family = 'sans', colour = 'black', size = 11, hjust = 0.5, vjust = 0.5,
    angle = 0, lineheight = 0.9, margin = margin(0, 2.2, 0, 0),
    inherit.blank = FALSE
  ),
  
  # x axis (only text)
  axis.text.x = element_text(
    family = 'sans', colour = 'black', size = 11, hjust = 0.5, vjust = 0.5,
    angle = 45, lineheight = 0.9, margin = margin(2.2, 0, 0, 0),
    inherit.blank = FALSE
  ),
  
  # legend position none by default
  legend.position = 'none',
  
  # plot title and subtitle
  plot.title = element_text(
    family = 'sans', face = 'bold', colour = 'black',
    size = rel(1.2), hjust = 0.4, vjust = 1.0,
    angle = 0, lineheight = 0.9, margin = margin(0, 0, 5.5, 0),
    inherit.blank = FALSE
  ),
  
  plot.subtitle = element_text(
    family = 'sans', face = 'plain', colour = 'black',
    size = rel(1), hjust = 0.4, vjust = 0.8,
    angle = 0, lineheight = 0.9, margin = margin(0, 0, 3.2, 0),
    inherit.blank = FALSE
  )
  
)

#### Helper functions ##########################################################

data_generator <- function(
  sql_db,
  ifn,
  agg,
  cd,
  data_sig,
  admin_div,
  .funs
) {
  
  # plots to use, based on data_sig
  idparcelas <- data_sig %>% pull(idparcela)
  # diameter classes
  cd_real <- if (isTRUE(cd)) {'cd'} else {''}
  
  
  # real time calculations
  if (stringr::str_detect(agg, '_rt')) {
    
    # polygons
    if (stringr::str_detect(agg, 'territori_')) {
      
      # get the "official" aggregation level name
      agg_real <- stringr::str_remove(agg, '_rt') %>%
        stringr::str_remove('territori_')
      
      # get the core data name based on agg and cd
      if (agg_real == 'parcela') {
        if (isTRUE(cd)) {
          core_name <- paste0('r_', cd, '_', ifn)
        } else {
          core_name <- paste0('r_', ifn)
        }
        
        # data to return
        core_tbl <- tbl(sql_db, core_name) %>% collect()
        res <- data_sig %>%
          select(idparcela, !!sym(admin_div)) %>% 
          inner_join(core_tbl, by = 'idparcela') %>%
          group_by(!!sym(admin_div)) %>%
          summarise_if(is.numeric, .funs = .funs)
      } else {
        # core data name
        core_name <- paste0('r_', paste0(agg_real, cd_real, '_'), ifn)
        # tipu funcional variable
        agg_tipfun_var <- paste0('id', agg_real)
        
        # data to return
        core_tbl <- tbl(sql_db, core_name) %>% collect()
        res <- data_sig %>%
          select(idparcela, !!sym(admin_div)) %>%
          inner_join(core_tbl, by = 'idparcela') %>%
          group_by(!!sym(admin_div), !!sym(agg_tipfun_var)) %>%
          summarise_if(is.numeric, .funs = .funs)
      }
      
    } else {
      # tipus funcionals aggregations without polygons
      
      # get the "official" aggregation level name
      agg_real <- stringr::str_remove(agg, '_rt')
      
      # get the tipu funcinal variable
      agg_tipfun_var <- paste0('id', agg_real)
      
      # get the core data name
      core_name <- paste0('r_', paste0(agg_real, cd_real, '_'), ifn)
      
      # data to return
      res <- tbl(sql_db, core_name) %>%
        collect() %>%
        filter(idparcela %in% idparcelas) %>%
        group_by(!!sym(agg_tipfun_var)) %>% 
        summarise_if(is.numeric, .funs = .funs)
    }
    
  } else {
    
    # plot agg level, or plot/tipu funcional agg levels (not real time
    # calculations)
    if (agg == 'parcela') {
      if (isTRUE(cd)) {
        core_name <- paste0('r_', cd_real, '_', ifn)
      } else {
        core_name <- paste0('r_', ifn)
      }
    } else {
      core_name <- paste0('r_', paste0(agg, cd, '_'), ifn)
    }
    
    res <- tbl(sql_db, core_name) %>%
      collect() %>%
      filter(idparcela %in% idparcelas)
    
  }
  
  return(res)
  
}


################################################################################

## On.Stop routine ####
onStop(
  function() {
    poolClose(oracle_ifn)
  }
)
