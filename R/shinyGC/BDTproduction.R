# Villes à produire
# # 78
# nom de commune, insee
# Conflans-Sainte-Honorine,	78172
# Saint-Rémy-lès-Chevreuse,	78575
# Les Mureaux,	78440
# Freneuse,	78255
# Auffargis,	78030
# Les Essarts-le-Roi,	78220
# Orgeval,	78466
# Rosny-sur-Seine,	78531
# Saint-Léger-en-Yvelines,	78562
# Poigny-la-Forêt,	78497
# Blaru,	78068
# Paray-Douaville,	78478
# Le Tartre-Gaudran,	78606

# Test conflans 2011 : OK

test<-geoClimateConfigFile(wf = "BDT",
   date="2011",
   BDTinFolder="/home/gousseff/Documents/3_data/BDTOPO/2011/",
   locations="78172",
   outConfigFile="",
   outConfigDir = "/home/gousseff/Documents/0_DocBiblioTutosPublis/0_ArticlesScientEtThèses/ArticleComparaisonLCZGCWUDAPTEXPERTS",
   outFolder = "/home/gousseff/Documents/0_DocBiblioTutosPublis/0_ArticlesScientEtThèses/ArticleComparaisonLCZGCWUDAPTEXPERTS",
   rsuIndics = c("LCZ"),
   writeNow = TRUE,
   forceSRID=TRUE)

geoClimateCall(
  jarFilePath= "/home/gousseff/Documents/2_CodesSources/GeoClimate/Release1.0/geoclimate-1.0.0.jar",
  configFilePath="/home/gousseff/Documents/0_DocBiblioTutosPublis/0_ArticlesScientEtThèses/ArticleComparaisonLCZGCWUDAPTEXPERTS/78172BDT2011.json",
  wf="BDTOPO_V2")

# Test conflans 2022 :

test<-geoClimateConfigFile(wf = "BDT",
                           date="2022",
                           BDTinFolder="/home/gousseff/Documents/3_data/BDTOPO/2022/",
                           locations="78172",
                           outConfigFile="",
                           outConfigDir = "/home/gousseff/Documents/0_DocBiblioTutosPublis/0_ArticlesScientEtThèses/ArticleComparaisonLCZGCWUDAPTEXPERTS",
                           outFolder = "/home/gousseff/Documents/0_DocBiblioTutosPublis/0_ArticlesScientEtThèses/ArticleComparaisonLCZGCWUDAPTEXPERTS",
                           rsuIndics = c("LCZ"),
                           writeNow = TRUE,
                           forceSRID=TRUE)

geoClimateCall(
  jarFilePath= "/home/gousseff/Documents/2_CodesSources/GeoClimate/Release1.0/geoclimate-1.0.0.jar",
  configFilePath="/home/gousseff/Documents/0_DocBiblioTutosPublis/0_ArticlesScientEtThèses/ArticleComparaisonLCZGCWUDAPTEXPERTS/78172BDT2022.json",
  wf="BDTOPO_V3")

city_code_78_93<-c("78172", "78575", "78440", "78255",	"78030", "78220", "78466",
                 "78531", "78562", "78497", "78068", "78478", "78606", "93029",	"93066",	"93048")

configDir<-"/home/gousseff/Documents/0_DocBiblioTutosPublis/0_ArticlesScientEtThèses/ArticleComparaisonLCZGCWUDAPTEXPERTS"
for (i in city_code_78){
  # 2011
  geoClimateConfigFile(wf = "BDT",
                       date="2011",
                       BDTinFolder="/home/gousseff/Documents/3_data/BDTOPO/2011/78/",
                       locations=i,
                       outConfigFile="",
                       outConfigDir = configDir,
                       outFolder = configDir,
                       rsuIndics = c("LCZ"),
                       writeNow = TRUE,
                       forceSRID=TRUE)

  confFile<-paste0(configDir, "/", i, "BDT2011.json")

  geoClimateCall(
    jarFilePath= "/home/gousseff/Documents/2_CodesSources/GeoClimate/Release1.0/geoclimate-1.0.0.jar",
    configFilePath=confFile,
    wf="BDTOPO_V2")


  # 2022
  geoClimateConfigFile(wf = "BDT",
                       date="2022",
                       BDTinFolder="/home/gousseff/Documents/3_data/BDTOPO/2022/78/",
                       locations=i,
                       outConfigFile="",
                       outConfigDir = configDir,
                       outFolder = configDir,
                       rsuIndics = c("LCZ"),
                       writeNow = TRUE,
                       forceSRID=TRUE)

  confFile<-paste0(configDir, "/", i, "BDT2022.json")

  geoClimateCall(
    jarFilePath= "/home/gousseff/Documents/2_CodesSources/GeoClimate/Release1.0/geoclimate-1.0.0.jar",
    configFilePath=confFile,
    wf="BDTOPO_V3")
  }
