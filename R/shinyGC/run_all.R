city_names_78_93<-c("Conflans-Sainte-Honorine", "Saint-Rémy-lès-Chevreuse",	"Les Mureaux",	"Freneuse",	"Auffargis",
                    "Les Essarts-le-Roi",	"Orgeval",	"Rosny-sur-Seine", "Saint-Léger-en-Yvelines",	"Poigny-la-Forêt",	"Blaru",
                    "Paray-Douaville",	"Le Tartre-Gaudran", "Drancy", "Saint-Denis", "Montreuil" )

configDir<-"/home/gousseff/Documents/0_DocBiblioTutosPublis/0_ArticlesScientEtThèses/ArticleComparaisonLCZGCWUDAPTEXPERTS"
outFolderRelease<-paste0(configDir,"/geojsonOut")
jarPath<-"/home/gousseff/Documents/2_CodesSources/GeoClimate/GC_2024_05_14/geoclimate-1.0.1-SNAPSHOT.jar"
jarPathRelease<-"/home/gousseff/Documents/2_CodesSources/GeoClimate/Release1.0/geoclimate-1.0.0.jar"


for (i in city_names_78_93) {

  # 2011
  geoClimateConfigFile(wf = "OSM", locations = i,
                       date="2011-01-01T12:00:00Z",
                       outConfigFile="",
                       outConfigDir = configDir,
                       outFolder =   outFolderRelease,
                       rsuIndics = c("LCZ"),
                       writeNow = TRUE)

  confFile<-paste0(configDir,"/",i,"OSM2011.json")



  geoClimateCall(
    jarFilePath= jarPathRelease,
    configFilePath=confFile,
    wf="osm")

  # 2022
  geoClimateConfigFile(wf = "OSM", locations = i,
                       date="2022-01-01T12:00:00Z",
                       outConfigFile="",
                       outConfigDir = configDir,
                       #outFolder = configDir,
                       outFolder = outFolderRelease,
                       rsuIndics = c("LCZ"),
                       writeNow = TRUE)

  confFile<-paste0(configDir,"/",i,"OSM2022.json")

  geoClimateCall(
    jarFilePath= jarPathRelease,
    configFilePath=confFile,
    wf="osm")
}

city_code_78<-c("78172", "78575","78440", "78255",	"78030", "78220", "78466", "78531", "78562", "78497", "78068", "78478", "78606")

city_code_93<-c(  "93029",	"93066",	"93048")


for (i in city_code_93){
  # 2011
  geoClimateConfigFile(wf = "BDT",
                       date="2011",
                       BDTinFolder="/home/gousseff/Documents/3_data/BDTOPO/2011/93",
                       locations=i,
                       outConfigFile="",
                       outConfigDir = configDir,
                       # outFolder = configDir,
                       outFolder = outFolderRelease,
                       rsuIndics = c("LCZ"),
                       writeNow = TRUE,
                       forceSRID=TRUE)

  confFile<-paste0(configDir, "/", i, "BDT2011.json")

  geoClimateCall(
    jarFilePath= jarPathRelease,
    configFilePath=confFile,
    wf="BDTOPO_V2")


  # 2022
  geoClimateConfigFile(wf = "BDT",
                       date="2022",
                       BDTinFolder="/home/gousseff/Documents/3_data/BDTOPO/2022/93",
                       locations=i,
                       outConfigFile="",
                       outConfigDir = configDir,
                       # outFolder = configDir,
                       outFolder = outFolderRelease,
                       rsuIndics = c("LCZ"),
                       writeNow = TRUE,
                       forceSRID=TRUE)

  confFile<-paste0(configDir, "/", i, "BDT2022.json")

  geoClimateCall(
    jarFilePath= jarPathRelease,
    configFilePath=confFile,
    wf="BDTOPO_V3")
}

for (i in city_code_78){
  # 2011
  geoClimateConfigFile(wf = "BDT",
                       date="2011",
                       BDTinFolder="/home/gousseff/Documents/3_data/BDTOPO/2011/78",
                       locations=i,
                       outConfigFile="",
                       outConfigDir = configDir,
                       # outFolder = configDir,
                       outFolder = outFolderRelease,
                       rsuIndics = c("LCZ"),
                       writeNow = TRUE,
                       forceSRID=TRUE)

  confFile<-paste0(configDir, "/", i, "BDT2011.json")

  geoClimateCall(
    jarFilePath = jarPathRelease,
    configFilePath=confFile,
    wf="BDTOPO_V2")


  # 2022
  geoClimateConfigFile(wf = "BDT",
                       date="2022",
                       BDTinFolder="/home/gousseff/Documents/3_data/BDTOPO/2022/78",
                       locations=i,
                       outConfigFile="",
                       outConfigDir = configDir,
                       outFolder = outFolderRelease,
                       rsuIndics = c("LCZ"),
                       writeNow = TRUE,
                       forceSRID=TRUE)

  confFile<-paste0(configDir, "/", i, "BDT2022.json")

  geoClimateCall(
    jarFilePath= jarPathRelease,
    configFilePath=confFile,
    wf="BDTOPO_V3")
}
