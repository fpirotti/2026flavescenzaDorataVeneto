library(readxl)
library(tidyverse)
library(sf)
library(stars)
library(terra)
library(ggplot2) 
library(doParallel)
library(parallel)
library(foreach)

plotit <- F

## preparo comuni ----
if(!file.exists("data/comuniVeneto.rda")){
  comuni <- sf::read_sf("/archivio/shared/geodati/vettoriali/confini/ITA/ISTAT/limiti01012025/Com01012025_g/Com01012025_g_WGS84.shp")
  comuniVeneto <- comuni |> filter(COD_REG==5) |> sf::st_transform(3035) |> select(COD_REG,PRO_COM_T,  COD_PROV, COMUNE)
  
  provVeneto <- sf::read_sf("/archivio/shared/geodati/vettoriali/confini/ITA/ISTAT/limiti01012025/ProvCM01012025_g/ProvCM01012025_g_WGS84.shp")
  provVeneto <- provVeneto |> sf::st_drop_geometry() |> select(COD_PROV,SIGLA,DEN_UTS)
  
  comuniVeneto <- comuniVeneto |> left_join(  provVeneto, by = c("COD_PROV"))
  
  save(comuniVeneto, file = "data/comuniVeneto.rda") 
} else {
  load("data/comuniVeneto.rda")
}

## LEGGO I DATI ----
dati <-  readxl::read_xlsx("data/DATASET SUBSAMPLE.xlsx")
## se alcuni punti non hanno lat long, possiamo inserire il centroide del comune 
senzaCoordinate <- which(is.na(dati$Lon))
centroidiComuni <- sf::st_centroid(comuniVeneto$geometry)
mat <- charmatch(paste(dati$Provincia, dati$Comune),  
                 paste(comuniVeneto$SIGLA, comuniVeneto$COMUNE) )
## se nomi dei comuni non corrispondono dove non ci sono coordinate,
##  bisogna risolvere in qualche modo - per ora eliminiamo quelli senza coordinate
##  e senza match nome comune
matNA <- which(is.na(mat))
print(dati$Comune[matNA])
###############################
cent <- centroidiComuni[ mat , "geometry" ]  |> sf::st_transform(4326) |> st_coordinates()


dati <- dati |> mutate(LonCentroComune = cent[,1] , LatCentroComune = cent[,2],
                       LonFin = cent[,1] , LatFin = cent[,2] )

dati[!is.na(dati$Lat), c("LonFin","LatFin")]  <- dati[!is.na(dati$Lat), c("Lon","Lat")] 
 
## tolgo gli NA
dati.clean <-  dati[!is.na(dati$Lat),] 
dati.clean.sf <- dati.clean |> sf::st_as_sf(coords = c("Lon", "Lat"), crs=4326 )
  
dati.centroComune.sf <- dati |> sf::st_as_sf(coords = c("LonCentroComune", "LatCentroComune"), crs=4326 )
dati.fin.sf <- dati |> sf::st_as_sf(coords = c("LonFin","LatFin"), crs=4326 )

if(plotit){
  ggplot() + 
    geom_sf(data = comuniVeneto,  color = "#cccccc", fill="#eaeaea", alpha=0.2) +
    geom_sf(data = dati.clean.sf,  aes(color = "Coordinate Originali"), alpha=0.5, size = 1.1 ) +
    geom_sf(data = dati.centroComune.sf, aes(color = "Coordinate Comune"),  alpha=0.5, size = 1.1) +
    scale_color_manual(
      name = NULL,
      values = c("Coordinate Originali" = "red",
                 "Coordinate Comune" = "blue")
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      legend.position = c(0.02, 0.98),    # top-left inside the plot
      legend.justification = c("left", "top"),
      legend.background = element_rect(fill = alpha("white", 0.4),
                                       linewidth = 0, color = "black"),
      panel.grid.major = element_line(size = 0.1),
      text = element_text(size = 12)
    ) +
    labs(title = "Campionamento",
         subtitle = "Veneto Flavescenza Dorata",
         caption = "NB: 100 punti (in rosso) su 200 hanno coordinate, al resto è stata \nassegnata la coordinata del centroide del comune (in blu)")
  
}

# CORINE ----
## #   preparo CORINE ----
corine <- terra::rast("/archivio/shared/geodati/raster/CLMS_CLCplus_RASTER_2023/CLMS_CLCplus_RASTER_2023.tif") 
crops <- terra::rast("/archivio/shared/geodati/raster/CLMS_CropTypes_RASTER_2021/CLMS_CropTypes_RASTER_2021.tif") 

## elaboro solo quelli con le coordinate, non ha senso usare il centroide del comune per sapere la vicinanza con bosco
 
corine.class <- terra::extract(corine, terra::vect(dati.clean.sf), ID=F)
## verifico che effettivamente siano classi non urbane o acqua (vedi )
table(corine.class[,1])
crop.class <- terra::extract(crops, terra::vect(dati.clean.sf), ID=F)
## verifico che effettivamente siano classi agricole - vedi legenda (110 = vitigni)
## https://www.cirgeo.unipd.it/archivio/shared/geodati/raster/CLMS_CropTypes_RASTER_2021/CLMS_CropTypes_Legend.png
table(crop.class[,1])

# Rileva il numero di core disponibili sul tuo PC
n_cores <- detectCores(logical = FALSE) 
 
risultati <- list()
distanze <- c(100,250,500,1000, 2000)
dati.clean.sf.3035 <- dati.clean.sf  |> sf::st_transform(3035)
appFun <- function(i){
  corine.crop <- terra::crop(corine, dati.clean.sf.3035.buff[i,] )
  corine.crop.m <- ifel(corine.crop > 1 & corine.crop < 5, 1, NA)
  corine.crop.m0 <- ifel(corine.crop > 1 & corine.crop < 5, 1, 0)
  corine.forestRatio <- terra::extract(corine.crop.m0, 
                                       dati.clean.sf.3035.buff[i,],
                                       fun="mean", na.rm=F, ID=F )
  corine.forestDist <- terra::extract(corine.crop.m, 
                                      dati.clean.sf.3035[i,],
                                      search_radius = dist-10 , ID=F )
  
  data.frame(forestRatio=corine.forestRatio[,1], 
             forestDist = corine.forestDist[1,2])
}

for(dist in distanze){
  dati.clean.sf.3035.buff <- dati.clean.sf.3035 |> sf::st_buffer(dist)
  message(dist)
   
  # Define your task as a function
  g <- mclapply(1:nrow(dati.clean.sf.3035), function(i) {
    appFun(i)
  }, mc.cores = detectCores() - 3)
  dt <- do.call(rbind, g)
  dt$BufferDist <- dist 
  risultati[[sprintf("D%04dm",dist)]] <- dt
}

dt.final <- do.call(cbind, risultati)
dt.final.long <- do.call(rbind, risultati)

dati.clean.variabili <- cbind(dati.clean, dt.final)
save(dati.clean.variabili, file="output/datiDistanzaBosco.rda")
