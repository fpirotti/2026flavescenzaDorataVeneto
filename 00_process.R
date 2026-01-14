library(readxl)
library(tidyverse)
library(sf)
library(stars)
library(terra)
library(ggplot2) 


comuni <- sf::read_sf("/archivio/shared/geodati/vettoriali/confini/ITA/ISTAT/limiti01012025/Com01012025_g/Com01012025_g_WGS84.shp")
comuniVeneto <- comuni |> filter(COD_REG==5) |> sf::st_transform(3035) |> select(COD_REG,PRO_COM_T,
                                                                                          COD_PROV, COMUNE)

provVeneto <- sf::read_sf("/archivio/shared/geodati/vettoriali/confini/ITA/ISTAT/limiti01012025/ProvCM01012025_g/ProvCM01012025_g_WGS84.shp")
provVeneto <- provVeneto |> sf::st_drop_geometry() |> select(COD_PROV,SIGLA,DEN_UTS)

comuniVeneto <- comuniVeneto |> left_join(  provVeneto, by = c("COD_PROV"))

# corine <- terra::rast("/archivio/shared/geodati/raster/CLMS_CLCplus_RASTER_2023/CLMS_CLCplus_RASTER_2023.tif")
corine <- terra::rast("/archivio/shared/geodati/raster/CLMS_CLCplus_RASTER_2023/CLMS_CLCplus_RASTER_2023.tif")
# agric <- terra::rast("/archivio/shared/geodati/raster/CLMS_CropTypes_RASTER_2021/CLMS_CropTypes_RASTER_2021.vrt")
corineVeneto <- terra::crop(corine, terra::ext(comuniVeneto))
 
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
dati <- dati |> mutate(LonCentroComune = cent[,1] , LatCentroComune = cent[,2])


 
## tolgo gli NA
dati.clean <-  dati[!is.na(dati$Lat),] 
dati.clean.sf <- dati.clean |> sf::st_as_sf(coords = c("Lon", "Lat"), crs=4326 )
  
dati.centroComune.sf <- dati |> sf::st_as_sf(coords = c("LonCentroComune", "LatCentroComune"), crs=4326 )

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


