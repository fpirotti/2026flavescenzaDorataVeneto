library(readxl)
library(tidyverse)
library(sf)
library(stars)
library(terra)
library(ggplot2)
library(deldir)   # does the triangulation

comuniVeneto <- sf::read_sf("/archivio/shared/geodati/vettoriali/comuni_regione_veneto.shp")

dati <-  readxl::read_xlsx("data/DATASET SUBSAMPLE.xlsx")
## alcuni non hanno lat long, possiamo inserire il centroide del comune 
centroidiComuni <- sf::st_centroid(comuniVeneto$geometry)
mat <- charmatch(dati$Comune,  comuniVeneto$nomcom)
## qui comincia il balletto dei nomi dei comuni che non corrispondono
matNA <- which(is.na(mat))
print(dati$Comune[matNA])
aggiorno <- unique(dati[matNA, c("Comune","Provincia")])

if(nrow(aggiorno)>0){
  for(i in 1:nrow(aggiorno)){
    message(" no match ", aggiorno[i,] )
    ## qui va inserita un'euristica per ripulire i comuni senza match 
    
  }
}

##ora il match dovrebbe essere del 100%
mat <- charmatch(dati$Comune,  comuniVeneto$nomcom)

## tolgo gli NA
dati.clean <-  dati[!is.na(dati$Lat),] 
dati.clean.sf <- dati.clean |> sf::st_as_sf(coords = c("Lon", "Lat"), crs=4326 )
 
ggplot() + 
  geom_sf(data = comuniVeneto, color = "#eaeaea44", alpha=0.4) +
  geom_sf(data = dati.clean.sf, color = "red", size = 1.1) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(size = 0.1),
    text = element_text(size = 12)
  ) +
  labs(title = "Campione",
       subtitle = "Veneto Flavescenza Dorata",
       caption = "Data: xxxx")
