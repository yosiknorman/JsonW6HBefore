#!/usr/bin/Rscript

library(curl)
library(geojsonio)
library(rgdal)
library(stringr)
library(leaflet)
library(magrittr)
library(mapview)

rm(list = ls())
setwd("~/cuda-workspace/6hw/")

url = "http://web.meteo.bmkg.go.id//media/data/bmkg/mfy/Peringatan/Warning.txt"
dt = read.csv(curl(url = url))
dt_table = matrix( nrow = dim(dt)[1], ncol =  5)
for(i in 1:dim(dt)[1]){
  dt_table[i,] = str_to_title(strsplit(as.character(dt[i,]), split = " \t ")[[1]])
  for(j in 1:length(dt_table[i,])){
    if( substr(dt_table[i,j], (nchar(dt_table[i,j])), nchar(dt_table[i,j])) == " " ){
      dt_table[i,j] = substr(dt_table[i,j], 1,(nchar(dt_table[i,j])-1) )
    }else if( substr(dt_table[i,j], 1,1) == " "  )
      dt_table[i,j] = substr(dt_table[i,j], 2,nchar(dt_table[i,j]) )
  }
}

warn = as.data.frame(dt_table)
names(warn) = c("Pulau", "Provinsi", "Kota", "Kecamatan", "Cuaca")  # use grep to catch the Cuaca
warn_matrix = as.matrix(warn)
hujan_sedang = which(warn_matrix == "Hujan Sedang", arr.ind = T)



# Spatial Data  (BIG, 2015 Sub-regency Polygon Data of Indonesia)
load("~/Data_riset/kec2.Rda")
idKC = str_to_title(kec$Kecamatan)
idKB = str_to_title(kec$Kabupaten)

id_province = str_to_title(kec$Provinsi)
uid_province = unique(str_to_title(kec$Provinsi))

kec$Kecamatan = idKC
kec$Kabupaten = idKB
kec$Provinsi = id_province

iPV = list()
shp_PV = list()
warn_PV = list()

for(i in 1:length(uid_province)){
  iPV[[i]] = which(id_province == uid_province[i])
  shp_PV[[i]] = kec[iPV[[i]],]
  warn_PV[[i]] = warn[which(warn == uid_province[i]),]
}



# delta = list()

# for(i in 1:length(shp_PV)){
#   if( any(is.na(warn_PV[[i]])) ){
#     shp_PV[[i]]$Cuaca = "No Significant Weather"
#     for(j in 1:length(shp_PV[[i]]$Provinsi)){
#       if( any(is.na(shp_PV[[i]]$Kecamatan == warn_PV[[i]]$Kecamatan[j])) ){
#         shp_PV[[i]]$Cuaca[j] = warn_PV[[i]]$Cuaca[j]
#       }
#     }
#     
#     # delta[[i]] = 
#   }else if(any(!is.na(warn_PV[[i]]))){
#     for(j in 1:length(shp_PV[[i]]$Provinsi)){
#       if( any(!is.na(warn_PV[[i]]$Kecamatan == shp_PV[[i]]$Kecamatan[j]) ) ){
#         shp_PV[[i]]$Cuaca[j] = warn_PV[[i]]$Cuaca[j]
#       }
#     }
#   }
# }

ID_Cuaca = c("No Significant Weather", "Hujan Sedang", "Hujan Lebat", "Hujan Sangat Lebat")
for(i in 1:length(warn_PV)){
  shp_PV[[i]]$Cuaca = "No Significant Weather"
  shp_PV[[i]]$ID_Cuaca = 1
  for(j in 1:length(warn_PV[[i]]$Kecamatan)){
    shp_PV[[i]]$Cuaca[ which(shp_PV[[i]]$Kecamatan == warn_PV[[i]]$Kecamatan[j]) ] = as.matrix(warn_PV[[i]]$Cuaca[j])
    shp_PV[[i]]$ID_Cuaca[ which(shp_PV[[i]]$Kecamatan == warn_PV[[i]]$Kecamatan[j]) ] = which(ID_Cuaca == as.character(as.matrix(warn_PV[[i]]$Cuaca[j])) )
  }
  # geojson_write(file = paste0("JSON/",uid_province[i]), shp_PV[[i]])
}

bins <- c(0, 1, 2, 3)
pal <- colorBin("YlOrRd", domain = states$density, bins = bins)
pal <- colorBin(c("white", "blue", "pink","darkred"), domain = states$density, bins = bins)

m = leaflet() %>%  addTiles(urlTemplate = "http://mt1.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga")
#   if( any(shp_PV[[14]]$ID_Cuaca == 2)){
#     m = m %>% addPolygons(data = shp_PV[[14]][shp_PV[[14]]$ID_Cuaca == 2,], weight = 0.7, 
#                       label = paste0("Prov. ",
#                                      shp_PV[[14]][shp_PV[[14]]$ID_Cuaca == 2,]$Provinsi," | ",
#                                      shp_PV[[14]][shp_PV[[14]]$ID_Cuaca == 2,]$Kabupaten," | ",
#                                      "Kec. ",
#                                      shp_PV[[14]][shp_PV[[14]]$ID_Cuaca == 2,]$Kecamatan," | ",
#                                      "Warning : ",
#                                      shp_PV[[14]][shp_PV[[14]]$ID_Cuaca == 2,]$Cuaca), fillColor = "orange", fillOpacity = 0.6, color = "black")
#   }
#   if( any(shp_PV[[14]]$ID_Cuaca == 3)){
#     m = m %>% addPolygons(data = shp_PV[[14]][shp_PV[[14]]$ID_Cuaca == 3,], weight = 0.7, 
#                           label = paste0("Prov. ",
#                                          shp_PV[[14]][shp_PV[[14]]$ID_Cuaca == 3,]$Provinsi," | ",
#                                          shp_PV[[14]][shp_PV[[14]]$ID_Cuaca == 3,]$Kabupaten," | ",
#                                          "Kec. ",
#                                          shp_PV[[14]][shp_PV[[14]]$ID_Cuaca == 3,]$Kecamatan," | ",
#                                          "Warning : ",
#                                          shp_PV[[14]][shp_PV[[14]]$ID_Cuaca == 3,]$Cuaca),
#                           fillColor = "red", fillOpacity = 0.9, color = "black")
#   }
#   if( any(shp_PV[[14]]$ID_Cuaca == 1)){
#     m = m %>% addPolygons(data = shp_PV[[14]][shp_PV[[14]]$ID_Cuaca == 1,], weight = 0.7, 
#                           label = paste0("Prov. ",
#                                          shp_PV[[14]][shp_PV[[14]]$ID_Cuaca == 1,]$Provinsi," | ",
#                                          shp_PV[[14]][shp_PV[[14]]$ID_Cuaca == 1,]$Kabupaten," | ",
#                                          "Kec. ",
#                                          shp_PV[[14]][shp_PV[[14]]$ID_Cuaca == 1,]$Kecamatan," | ",
#                                          "Warning : ",
#                                          shp_PV[[14]][shp_PV[[14]]$ID_Cuaca == 1,]$Cuaca), fillColor = "blue", fillOpacity = 0.001, color = "black")
#   }
# 
# if( any(shp_PV[[14]]$ID_Cuaca == 4)){
#   m = m %>% addPolygons(data = shp_PV[[14]][shp_PV[[14]]$ID_Cuaca == 4,], weight = 0.7, 
#                         label = paste0("Prov. ",
#                                        shp_PV[[14]][shp_PV[[14]]$ID_Cuaca == 4,]$Provinsi," | ",
#                                        shp_PV[[14]][shp_PV[[14]]$ID_Cuaca == 4,]$Kabupaten," | ",
#                                        "Kec. ",
#                                        shp_PV[[14]][shp_PV[[14]]$ID_Cuaca == 4,]$Kecamatan," | ",
#                                        "Warning : ",
#                                        shp_PV[[14]][shp_PV[[14]]$ID_Cuaca == 4,]$Cuaca), fillColor = "brown", fillOpacity = 1, color = "black")
# }

source("ch")

m = leaflet() %>%  addTiles(urlTemplate = "http://mt1.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga")
  # for(i in 1:length(shp_PV)){
    m = chloro(ID_Provinsi = 14)
  # }
  



