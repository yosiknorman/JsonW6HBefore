chloro = function(ID_Provinsi){
  if( any(shp_PV[[ID_Provinsi]]$ID_Cuaca == 2)){
    m = m %>% addPolygons(data = shp_PV[[ID_Provinsi]][shp_PV[[ID_Provinsi]]$ID_Cuaca == 2,], weight = 0.7, 
                          label = paste0("Prov. ",
                                         shp_PV[[ID_Provinsi]][shp_PV[[ID_Provinsi]]$ID_Cuaca == 2,]$Provinsi," | ",
                                         shp_PV[[ID_Provinsi]][shp_PV[[ID_Provinsi]]$ID_Cuaca == 2,]$Kabupaten," | ",
                                         "Kec. ",
                                         shp_PV[[ID_Provinsi]][shp_PV[[ID_Provinsi]]$ID_Cuaca == 2,]$Kecamatan," | ",
                                         "Warning : ",
                                         shp_PV[[ID_Provinsi]][shp_PV[[ID_Provinsi]]$ID_Cuaca == 2,]$Cuaca), fillColor = "orange", fillOpacity = 0.6, color = "black")
  }
  if( any(shp_PV[[ID_Provinsi]]$ID_Cuaca == 3)){
    m = m %>% addPolygons(data = shp_PV[[ID_Provinsi]][shp_PV[[ID_Provinsi]]$ID_Cuaca == 3,], weight = 0.7, 
                          label = paste0("Prov. ",
                                         shp_PV[[ID_Provinsi]][shp_PV[[ID_Provinsi]]$ID_Cuaca == 3,]$Provinsi," | ",
                                         shp_PV[[ID_Provinsi]][shp_PV[[ID_Provinsi]]$ID_Cuaca == 3,]$Kabupaten," | ",
                                         "Kec. ",
                                         shp_PV[[ID_Provinsi]][shp_PV[[ID_Provinsi]]$ID_Cuaca == 3,]$Kecamatan," | ",
                                         "Warning : ",
                                         shp_PV[[ID_Provinsi]][shp_PV[[ID_Provinsi]]$ID_Cuaca == 3,]$Cuaca),
                          fillColor = "red", fillOpacity = 0.9, color = "black")
  }
  if( any(shp_PV[[ID_Provinsi]]$ID_Cuaca == 1)){
    m = m %>% addPolygons(data = shp_PV[[ID_Provinsi]][shp_PV[[ID_Provinsi]]$ID_Cuaca == 1,], weight = 0.7, 
                          label = paste0("Prov. ",
                                         shp_PV[[ID_Provinsi]][shp_PV[[ID_Provinsi]]$ID_Cuaca == 1,]$Provinsi," | ",
                                         shp_PV[[ID_Provinsi]][shp_PV[[ID_Provinsi]]$ID_Cuaca == 1,]$Kabupaten," | ",
                                         "Kec. ",
                                         shp_PV[[ID_Provinsi]][shp_PV[[ID_Provinsi]]$ID_Cuaca == 1,]$Kecamatan," | ",
                                         "Warning : ",
                                         shp_PV[[ID_Provinsi]][shp_PV[[ID_Provinsi]]$ID_Cuaca == 1,]$Cuaca), fillColor = "blue", fillOpacity = 0.001, color = "black")
  }
  
  if( any(shp_PV[[ID_Provinsi]]$ID_Cuaca == 4)){
    m = m %>% addPolygons(data = shp_PV[[ID_Provinsi]][shp_PV[[ID_Provinsi]]$ID_Cuaca == 4,], weight = 0.7, 
                          label = paste0("Prov. ",
                                         shp_PV[[ID_Provinsi]][shp_PV[[ID_Provinsi]]$ID_Cuaca == 4,]$Provinsi," | ",
                                         shp_PV[[ID_Provinsi]][shp_PV[[ID_Provinsi]]$ID_Cuaca == 4,]$Kabupaten," | ",
                                         "Kec. ",
                                         shp_PV[[ID_Provinsi]][shp_PV[[ID_Provinsi]]$ID_Cuaca == 4,]$Kecamatan," | ",
                                         "Warning : ",
                                         shp_PV[[ID_Provinsi]][shp_PV[[ID_Provinsi]]$ID_Cuaca == 4,]$Cuaca), fillColor = "brown", fillOpacity = 1, color = "black")
  }
  return(m)
  
}
