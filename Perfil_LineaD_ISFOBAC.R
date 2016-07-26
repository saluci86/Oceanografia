

#### CLEAN EVERYTHING ####
rm(list = ls(all = TRUE)) #clear all;
graphics.off() # close all;
gc() # Clear memmory (residuals of operations?)

#### Este programa calcula la MLD para el proyecto isfobac ###

setwd("~/Doctorado/Eliana/My_R/Capitulo2/Datos/In_situ/ISFOBAC/")

#Getting a List of Files in a Directory
isfobac <- read.csv("CTD_isfobac2.csv",header=TRUE)
Lance <- isfobac$Nombre_original
Crucero <- isfobac$ID
File_csv <- isfobac$Lance

#Selecciona las lineas del Isfobac a analizar
Lineas <- which(isfobac$Linea==1)
Isfobac <- isfobac[Lineas,]
Yrs  <- which(Isfobac$Año==2012)
Isfobac <- Isfobac[Yrs,]

Yr <- c(2012)

require(sp)
for(j in seq_along(Yr)) {
  #Selecciona las estaciones 
  I24 <- which(Isfobac$Año== Yr[j])
  Datos <- Isfobac[I24,]
  Lance <- Datos$Lance
  Crucero <- Datos$ID
  na.strings = c("", "NA")
  DATASET = NULL
  
  for(k in seq_along(Datos)) {
    path = '~/Doctorado/Eliana/My_R/Capitulo2/Datos/In_situ/ISFOBAC/CSV_CTD'
    File <- file.path(path, Crucero[k], Lance[k])
    
    if(file.exists(File)) { 
      Data <- read.csv(File, header=TRUE, na.strings=na.strings)
      Data$Longitud <- rep(Datos$Longitud[k],nrow(Data))
      Data$Latitud <- rep(Datos$Latitud[k],nrow(Data))
      Data$Estacion <- rep(Datos$Estacion[k],nrow(Data))
#       
#       ### Brunt Vaisala Frequency squared
#       require(gsw)
#       FBV <- gsw_Nsquared(Data$Salinidad,Data$Temp,Data$Presion,latitude=Datos$Latitud[j])
#       
#       ###Guarda histograma
#       setwd('~/Doctorado/Eliana/My_R/Capitulo2/Figuras/Modificacion/Hist_brunt')
#       filename = paste0('N2LineB',Datos$Estacion[k],'.pdf')
#       
#       pdf(filename,width=6,height=4,paper='special')
#       
#       hist(FBV$N2*10^4, xlab = expression(paste(~N^{2},'x ',~10^{-4},' (',~s^{-1},')')),ylab='Frequency',
#            main=paste0('Estacion ',Datos$Nombre_Estacion[k]))
#       
#       dev.off()
#       
      
      temp <- Data
      assign(x = paste0("LAN",k), value = temp)
      #Une los lances de un año
      dataset = rbind(assign(x = paste0("LAN",k), value = temp))
      DATASET = rbind(DATASET, dataset)
    }
    
  }
  
  #Organiza de menor a mayor las profundidades
  
  dataset2 <- DATASET[order(DATASET$Presion), ]
  
  #Calcula la distancia a la estacion 30
  
  I <- which(dataset2$Estacion==1)
  Lon1 <- dataset2$Longitud[I[1]]
  Lat1 <- dataset2$Latitud[I[1]]
  pts <- matrix(c(Lon1,Lat1), nrow=1, ncol=2)
  
  for(k in seq(1,nrow(dataset2))){
    
    pt <- c(dataset2$Longitud[k], dataset2$Latitud[k])
    dataset2$Dist[k] <- spDistsN1(pts,pt)
    
  }
  
  #Guarda la climatologia en un csv
  setwd('~/Doctorado/Eliana/My_R/Capitulo2/Datos/In_situ/ISFOBAC')
  write.csv(dataset2,file=paste0('LineA_CTD_Isfobac',Yr[j],'.csv'))
  
}







