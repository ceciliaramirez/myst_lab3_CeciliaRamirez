# -- Borrar todos los elementos del environment
rm(list=ls())
mdir <- getwd()

# -- Establecer el sistema de medicion de la computadora
Sys.setlocale(category = "LC_ALL", locale = "")

# -- Huso horario
Sys.setenv(tz="America/Monterrey", TZ="America/Monterrey")
options(tz="America/Monterrey", TZ="America/Monterrey")

# -- Cargar y/o instalar en automatico paquetes a utilizar -- #

pkg <- c("base","downloader","dplyr","fBasics","forecast","grid",
         "gridExtra","httr","jsonlite","lmtest","lubridate","moments",
         "matrixStats", "PerformanceAnalytics","plyr","quantmod",
         "reshape2","RCurl","RMySQL", "stats","scales","tseries",
         "TTR","TSA","XML","xts","zoo")

inst <- pkg %in% installed.packages()
if(length(pkg[!inst]) > 0) install.packages(pkg[!inst])
instpackages <- lapply(pkg, library, character.only=TRUE)

# -- Cargar archivos desde GitHub -- #

RawGitHub <- "https://raw.githubusercontent.com/IFFranciscoME/"
ROandaAPI <- paste(RawGitHub,"ROandaAPI/master/ROandaAPI.R",sep="")
downloader::source_url(ROandaAPI,prompt=FALSE,quiet=TRUE)

# -- Parametros para usar API-OANDA

# Tipo de cuenta practice/live
OA_At <- "practice"
# ID de cuenta
OA_Ai <- 1742531
# Token para llamadas a API
OA_Ak <- "ada4a61b0d5bc0e5939365e01450b614-4121f84f01ad78942c46fc3ac777baa6" 
# Hora a la que se considera "Fin del dia"
OA_Da <- 17
# Uso horario
OA_Ta <- "America/Mexico_City"
# Instrumento
OA_In <- "EUR_USD"
# Granularidad o periodicidad de los precios H4 = Cada 4 horas
OA_Pr <- "H4"
# Multiplicador de precios para convertir a PIPS
MultPip_MT1 <- 10000

Precios_Oanda <- HisPrices(AccountType = OA_At, Granularity = OA_Pr,
                           DayAlign = OA_Da, TimeAlign = OA_Ta, Token = OA_Ak,
                           Instrument = OA_In, 
                           Start = NULL, End = NULL, Count = 900)

###############

Fadx <- ADX(Precios_Oanda[,c("High","Low","Close")])


###############

Historico <- data.frame("Date"= Precios_Oanda[,1],
                        "PrecioC"= Precios_Oanda[,5],
                        "PrecioA"= Precios_Oanda[,2],
                        "ADX"= Fadx,
                        "R_Precio" = 0,
                        "R_Activo" = 0,
                        "R_Cuenta" = 0,
                        "Capital" = 0,"Flotante" = 0, "Balance" = 0, "Unidades" = 0,
                        "Unidades_a" = 0,
                        "Ganancia"= 0, "Ganancia_ac"= 0, "Mensaje"= NA)

#####
PIP <- 10000
Capital_inicial <- 1000000
#####

#Calcular los titulos de posicion inicial
Historico$Unidades[28] <- PIP

#Calcular los titulos acumulados
Historico$Unidades_a[28] <- Historico$Unidades[28]

#Calcular Ganancia
Historico$Ganancia[28]<- (Historico$PrecioC[28]-Historico$PrecioA[28])*PIP

#Ganancia Acumulada
Historico$Ganancia_ac[28]<- Historico$Ganancia[28]
Historico$Flotante[28]<- Historico$Unidades_a[28]*Historico$PrecioC[28]

#Todo remanente se deja registrado en la cuenta de efectivo
Historico$Capital[28] <- Capital_inicial-Historico$Flotante[28]

#Calcular Balance
Historico$Balance[28] <- Historico$Flotante[28]+ Historico$Capital[28]

#Calcular R_Precio
Historico$R_Precio <- round(c(0, diff(log(Historico$PrecioC))),4)
##########################

for(i in 29:length(Historico$Date)){
  if(Historico$ADX.ADX[i]>25){
    
    if(Historico$ADX.DIp[i] > Historico$ADX.DIn[i]){
    
    Historico$Unidades[i] <- PIP
    
    Historico$Unidades_a[i] <- Historico$Unidades_a[i-1]+Historico$Unidades[i]
    
    Historico$Flotante[i]<- Historico$PrecioC[i]*Historico$Unidades_a[i]
    
    Historico$Ganancia[i]<-(Historico$PrecioC[i]-Historico$PrecioA[i])*Historico$Unidades[i]
    Historico$Ganancia_ac[i]<- Historico$Ganancia[i]+Historico$Ganancia_ac[i-1]
    Historico$Mensaje[i] <- "Compra"
    
  }
    if(Historico$ADX.DIp[i] < Historico$ADX.DIn[i]){
    
      Historico$Unidades[i]   <- PIP
    
    Historico$Unidades_a[i] <- Historico$Unidades_a[i-1]+Historico$Unidades[i]
    
    Historico$Flotante[i]<- Historico$PrecioC[i]*Historico$Unidades_a[i]
    
    Historico$Ganancia[i]<-(Historico$PrecioA[i]-Historico$PrecioC[i])*Historico$Unidades[i]
    Historico$Ganancia_ac[i]<- Historico$Ganancia[i]+Historico$Ganancia_ac[i-1]
    Historico$Mensaje[i] <- "Venta"
}
  }
  else { # Sin senal
    Historico$Mensaje[i] <- "No hay señal"
    Historico$Unidades [i] <- 0
    Historico$Unidades_a[i] <- Historico$Unidades_a[i-1]+Historico$Unidades[i]
    Historico$Flotante[i] <- Historico$PrecioC[i]*Historico$Unidades_a[i]
    Historico$Ganancia[i]<-(Historico$PrecioA[i]-Historico$PrecioC[i])*Historico$Unidades[i]
    Historico$Ganancia_ac[i]<- Historico$Ganancia[i]+Historico$Ganancia_ac[i-1]
  }
}

###########################
plot_ly(x = Historico[,1], type="candlestick", open = Historico$PrecioA, close=Historico$PrecioC, high = Precios_Oanda$High, low=Precios_Oanda$Low) %>%
  layout(tittle = "Basic Candlestick Chart")

plot_ly(x = Historico[,1], y = Historico[,4], type = 'scatter', mode = 'lines', name = 'ADX',
        
        line = list(color = 'blue'), hoverinfo = "text", text = ~paste('ADX', Historico[,4]))  %>%
  
  layout(title = "ADX vs Precio de cierre",
         
         xaxis = list(title = "Fechas", showgrid = T),
         
         yaxis = list(title = "Precios"), 
         
         legend = list(orientation = 'h', y = -0.25, x = 0.5))     
