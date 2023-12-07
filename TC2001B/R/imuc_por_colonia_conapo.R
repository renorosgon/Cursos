##########################################################################################

#                    Índice de marginación urbana por colonia 2020                       #

##########################################################################################

# La base de datos del índice de marginación urbana por colonia 2020 se encuentra disponible
# en la página de internet del CONAPO https://www.gob.mx/conapo o bien se puede descargar
# directamente del siguiente vinculo http://www.conapo.gob.mx/work/models/CONAPO/Marginacion/Datos_Abiertos/Colonia/IMC_2020.zip

#Se instalan los paquetes y librerías a utilizar para el cálculo del índice de marginación 

#install.packages("pacman") # Instalación de la paquetería pacman
library("pacman")           # Load pacman package

pacman::p_load(openxlsx, dplyr, p2distance, stratification)

##########################################################################################
################### Indice de marginación urbana por colonia #############################
##########################################################################################

# Se carga la base de datos por colonia
# Versión CONAPO
IMUC_2020 <- openxlsx::read.xlsx("data/IMUC_2020.xlsx", sheet = "IMUC_2020_AGS-MOR") %>% 
  rbind(., read.xlsx("data/IMUC_2020.xlsx", sheet = "IMUC_2020_NAY-ZAC")) 

########################## Método de Distancias DP2 ######################################

#José Bernardo Pena Trapero: `Problemas de la medición del bienestar y conceptos afines (1977)` 

# Para obtener el indicador sintético deben cumplir la condición de no ambigüedad, 
# es decir, un aumento en el valor de cada variable es un incremento en las carencia socieconómicas,
# por lo que se multiplica cada indicador por -1. De esta forma, un aumento en el índice 
# supone una disminución en la marginación. 

# Base de referencia 

# Por otro lado, para permitir la comparación en el tiempo de los indicadores simples se determina el vector 
# base de referencia al valor mínimo —el peor escenario teórico— de la fecha censal 2020.

minRV <- makeReferenceVector(X = -1*IMUC_2020[15:25], 
                             reference_vector_function = min)


ind_2020 <- p2distance(matriz = as.matrix(-1*IMUC_2020[15:25]),
                       reference_vector = minRV,
                       iterations = 50)

## Se anexa el índice a la base de datos por colonia

assign(paste0("IMUC_2020_resultados"), cbind(IMUC_2020[1:25], ind_2020[["p2distance"]])) 


# Identificación de datos atípicos 

## Se identifican los casos extremos en el índice de marginación y se aplica el 
## método de caja propuesto por Hubert y Vandervieren, para establecer los límites 
## con los que se debe trabajar.

assign(paste0("outliers_2020"), boxplot.stats(IMUC_2020_resultados[,26]))

## Se crea un índice ficticio con la posición de los datos que salen de la norma a la 
## primera observación del primer cuantil 

assign(paste0("IMUC_2020_resultados"), IMUC_2020_resultados %>%
         mutate(IM_out = ifelse(get(paste(colnames(IMUC_2020_resultados))[26]) >= outliers_2020$stats[1],
                                get(paste(colnames(IMUC_2020_resultados))[26]),
                                outliers_2020$stats[1])))

# Método de Dalenius & Hodges  

#`strata.cumrootf`: cumulative root frequency method by Dalenius and Hodges (1959) 

# Método iterativo para la obtención del número de clases óptimo a nivel colonia 

#### Nota: El método de estratificación iterativo a nivel colonia 
#### puede llegar a tardar en ejecutarse, o bien, puede bajarse el número de iteraciones, siempre 
#### y cuando no sea menor al número de clases publicada en la nota metodológica del índice de 
#### marginación urbana por colonia 2020. En promedio se tarda 20 minutos, para tomarlo 
#### en consideración.

#### Enlace de la nota metodológica : https://www.gob.mx/cms/uploads/attachment/file/714573/Nota_t_cnica_IMUC_2020.pdf

##start.time <- Sys.time()
##i <- 1
##sd <- matrix(NA, nrow = (1500 - 4), ncol = 3)
##meanh <- matrix(NA, nrow = (1500 - 4), ncol = 6)
##varh <- matrix(NA, nrow=(1500 - 4), ncol = 6)
##for (n in seq(5, 1500, 1)){
##   cum <- strata.cumrootf(x = IMUC_2020_resultados[,27], CV = 0.05 , Ls = 5, alloc = c(0.5, 0, 0.5), nclass = n)
##   sd[i,] <-  c(n, cum$stderr, cum$CV)
##   meanh[i,] <- c(n, cum$meanh)
##   varh[i,] <-  c(n, cum$varh)
##   i <- i + 1
##  }
##colnames(sd) <- c("n", "sderr", "CV")
##colnames(meanh) <- c("nclass", paste0(rep("Strata",5), 1:5))
##colnames(varh) <- c("nclass", paste0(rep("Strata",5), 1:5))
##end.time <- Sys.time()
##time.taken <- round(end.time - start.time, 2)
##time.taken

##min.strata <- sd %>% 
##             as.data.frame() %>% 
##            slice(which.min(.$CV))

strata.DH_2020 <- strata.cumrootf(IMUC_2020_resultados[,27],
                                  CV = 0.05,
                                  Ls = 5,
                                  alloc = c(0.5, 0, 0.5), 
                                  nclass = 20)
strata.DH_2020

#### Los resultados finales se agregan a la base de datos  

assign(paste0("IMUC_2020_resultados"), data.frame(IMUC_2020_resultados %>% 
                                                    select(-IM_out), ## Se elimina el índice ficticio
                                                  strata.DH_2020[["stratumID"]])) 

#### Se cambian los nombres de las columnas 
names(IMUC_2020_resultados) <- c("CVE_COL", "ID_COL", "COLONIA", "CP", "CLASIF", "CVE_ENT", "NOM_ENT", "CVE_MUN", "NOM_MUN", 
                                 "CVE_LOC", "NOM_LOC", "SUN_2018", "NOM_SUN", "POB_TOT", "P6A14NAE", "SBASC", "PSDSS", "OVSDE", "OVSEE", 
                                 "OVSAE", "OVPT", "OVHAC", "OVSREF", "OVSINT", "OVSCEL",
                                 paste0("IM_2020"), paste0("GM_2020"))

#### Se cambian los niveles del método de D&H

for(i in 2020){
  niveles = get(paste0("IMUC_",i,"_resultados")) 
  levels(niveles[,27]) = c("Muy alto", "Alto", "Medio", "Bajo", "Muy bajo")
  assign(paste0("IMUC_",i,"_resultados"), niveles)
  rm(niveles)
}

# Indice normalizado de marginación urbana por colonia

## La normalización se realiza utilizando un cambio de escala conocido como normalización
## mínima-máxima. Con este procedimiento el índice de marginación se escala a valores
## relativos con un rango de entre cero y uno, lo cual permite su comparación numérica y le da
## una propiedad adicional al índice de marginación.

## De antemano, se sabe que cada indicador simple toma valores de cero a 100 y, además, 
## el método DP2 ya proporcionó el orden de entrada de las variables.

#### Desviación estandar de los indicadores simples 

## Es necesario calcular el estimador insesgado de la varianza poblacional de cada 
## indiciador simple, ya que la función `p2distance()` no proporciona el cálculo.  

desvest <- as.matrix(apply(IMUC_2020_resultados[15:25], MARGIN = 2, sd)) %>%
  as.data.frame() %>%
  rename("desvest" ="V1") %>%
  mutate(sd_muestral = .$desvest *(sqrt((dim(IMUC_2020_resultados[15:25])[1] - 1)/dim(IMUC_2020_resultados[15:25])[1]))) %>%
  mutate(desvest.inversa = 1/(.$sd_muestral))

#### Escenarios del valor mínimo y máximo en el índice por el método DP2

## Valor mínimo del índice
vector_minimo <- minRV
minimo <- abs(vector_minimo - minRV)*desvest$desvest.inversa *
  ind_2020[["correction_factors"]][names(IMUC_2020[15:25])] %>%
  t() %>%
  as.data.frame()

## Valor máximo valor del índice
vector_maximo <- rep(0,11) # Cuando los indicadores valen cero 
maximo <- abs(vector_maximo - minRV)*desvest$desvest.inversa *
  ind_2020[["correction_factors"]][names(IMUC_2020[15:25])] %>%
  t() %>%
  as.data.frame()

assign(paste0("IMUC_2020_resultados"), IMUC_2020_resultados %>%  
         mutate(IMN_2020 = (.$IM_2020 - sum(minimo))/(sum(maximo) - sum(minimo))))

# Se guarda la base de datos  
#write.csv(IMUC_2020_resultados, file = paste0("~/IMUC_2020.csv"), sep = ",")