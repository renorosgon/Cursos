##########################################################################################

#                        Índice de Marginación 2020                                      #

##########################################################################################

# La base de datos del índice de marginación urbana por colonia 2020 se encuentra disponible
# en la página de internet del CONAPO https://www.gob.mx/conapo o bien se puede descargar
# directamente del siguiente vinculo http://www.conapo.gob.mx/work/models/CONAPO/Marginacion/Datos_Abiertos/Colonia/IMC_2020.zip

#Se instalan los paquetes y librerías a utilizar para el cálculo del índice de marginación.

instalar <- function(paquetes){
  for (p in paquetes) {
    if (p == "p2distance" && !suppressMessages(require(p,
                                                       character.only = TRUE,
                                                       quietly = TRUE,
                                                       warn.conflicts = FALSE))){
      install.packages("https://cran.r-project.org/src/contrib/Archive/p2distance/p2distance_1.0.1.tar.gz")
    }
    else if (!suppressMessages(require(p, 
                                       character.only = TRUE,
                                       quietly = TRUE,
                                       warn.conflicts = FALSE))){
      install.packages(paquetes, dependencies = TRUE, quiet = TRUE, verbose = FALSE)
    }
    library(paquetes,
            character.only = TRUE,
            quietly = TRUE,
            warn.conflicts = FALSE)
  }
}
paquetes <- c("openxlsx", "dplyr", "p2distance", "stratification")
lapply(paquetes, instalar)

##########################################################################################
################### Indice de marginación urbana por colonia #############################
##########################################################################################

# Se carga la base de datos a nivel datos por colonia

IMUC_2020 <-  readxl::read_excel("data/IMUC_2020.xlsx", sheet = "IMUC_2020_AGS-MOR") %>% 
  rbind(., readxl::read_excel("data/IMUC_2020.xlsx", sheet = "IMUC_2020_NAY-ZAC")) 

########################## Método de Distancias DP2 ######################################

#José Bernardo Pena Trapero: `Problemas de la medición del bienestar y conceptos afines (1977)` 

# Para obtener el indicador sintético deben cumplir la condición de no ambigüedad, 
# es decir, un aumento en el valor de cada variable un aumento en la carencia de los servicios,
#lo que implicaría una disminución de la calidad de vida, por lo que se multiplica cada indicador 
#por -1, de esta forma, un aumento en la variable supone una mejora en la calidad de vida. 

# Base de referencia 

#Por otro lado, para permitir la comparación en el tiempo de los indicadores simples,  y se determina el vector 
#base de referencia al valor mínimo de la fecha censal 2020, es decir, el peor escenario teórico.

### Versión CONAPO
# minRV <- makeReferenceVector(X = -1 * IMUC_2020[15:25], reference_vector_function = min)

# Corrigiendo por el principio de no ambiguedad (entre más lejos del cero peor)
minRV <- rep(0, 11) 

names(minRV) = names(as.matrix(IMUC_2020[15:25]))

ind_2020 <- p2distance(
  #matriz = as.matrix(-1 * IMUC_2020[15:25]), # Versión conapo
  matriz = as.matrix(IMUC_2020[15:25]), # Versión correcta
  reference_vector = minRV,
  iterations = 50
  )

## Se anexa el índice a la base de datos a por colonia

assign(paste0("IMUC_2020_resultados"), cbind(IMUC_2020[1:25], ind_2020[["p2distance"]])) 


# Identificación de datos atípicos 

## Se identifican los casos extremos en el índice de marginación y se aplica el 
## método de caja propuesto por Hubert y Vandervieren, concluyendo que los límites 
## con los que se debe trabajar 


assign(paste0("outliers_2020"), boxplot.stats(IMUC_2020_resultados[,26]))

## Se crea un índice ficticio donde se posición los datos que salen de la norma a la 
## primera observación del primer cuantil 

assign(paste0("IMUC_2020_resultados"), IMUC_2020_resultados %>%
         mutate(IM_out = ifelse(get(paste(colnames(IMUC_2020_resultados))[26]) >= outliers_2020$stats[1],
                                get(paste(colnames(IMUC_2020_resultados))[26]),
                                outliers_2020$stats[1])))



# Método de Dalenius & Hodges  

#`strata.cumrootf`: cumulative root frequency method by Dalenius and Hodges (1959) 

# Método iterativo para la obtención de número de clases óptimo a nivel por colonia 

#### Nota al público: El método de estratificación iterativo a nivel por colonia 
#### puede llegar a tarda en ejecutarse o bien puede bajar el número de iteraciones, siempre 
#### y cuando no sea menor al número de clases publicada en la nota metodológica del índice de 
#### marginación urbana por colonia 2020. En promedio se tarda 20 minutos, para tomarlo 
#### en consideración.


#### Enlace de la nota metodológica : https://www.gob.mx/cms/uploads/attachment/file/714573/Nota_t_cnica_IMUC_2020.pdf

#iteraciones <- 1500
#start.time <- Sys.time()
#i <- 1
#sd <- matrix(NA, nrow = (iteraciones), ncol = 3)
#meanh <- matrix(NA, nrow = (iteraciones), ncol = 6)
#varh <- matrix(NA, nrow = (iteraciones), ncol = 6)
#for (n in seq(5, iteraciones, 1)){
#     cum <- strata.cumrootf(x = IMUC_2020_resultados[,27], CV = 0.05 , Ls = 5, alloc = c(0.5, 0, 0.5), nclass = n)
#     sd[i,] <-  c(n, cum$stderr, cum$CV)
#     meanh[i,] <- c(n, cum$meanh)
#     varh[i,] <-  c(n, cum$varh)
#     i <- i + 1
#  }
#colnames(sd) <- c("n", "sderr", "CV")
#colnames(meanh) <- c("nclass", paste0(rep("Strata", 5), 1:5))
#colnames(varh) <- c("nclass", paste0(rep("Strata", 5), 1:5))
#end.time <- Sys.time()
#time.taken <- round(end.time - start.time, 2)
#time.taken

#min.strata <- sd %>% 
#               as.data.frame() %>% 
#                slice(which.min(.$CV))

strata.DH_2020 <- strata.cumrootf(IMUC_2020_resultados[,27],
                                  CV = 0.05,
                                  Ls = 5,
                                  alloc = c(0.5, 0, 0.5), 
                                  nclass = 20)
strata.DH_2020

#### Se agrega a la base de datos los resultados finales 

assign(paste0("IMUC_2020_resultados"), data.frame(IMUC_2020_resultados %>% 
                                                    select(-IM_out), ## Se elimina el índice ficticio
                                                  strata.DH_2020[["stratumID"]])) 

#### Se cambian los nombres de las columnas 
names(IMUC_2020_resultados) <- c(names(IMUC_2020_resultados)[1:25],
                                 paste0("IM_2020"), paste0("GM_2020"))

### Aquí corroboro que el índice siga la misma dirección
GGally::ggcorr(select(IMUC_2020_resultados, P6A14NAE:IM_2020), label = T)

#### Se cambian los levels del método de D&H
# Esta es la versión antigua de CONAPO
# levels(IMUC_2020_resultados[,27]) <- c("Muy alto", "Alto", "Medio", "Bajo", "Muy bajo")
# Dado que ya correjimos el principio de no ambiguedad ahora hay que invertir los niveles
levels(IMUC_2020_resultados[,27]) <- c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto")

# Indice de marginación normalizado a nivel urbana por colonia

## La normalización se realiza utilizando un cambio de escala conocido como normalización
## mínima-máxima. Con este procedimiento el índice de marginación se escala a valores
## relativos con un rango de entre cero y uno, lo cual permite su comparación numérica y le da
## una propiedad adicional al índice de marginación.

## De antemano, se sabe que cada indicador simple toma valores de cero a 100 y, además, 
## el método DP2 ya proporcionó el orden de entrada de las variables.

#### Desviación estandar de los indicadores simples 

## Es necesario calcular el estimador insesgado de la varianza poblacional de cada 
## indiciador simple, ya el la función `p2distance()` no proporciona el cálculo.  

desvest <- as.matrix(apply(IMUC_2020_resultados[15:25], MARGIN = 2, sd)) %>%
  as.data.frame() %>%
  rename("desvest" ="V1") %>%
  mutate(sd_muestral = .$desvest * (sqrt((dim(IMUC_2020_resultados[15:25])[1] - 1)/dim(IMUC_2020_resultados[15:25])[1]))) %>%
  mutate(desvest.inversa = 1/(.$sd_muestral))



#### Escenarios del mínimo y máximo valor en el índice por el método DP2

## Mínimo valor del índice
vector_minimo <- minRV
minimo <- abs(vector_minimo - minRV) * desvest$desvest.inversa *
  ind_2020[["correction_factors"]][names(IMUC_2020[15:25])] %>%
  t() %>%
  as.data.frame()

## Máximo valor del DP2   
vector_maximo <- rep(0, length(minRV)) # Cuando los indicadores valen cero 
maximo <- abs(vector_maximo - minRV) * desvest$desvest.inversa *
  ind_2020[["correction_factors"]][names(IMUC_2020[15:25])] %>%
  t() %>%
  as.data.frame()

assign(paste0("IMUC_2020_resultados"), IMUC_2020_resultados %>%  
         mutate(IMN_2020 = (.$IM_2020 - sum(minimo))/(sum(maximo) - sum(minimo))))

# Se guarda la base de datos  
#write.csv(IMUC_2020_resultados, file = paste0("~/IMUC_2020.csv"), sep = ",")




