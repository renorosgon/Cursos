# Fijar el directorio de trabajo
setwd("~/Desktop/ITESM/Cursos/TC2001B")

##########################################################################################

#                        Índice de Marginación 2020                                      #

##########################################################################################

# La base de datos del índice de marginación a nivel estatal se encuentra disponible
# en la página de internet del CONAPO https://www.gob.mx/conapo o bien se puede descargar
# directamente del siguiente vinculo http://www.conapo.gob.mx/work/models/CONAPO/Marginacion/Datos_Abiertos/Entidad_Federativa/IME_2020.xls

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
paquetes <- c("readxl", "dplyr", "p2distance", "stratification")
lapply(paquetes, instalar)

##########################################################################################

#                        Índice de Marginación 2020                                      #

##########################################################################################

# La base de datos del índice de marginación a nivel estatal se encuentra disponible
# en la página de internet del CONAPO https://www.gob.mx/conapo o bien se puede descargar
# directamente del siguiente vinculo http://www.conapo.gob.mx/work/models/CONAPO/Marginacion/Datos_Abiertos/Entidad_Federativa/IME_2020.xls

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
paquetes <- c("readxl", "dplyr", "p2distance", "stratification")
lapply(paquetes, instalar)

##########################################################################################
##################### Indice de marginación a nivel estatal ##############################
##########################################################################################

# Se carga la base de datos a nivel estatal
# Se elimina la fila de información a nivel Nacional   

IME_2020 <- readxl::read_xls(path = "data/IME_2020.xls", sheet = "IME_2020") %>% 
  filter(NOM_ENT != "Nacional") 

########################## Método de Distancias DP2 ######################################

#José Bernardo Pena Trapero: `Problemas de la medición del bienestar y conceptos afines (1977)` 

# Para asegurar las propiedades del indicador sintético, un aumento en los indicadores 
# simples implica un aumento en la carencia de los servicios, lo que implicaría una disminución 
# de la calidad de vida, por lo que se multiplica cada indicador por -1, 
# de esta forma, un aumento en la variable supone una mejora en la calidad de vida. 


# Base de referencia 

# Por otro lado, para permitir la comparación en el tiempo de los indicadores simples, y se determina el vector 
#base de referencia al valor mínimo de la fecha censal 2010-2020, es decir, el peor escenario teórico.

minRV <- setNames(
  object = rep(0,9),
  nm = c("ANALF", "SBASC", "OVSDE", "OVSEE", "OVSAE", "OVPT", "VHAC", "PL.5000", "P02SM")
  )


ind_2020 <- p2distance(matriz = as.matrix(IME_2020[4:12]),
                       reference_vector = minRV,
                       iterations = 50)

## Se anexa el índice a la base de datos

assign(paste0("IME_2020_resultados"), cbind(IME_2020[1:12], ind_2020[["p2distance"]])) 


# Método de Dalenius & Hodges  
strata.DH_2020 <- strata.cumrootf(IME_2020_resultados[,13],
                                  CV = 0.05,
                                  Ls = 5,
                                  alloc = c(0.5, 0, 0.5), 
                                  nclass = 18)

##Se agrega a la base de datos los resultados finales 

assign(paste0("IME_2020_resultados"), data.frame(IME_2020_resultados, strata.DH_2020[["stratumID"]])) 

# Se cambian los nombres de las columnas 

names(IME_2020_resultados) <- c(names(IME_2020_resultados)[1:12],
                                paste0("IM_2020"), paste0("GM_2020"))

# Se cambian los levels del método de D&H
levels(IME_2020_resultados[,14]) <- c("Muy bajo", "Bajo", "Medio" , "Alto", "Muy alto")

