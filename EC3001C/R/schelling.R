# Agentes -----------------------------------------------------------------
# Numero de agentes
num_agentes = 2000
# Caraceterización por grupo
grupos = c(
  # Celdas vacías
  rep(0, (51*51) - num_agentes), 
  # Grupo 1
  rep(1, num_agentes/2), 
  # Grupo 2
  rep(2, num_agentes/2)
  )

# Preferencas
preferencias_similitud = 0.2

# Tablero -----------------------------------------------------------------
# El tablero de 51 x 51 = 2,601
grid = matrix(
  # Seleccionar un grupo para cada celda
  data = sample(grupos, 2601, replace = FALSE), 
  # Definir el número de columnas
  ncol = 51
  )

# Visualización del tablero
# Una fila y dos columnas 
par(mfrow = c(1, 2))
# Mapa de calor
image(grid, col = c("black","orange2","lightblue3"), axes = FALSE)
# Graficar
plot(
  x = runif(100, 0, 1), 
  ylab = "% de personas felices", 
  xlab = "tiempo", 
  col = "white", 
  ylim = c(0,1)
  )

get_neighbors = function(coords) {
  # Conteos
  n = c()
  # # # # # # #
  # 6 # 7 # 8 #
  # 5 # X # 1 #
  # 4 # 3 # 2 #
  # # # # # # #
  for (i in c(1:8)) {
    # Primer vecino
    if (i == 1) {
      x = coords[1] + 1
      y = coords[2]
    }
    # Segundo vecino
    if (i == 2) {
      x = coords[1] + 1
      y = coords[2] + 1
    }
    # Tercer vecino
    if (i == 3) {
      x = coords[1]
      y = coords[2] + 1
    }
    # Cuarto vecino
    if (i == 4) {
      x = coords[1] - 1
      y = coords[2] + 1
    }
    # Quinto vecino
    if (i == 5) {
      x = coords[1] - 1
      y = coords[2]
    }
    # Sexto vecino
    if (i == 6) {
      x = coords[1] - 1
      y = coords[2] - 1
    }
    # Séptimo vecino
    if (i == 7) {
      x = coords[1]
      y = coords[2] - 1
    }
    # Octavo vecino
    if (i == 8) {
      x = coords[1] + 1
      y = coords[2] - 1
    }
    # Límites del tablero
    if (x < 1) {
      x = 51
    }
    if (x > 51) {
      x = 1
    }
    if (y < 1) {
      y = 51
    }
    if (y > 51) {
      y = 1
    }
    # Concatenar las coordenadas
    n = rbind(n, c(x, y))
  }
  return(n)
}

# Periódos ----------------------------------------------------------------
monitoreo_felicidad = c()

# Para cada periodo
for (t in c(1:1000)) {
  # Conteo de felicidad
  celdas_felicidad = c()
  celdas_infelicidad = c() 
  # Recorrer cada coordenada
  for (j in c(1:51)) {
    for (k in c(1:51)) {
      # Coordenada actual
      actual = c(j,k)
      # Grupo
      valor = grid[j,k] 
      # Celdas vacias
      if (valor > 0) {
        similitud_vecinos = 0
        todos_vecinos = 0
        # Obtener vecinos
        vecinos = get_neighbors(actual)
        # Para cada vecino
        for (i in c(1:nrow(vecinos))){
          # Coordenadas del vecino
          x = vecinos[i,1]
          y = vecinos[i,2]
          # Agregar vecinos
          if (grid[x,y] > 0) {
            todos_vecinos = todos_vecinos + 1
          }
          # Contavilizar simiitud
          if (grid[x,y] == valor) {
            similitud_vecinos = similitud_vecinos + 1
          }
        }
        # Si no es indefinido
        if (is.nan(similitud_vecinos / todos_vecinos) == FALSE) {
          # Lugar infeliz 
          if ((similitud_vecinos / todos_vecinos) < preferencias_similitud) {
            celdas_infelicidad = rbind(celdas_infelicidad, c(actual[1], actual[2]))
          }
          # Lugar feliz
          else {
            celdas_felicidad = rbind(celdas_felicidad, c(actual[1], actual[2]))
          }
        }
        # Si está definido  
        else {
          celdas_felicidad = rbind(celdas_felicidad, c(actual[1], actual[2]))
        }
      }
    }
  }
  # Agregar al monitoreo el ratio de felicidad
  monitoreo_felicidad = append(monitoreo_felicidad, length(celdas_felicidad)/(length(celdas_felicidad) + length(celdas_infelicidad)))
  # Extraer celdas aleatorias
  rand = sample(nrow(celdas_infelicidad))
  for (i in rand) {
    # Celda para mover
    mover = celdas_infelicidad[i,]
    mover_valor = grid[mover[1], mover[2]]
    # Destino
    mover_a = c(sample(1:51,1), sample(1:51,1))
    mover_a_valor = grid[mover_a[1], mover_a[2]]
    # Mientras la celda esté vacía
    while (mover_a_valor > 0){
      mover_a = c(sample(1:51,1),sample(1:51,1))
      mover_a_valor = grid[mover_a[1], mover_a[2]]
    }
    # Se vacía la celda
    grid[mover[1], mover[2]] = 0
    # Nueva ubicación
    grid[mover_a[1], mover_a[2]] = mover_valor
  }
  # Actualizar gráfico
  # Una fila y dos columnas 
  par(mfrow = c(1, 2))
  # Mapa de calor
  image(grid, col = c("black","orange2","lightblue3"), axes = FALSE)
  # Graficar
  plot(
    x = runif(100, 0, 1), 
    ylab = "% de personas felices", 
    xlab = "tiempo", 
    col = "white", 
    ylim = c(0,1)
  )
  # Actualizar
  lines(monitoreo_felicidad, oma = c(0, 0, 2, 0), col="red")
}


