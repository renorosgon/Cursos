# Definir una población
poblacion = 1000

# Generar agentes
agentes = data.frame(
  # Creando 100o ids
  id = 1:poblacion,
  precio_compra = runif(poblacion),
  precio_venta = runif(poblacion)
)


# Definir un umbrarl
umbral = 0.5

# Asignación aleatoria
asignacion = runif(poblacion) < umbral
# data.frame[fila, columna]
compradores = agentes[asignacion, ]
vendedores = agentes[!asignacion, ]

# Crear listas vacias
lista_precios_compra = c()
lista_precios_venta = c()
lista_pujas = c()
lista_transacciones = c()

# Iterar 10000 periodos
for (periodo in 1:10000) {
  # Sacar un comprador al azar
  comprador = compradores[
    sample(
      # Elegir entre 1 hasta nfilas de compradores
      x = 1:nrow(compradores), 
      # Solo sacar 1 comprador
      size = 1, 
      # Muestreo sin remplazo
      replace = FALSE
    ) ,
  ]
  
  # El precio de reserva de esta transacción
  precio_compra = comprador$precio_compra
  
  # Declarar la puja
  puja = precio_compra * runif(1)
  
  # Sacar un vendedor al azar
  vendedor = vendedores[
    sample(
      # Elegir entre 1 hasta nfilas de vendedores
      x = 1:nrow(vendedores), 
      # Solo sacar 1 vendedor
      size = 1, 
      # Muestreo sin remplazo
      replace = FALSE
    ) ,
  ]
  
  # El precio de reserva de esta transacción
  precio_venta = vendedor$precio_venta
  
  if(precio_venta <= puja){
    transaccion = TRUE
    # Actualizando mi lista de compradores
    compradores = subset(compradores, id != comprador$id)
    compradores = rbind(compradores, vendedor)
    
    # Actualizar mi lista de vendedoreas
    vendedores = subset(vendedores, id != vendedor$id)
    vendedores = rbind(vendedores, comprador)
  } else {
    transaccion = FALSE
  }
  
  lista_precios_compra = append(lista_precios_compra, precio_compra)
  lista_precios_venta = append(lista_precios_venta, precio_venta)
  lista_pujas = append(lista_pujas, puja)
  lista_transacciones = append(lista_transacciones, transaccion)

}

hist(agentes$precio_compra)

# Como hacer graficas
hist(
  x = lista_pujas,
  breaks = 100,
  col = 'darkred',
  border = 'white',
  main = 'Distribución de las\npujas de los consumidores',
  xlab = 'Pujas',
  ylab = 'Frecuencias'
  )

hist(
  x = lista_pujas[lista_transacciones],
  breaks = 30,
  col = 'darkred',
  border = 'white',
  main = 'Distribución de las\npujas de transacciones realizadas',
  xlab = 'Pujas',
  ylab = 'Frecuencias'
)

resultados = data.frame(
  pujas = lista_pujas,
  transacciones = lista_transacciones,
  precios_compra = lista_precios_compra,
  precios_venta = lista_precios_venta
)

resultados$beneficios = resultados$pujas - resultados$precios_venta

hist(resultados$beneficios)
summary(resultados$beneficios)

transacciones_exitosas = resultados[resultados$transacciones == TRUE, ]

barplot(
  sort(transacciones_exitosas$beneficios),
  col = 'darkblue',
  border = NA,
  main = 'Beneficios de los oferentes'
)

resultados$brecha = resultados$precios_compra - resultados$precios_venta

barplot(
  sort(resultados$brecha, decreasing = TRUE),
  col = 'darkblue',
  border = NA,
  main = 'Brecha entre oferta y demanda'
)

plot(
  x = transacciones_exitosas$precios_compra,
  y = transacciones_exitosas$pujas,
  col = 'darkred',
  pch = 16,
  xlab = 'Precios de reserva',
  ylab = 'Precio de mercado',
  main = 'Excedentes del productor y del consumidor'
)

points(
  x = transacciones_exitosas$precios_venta,
  y = transacciones_exitosas$pujas,
  col = 'darkblue',
  pch = 16
)

legend(
  'topright',
  legend = c('Compradores','Productores'),
  col = c('darkred','darkblue'),
  pch = 16
)

transacciones_exitosas$brecha = 
  transacciones_exitosas$precios_compra - 
  transacciones_exitosas$precios_venta

transacciones_exitosas$brecha[order(-transacciones_exitosas$brecha)]

transacciones_exitosas = transacciones_exitosas[
  order(-transacciones_exitosas$brecha) ,
  ]

plot(
  x = 1:nrow(transacciones_exitosas),
  y = transacciones_exitosas$precios_compra,
  col = 'darkred',
  pch = 16,
  xlab = 'Número de transacción',
  ylab = 'Precio de mercado'
)

points(
  x = 1:nrow(transacciones_exitosas),
  y = transacciones_exitosas$precios_venta,
  col = 'darkblue',
  pch = 16
)

resultados$bins = cut(-resultados$brecha, breaks = 100, labels = FALSE)

resumen_bins = data.frame(
  bins = 1:100,
  precio_compra = NA,
  precio_venta = NA
)

for (bin in 1:100) {
  subset_bin = resultados[resultados$bins == bin, ]
  resumen_bins$precio_compra[bin] = mean(subset_bin$precios_compra, na.rm = T)
  resumen_bins$precio_venta[bin] = mean(subset_bin$precios_venta, na.rm = T)
}

plot(
  x = resumen_bins$bins,
  y = resumen_bins$precio_compra,
  col = 'darkred',
  pch = 16,
  xlab = 'Cantidades',
  ylab = 'Precio',
  main = 'Mercado de Zero Intelligence Traders'
)

points(
  x = resumen_bins$bins,
  y = resumen_bins$precio_venta,
  col = 'darkblue',
  pch = 16
)

lines(
  lowess(resumen_bins$bins, resumen_bins$precio_compra),
  col = 'darkred',
  lwd = 2
)

lines(
  lowess(resumen_bins$bins, resumen_bins$precio_venta),
  col = 'darkblue',
  lwd = 2
)
# Ajustar los ejes
axis(side = 1, at = seq(0, 100, by = 10), labels = seq(0, 100, by = 10))





















