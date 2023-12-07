library(tidyverse)
library(patchwork)
library(ggdist)
library(randomForest)
set.seed(123)

df = tibble(x = sort(runif(n = 100, min=-5, max=5)),
            y = x^2/5  + runif(n = 100, min = -1, max= 1)) 



df = df %>% 
  mutate(y_lineal = predict(lm(y~x)),
         y_cuadratica = predict(lm(y~poly(x,2))))
rf = randomForest(x=as.matrix(df$x), y=df$y, ntree=1000)
df$rf = predict(rf, newdata=as.matrix(df$x))

p = ggplot(data = df, aes(x=x, y=y)) + 
  geom_point(alpha = 0.65) +
  geom_line(
    aes(y = rf),
    col = '#B6171A'
  ) +
  theme_ggdist() +
  theme( rect = element_rect(fill = "transparent"),
         panel.background = element_rect(fill = "transparent"), 
         plot.background = element_rect(fill = "transparent", color = NA))
p

ggsave('ideal.png', units = 'cm', 
       height = 10, width = 10, 
       dpi = 150,bg = "transparent")

set.seed(1)
new_x = sort(runif(n = 100, min=-5, max=5))
new_y =  y = new_x^2/5  + runif(n = 100, min = -1, max= 1)
df$new_x = new_x
df$new_y = new_y
df$rf_new = predict(rf, newdata=as.matrix(df$new_x))



p = ggplot(data = df, aes(x=x)) + 
  geom_point(aes(y = y, col = 'Conocidos'),alpha = 0.65) +
  geom_point(aes(y = new_y, col = 'Nuevos'),alpha = 0.65) +
  scale_color_manual(values = c('#B6171A','#868382')) +
  geom_line(aes(y = rf), col = '#B6171A') +
  geom_line(aes(y = rf_new), col = '#868382') +
  theme_ggdist() +
  theme( rect = element_rect(fill = "transparent"),
         panel.background = element_rect(fill = "transparent"), 
         plot.background = element_rect(fill = "transparent", color = NA)) +
  labs(col = 'Datos') +
  theme(legend.position = 'none',
        rect = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA)) +
  ggtitle('Predicciones de un Modelo')
p


res = df %>% 
  ggplot() +
  geom_density(aes(y - rf, fill = 'Conocidos'), alpha = 6.5, col= NA)  +
  geom_density(aes(new_y - rf_new, fill = 'Nuevos'),  alpha = 0.65, col= NA) +
  scale_fill_manual(values = c('#B6171A','#868382')) +
  labs(fill = 'Datos', x = 'Residuales', y = 'Densidad') +
  theme_ggdist() +
  theme(legend.position = 'none',
        rect = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA)) +
  ggtitle('Residuales')

res
plot_grid(p,res, nrow = 1)
ggsave('residuale.png', units = 'cm', 
       height = 15, width = 15, 
       dpi = 150,bg = "transparent")



gp = runif(100)
df$split = if_else(gp>0.75,'Prueba','Entrenamiento')
df$new_cuadratica = predict(lm(y~poly(x,2), data = df), newdata = tibble(df$new_x))

p = ggplot(data = df, aes(x=x)) + 
  geom_point(aes(y = y, col = split),alpha = 0.65) +
  geom_point(aes(y = new_y, col = 'Nuevos'),alpha = 0.65) +
  geom_line(aes(y = y_cuadratica, col = split)) +
  geom_line(aes(y = new_cuadratica, col = 'Nuevos')) +
  scale_color_manual(values = c('#E01B5D','#868382','#005CB9')) +
  theme_ggdist() +
  theme( rect = element_rect(fill = "transparent"),
         panel.background = element_rect(fill = "transparent"), 
         plot.background = element_rect(fill = "transparent", color = NA)) +
  labs(col = 'Datos') +
  theme(legend.position = 'none',
        rect = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA)) +
  ggtitle('Predicciones de un Modelo')


res = df %>% 
  ggplot() +
  geom_density(aes(y - y_cuadratica, fill = split), alpha = 0.65, col= NA)  +
  geom_density(aes(new_y - new_cuadratica, fill = 'Nuevos'),  alpha = 0.65, col= NA) +
  scale_fill_manual(values = c('#E01B5D','#868382','#005CB9')) +
  labs(fill = 'Datos', x = 'Residuales', y = 'Densidad') +
  theme_ggdist() +
  theme(legend.position = 'none',
        rect = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA)) +
  ggtitle('Residuales')

res





#Fija una semilla aleatoria en 123
set.seed(123)

# Crea una distribución normal de 5000 observaciones, con media = 68 y desviación estandar 5,
# y asignala a un objeto llamado x
x = rnorm(n = 5000, mean = 68, sd = 10)

d = rbinom(n = 5000, size = 1, prob = 0.5)




# Crea un histograma de pesos utilizando la funcion qplot
hist_x = qplot(x) +
  theme_ggdist() +
  ggtitle('Variable Predictiva') +
  theme(legend.position = 'none',
        rect = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA)) 

hist_x
ggsave('reg_simple.png', units = 'cm', 
       height = 15, width = 15, 
       dpi = 150,bg = "transparent")

# Crea una distribución normal de 5000 obserbaciones, con media = 0 y desviación 
# estandar 5, y asignala a un objeto llamado e
e = rnorm(n = 5000, mean = 0, sd = 5)


# Define un objeto llamado y compuesto por la formula 100 + 1.25*x + e
y = 100 + 1.25*x + e + d*0.5*1.25*x

hist_y = qplot(y)+
  theme_ggdist() +
  ggtitle('Variable Objetivo')+
  theme(legend.position = 'none',
        rect = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA)) 
hist_y

b_hat = cov(x,y)/var(x)
a_hat = mean(y) - b_hat * mean(x)

# Crea un tibble con la variable x y y
df = tibble(
  x = x, 
  y = y
)

# Carga la librerīa ggside
# En caso de que aun no la instales utiliza el comando install.packages('ggside')
library(ggside)

ggplot(
  data = df,
  aes(
    x = x,
    y = y
  )
) +
  geom_point(alpha = 0.15, col = 'gray15')  +
  geom_xsidehistogram() +
  geom_ysidehistogram() +
  geom_abline(
    intercept = a_hat,
    slope = b_hat*.8,
    col = '#E01B5D'
  ) +
  geom_abline(
    intercept = a_hat,
    slope = b_hat*1.2,
    col = '#005CB9'
  )+ 
  theme_ggdist() +
  theme(legend.position = 'none',
        rect = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA)) 




tibble(
  y = rep(125,9),
  x = rep(100,9),
  beta = seq(1,1.5,0.0625),
  y_hat = x * beta
  ) %>% 
  ggplot(
    aes(
      x= beta,
      y = (y - y_hat)^2
    )
  ) +
  geom_line() +
  geom_point() +
  theme_ggdist() +
  labs(y = 'Error Cuadratico', x='Beta') +
  theme(rect = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA)) 

ggsave('pendientes.png', units = 'cm', 
       height = 15, width = 15, 
       dpi = 150,bg = "transparent")




set.seed(28015)
beta_vec <- rnorm(100, 0, 0.2)
p <- length(beta_vec)
beta <- tibble(term = str_c('V', 1:p), valor = beta_vec)
head(beta)

sim_datos <- function(n, beta){
  p <- nrow(beta)
  mat_x <- matrix(rnorm(n * p, 0, 1), n, p) + rnorm(n, 0, 5) 
  colnames(mat_x) <- beta %>% pull(term)
  beta_vec <- beta  %>%  pull(valor)
  f_x <- mat_x %*% beta_vec 
  y <- as.numeric(f_x) + rnorm(n, 0, 1)
  datos <- as_tibble(mat_x) 
  datos %>% mutate(y = y)
}
datos <- sim_datos(n = 4000, beta = beta)


library(tidymodels)
set.seed(994)
n_entrena <- nrow(datos) * 0.03
separacion <- initial_split(datos, 0.03)
dat_ent <- training(separacion)
modelo <-  linear_reg() %>% set_engine("lm")
receta <- recipe(y ~ ., dat_ent)
flujo <- workflow() %>% 
  add_model(modelo) %>% 
  add_recipe(receta)
flujo_ajustado <- fit(flujo, dat_ent)
mod_1  <- flujo_ajustado %>% extract_fit_engine()


library(patchwork)
dat_pr <- testing(separacion)
preds_entrena <- predict(flujo_ajustado, dat_ent) %>% 
  bind_cols(dat_ent %>% select(y))
preds_prueba <- predict(flujo_ajustado, dat_pr) %>% 
  bind_cols(dat_pr %>% select(y))
g_1 <- ggplot(preds_entrena, aes(x = .pred, y = y)) +
  geom_abline(colour = "red") +
  geom_point(alpha = 0.15, col = 'gray15') + 
  xlab("Predicción") + ylab("y") +
  labs(subtitle = "Muestra de entrenamiento") +
  theme_ggdist() +
  theme(rect = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA)) 
g_2 <- ggplot(preds_prueba, aes(x = .pred, y = y)) + 
  geom_abline(colour = "red") +
  geom_point(alpha = 0.15, col = 'gray15') + 
  xlab("Predicción") + ylab("y") +
  labs(subtitle = "Muestra de prueba") +
  theme_ggdist() +
  theme(rect = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA)) 
g_2 

ggsave('varianza.png', units = 'cm', 
       height = 15, width = 15, 
       dpi = 150,bg = "transparent")


df = tibble(
  beta_1 = rnorm(500,mean = 2, sd = 2),
  beta_2 = rnorm(500,mean = beta_1, sd = 2),
  y = beta_1 + beta_2*rnorm(500,mean = 2, sd = 2) + rnorm(500,mean = 0, sd = 1),
  y_hat = beta_1 + beta_2*rnorm(500,mean = 2, sd = 2)
)


ggplot(df) +
  stat_contour(
    aes( x = beta_1,
         y = beta_2,
         z = y_hat-y)
  )

as.data.frame(volcano) %>% #convert the matrix to data frame
  rownames_to_column() %>% #get row coordinates
  gather(key, value, -rowname) %>% #convert to long format
  mutate(key = as.numeric(gsub("V", "", key)), #convert the column names to numbers
         rowname = as.numeric(rowname)) %>%
  ggplot() +
  geom_contour(aes(x = rowname, y = key, z = value))




source("http://freakonometrics.free.fr/probit.R") 
reg=glm(Y~X1+X2,family=binomial)

df = tibble(
  prediccion = predict(reg),
  residuales = residuals(reg)
  )

df %>% 
  ggplot(
    aes(
      y = prediccion,
      x = residuales
    )
  ) +
  geom_point(alpha = 0.15, col = 'gray15') + 
  labs(y = "Predicción", x = 'Residuales') +
  geom_vline(xintercept = 0, linetype = 2, col = 'gray15') +
  theme_ggdist() +
  theme(rect = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA))  +
  geom_xsidehistogram()

qplot(residuals(reg))  +
  labs(x = 'Residuales') +
  theme_ggdist() +
  theme(rect = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA)) 

ggsave('res_pois.png', units = 'cm', 
       height = 15, width = 15, 
       dpi = 150,bg = "transparent")


        
        
require(ggplot2)
require(sandwich)
require(msm)        
p <- read.csv("https://stats.idre.ucla.edu/stat/data/poisson_sim.csv")
p <- within(p, {
  prog <- factor(prog, levels=1:3, labels=c("General", "Academic", 
                                            "Vocational"))
  id <- factor(id)
})
summary(p)

with(p, tapply(num_awards, prog, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))


summary(m1 <- glm(num_awards ~ prog + math, family="poisson", data=p))
m2 <- update(m1, . ~ . - prog)
qplot(residuals(m2)) +
  labs(x = 'Residuales') +
  theme_ggdist() +
  theme(rect = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA)) 






set.seed(0)
dat <- data.frame(x=(x=runif(10000, 0, 50)),
                  y=rnorm(10000, 10*x, 100))
## breaks: where you want to compute densities
breaks <- seq(0, max(dat$x), len=5)
dat$section <- cut(dat$x, breaks)
## Get the residuals
dat$res <- residuals(lm(y ~ x, data=dat))
## Compute densities for each section, flip the axes, add means 
## of sections.  Note: densities need to be scaled in relation 
## to section size (2000 here)
dens <- do.call(rbind, lapply(split(dat, dat$section), function(x) {
  d <- density(x$res, n=5000)
  res <- data.frame(x=max(x$x)- d$y*1000, y=d$x+mean(x$y))
  res <- res[order(res$y), ]
  ## Get some data for normal lines as well
  xs <- seq(min(x$res), max(x$res), len=5000)
  res <- rbind(res, data.frame(y=xs + mean(x$y),
                               x=max(x$x) - 1000*dnorm(xs, 0, sd(x$res))))
  res$type <- rep(c("empirical", "normal"), each=5000)
  res
}))
dens$section <- rep(levels(dat$section), each=10000)
ols_assume <- ggplot(dat, aes(x, y)) +
  geom_point(size = 0.1, alpha = .25) +
  geom_smooth(method="lm", fill=NA, lwd=2, col = '#005CB9') +
  geom_path(data=dens[dens$type=="normal",], 
            aes(x, y, group=section), 
            color='#E01B5D', lwd=1.1)  +
  geom_vline(xintercept=breaks, lty=2, col = '#868382') +
  theme_ggdist()+
  theme(rect = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA)) 
# Now make Poisson regression picture
set.seed(0)
dat <- data.frame(x=(x=runif(1000, 0, 20)),
                  y=rpois(1000, exp(.1*x)))
## breaks: where you want to compute densities
breaks <- seq(2, max(dat$x), len=5)
dat$section <- cut(dat$x, breaks)
## Get the residuals
dat$res <- dat$y - .1*dat$x
## Compute densities for each section, flip the axes, add means
## of sections.  Note: densities need to be scaled in relation 
## to section size
dens <- do.call(rbind, lapply(split(dat, dat$section), function(x) {
  d <- density(x$res, n=500)
  res <- data.frame(x=max(x$x)- d$y*10, y=d$x+mean(x$y))
  res <- res[order(res$y), ]
  ## Get some data for poisson lines as well
  xs <- seq(min(x$y), max(x$y), len=500)
  res <- rbind(res, data.frame(y=xs,
                               x=max(x$x) - 10*dpois(round(xs), exp(.1*max(x$x)))))
  res$type <- rep(c("empirical", "poisson"), each=500)
  res
}))
dens$section <- rep(levels(dat$section), each=1000)
pois_assume <- ggplot(dat, aes(x, jitter(y, .25))) +
  geom_point(size = 0.1) +
  geom_smooth(method="loess", fill=NA, lwd=1.5, col = '#005CB9') +
  geom_path(data=dens[dens$type=="poisson",], 
            aes(x, y, group=section), 
            color='#E01B5D', lwd=1.1) +
  ylab("y") + xlab("x") +
  geom_vline(xintercept=breaks, lty=2, col = '#868382') +
  theme_ggdist()+
  theme(rect = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA)) 

pois_assume
ols_assume

ggsave('reg_logit.png', units = 'cm', 
       height = 15, width = 15, 
       dpi = 150,bg = "transparent")

tibble(
  lambda_1 = rpois(1000,1),
  lambda_4 = rpois(1000,5),
  lambda_10 = rpois(1000,10)
) %>% 
  gather(lambda, valor) %>% 
  mutate(
    lambda = str_remove_all(lambda, 'lambda_')
  ) %>% 
  group_by(lambda, valor) %>% 
  count() %>% 
  ggplot(
    aes(
      x = valor,
      y = n
    )
  ) + 
  geom_col() +
  facet_wrap(~as.numeric(lambda), ncol = 1) +
  labs(x = 'Conteo', y = '', title = 'Distribución Poisson',subtitle = 'Valores Lambda')+
  theme_ggdist()+
  theme(rect = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA)) 
