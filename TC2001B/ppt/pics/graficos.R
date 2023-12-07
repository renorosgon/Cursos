library(tidyverse)
library(ggdist)

set.seed(0)
dat <- tibble(x=runif(200, -5, 10),
              p=exp(-2+1*x)/(1+exp(-2+1*x)),
              y=rbinom(200, 1, p),
              y2=.3408+.0901*x,
              logit=log(p/(1-p)))
dat2 <- tibble(x = c(dat$x, dat$x),
               y = c(dat$y2, dat$p),
               `Regression model` = c(rep("linear", 200),
                                      rep("logistic", 200)))
ggplot() + 
  geom_point(data = dat, aes(x, y),
             alpha = 0.25, col = 'gray15') +
  geom_line(data = dat2[dat2$`Regression model`=='logistic',], 
            aes(x, y, linetype = `Regression model`)) +
  scale_y_continuous(breaks = c(0,1)) +
  labs(y = 'Prob( Y= 1)') +
  theme_ggdist()+
  theme(legend.position = 'none',
        rect = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA))




ggplot( data = dat,aes(x =x, y=logit)) +
  geom_point(alpha = 0.25, col = 'gray15') + 
  labs(y = 'Log(P/(1-P))') +
  theme_ggdist()+
  theme(legend.position = 'none',
        rect = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA))





impureza <- function(p){
  -(p*log(p) + (1-p)*log(1-p))
}

df = tibble(y = impureza(seq(0.0001,.9999,0.00005)),
            x = seq(0.0001,.9999,0.00005))

ggplot(df, aes(x=x, y = y))+
  geom_line() +
  theme_ggdist()+
  labs(x = 'Probabilidad', y='Entropia') +
  theme(legend.position = 'none',
        rect = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA))



predicciones = c()
for(i in seq(1,10)){
  x <- runif(200,0,1)
  y <- 2*x + rnorm(200,0,0.1)
  arbol <- rpart(y~x, data=tibble(x=x, y=y), method = 'anova')
  x_pred <- seq(0,1,0.05)
  y_pred <- predict(arbol, newdata = data_frame(x=x_pred))
  y_verdadera <- 2*x_pred
  predicciones = bind_cols(predicciones, y_pred)
}

predicciones %>% 
  rename_all(str_replace_all,'...','pred_') %>% 
  mutate(pred_media = rowMeans(predicciones)) %>% 
  gather('prediccion','valor') %>% 
  mutate(x_pred = rep(x_pred,11)) %>% 
  ggplot(aes(x=x_pred, y=valor, 
             colour=prediccion, 
             linetype=prediccion!='pred_media')) +
  geom_line() + 
  theme_ggdist()+
  labs(x = 'x', y='y', col = '') +
  scale_color_manual(values  =c(rep('gray70',10),'#B6171A'))+
  theme(legend.position = 'none',
        rect = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA))


x <- runif(200,0,1)
y <- 2*x + rnorm(200,0,0.1)
arbol <- rpart(y~x, data=tibble(x=x, y=y), method = 'anova')


dat <- data_frame(x_pred=x_pred, 
                  y_pred=y_pred, y_verdadera=y_verdadera) %>% gather(y, valor, y_pred:y_verdadera)
ggplot(dat, aes(x=x_pred, y=valor, colour=y)) +
  geom_line() + 
  theme_ggdist()+
  labs(x = 'x', y='y', col = '') +
  scale_color_manual(values =c('#B6171A','#868382'))+
  theme(legend.position = 'top',
        rect = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA))
