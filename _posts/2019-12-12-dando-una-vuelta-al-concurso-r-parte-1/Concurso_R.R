load ("Carrizal.RData")
load ("Canizal.RData")
load ("Granja.RData")
etiquetas <- read.csv2("descripcion variables.csv")

library(tidyverse)

# df <- granja
df <- full_join(canizal,carrizal)
df <- full_join(df,granja)

str(df)

# probé una forma de etiquetar las variables para ver la descripción pero 
# interfiere con las operaciones en ggplot más tarde
#library(Hmisc)
#nombres <- as.vector(etiquetas$descripcion)
#variables <- as.vector(etiquetas$variable)
#label(df) <- as.list(nombres[match(names(df), variables)])

## Formatear resumen de datos en un DF
resumen <- do.call(cbind, lapply(df, summary))
resumen <- as.data.frame(t(resumen))

## tabla para Rmarkdown
knitr::kable(resumen)

## conversiones de fechas que son útiles para agrupar más tarde
df$fecha <- as.Date(df$TIMESTAMP)
df$dia <- as.Date(cut(df$fecha, "day"))
df$semana <- as.Date(cut(df$fecha, "week"))
df$mes <- as.Date(cut(df$fecha, "month"))
df$año <- as.Date(cut(df$fecha, "year"))
df$diasemana <- format(df$fecha,"%A")
df$diasemana <- factor(df$diasemana, levels=c("lunes","martes","miércoles","jueves","viernes","sábado","domingo"),ordered=TRUE)

hist(df$Lluvia_Tot)
hist(df$Temp_Aire_Avg)
hist(df$Hum_Rel_Avg)

boxplot(df$Lluvia_Tot~format(df$año, "%Y"))
boxplot(df$Lluvia_Tot~format(df$mes, "%m"))
boxplot(df$Lluvia_Tot~df$diasemana)

boxplot(df$Temp_Aire_Avg~format(df$año, "%Y"))
boxplot(df$Temp_Aire_Avg~format(df$mes, "%m"))
boxplot(df$Temp_Aire_Avg~df$diasemana)

boxplot(df$Hum_Rel_Avg~format(df$año, "%Y"))
boxplot(df$Hum_Rel_Avg~format(df$mes, "%m"))
boxplot(df$Hum_Rel_Avg~df$diasemana)

library(tidyverse)

df <- df [df$Hum_Rel_Avg > 0,]
df <- df [df$Hum_Rel_Avg > 3,]

ggplot(data = df, aes (as.factor(format(año,"%Y")),Hum_Rel_Avg)) +
  geom_boxplot(aes(colour = estacion )) 

ggplot(data = df, aes (as.factor(format(mes,"%m")),Hum_Rel_Avg)) +
  geom_boxplot(aes(colour = estacion )) 

ggplot(data = df, aes (as.factor(format(mes,"%m")),Hum_Rel_Avg)) +
  geom_boxplot(aes(colour = estacion )) +
  facet_grid(as.factor(format(año,"%Y"))~.)
             
df <- df [df$fecha >= "2009-01-01",]
df <- df [df$fecha < "2019-01-01",]

#https://drsimonj.svbtle.com/quick-plot-of-all-variables
df %>%
#  keep(is.numeric) %>% 
#  select (-RECORD) %>%  ## es numerica pero no aporta nada
#  gather() %>% 
  select (-TIMESTAMP,-RECORD, -c(latitud:diasemana)) %>%
  pivot_longer(-estacion, names_to = "key", values_to = "value") %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(aes(colour = estacion), alpha = 0.2)

df %>%
  #  keep(is.numeric) %>% 
  #  select (-RECORD) %>%  ## es numerica pero no aporta nada
  #  gather() %>% 
  select (-TIMESTAMP,-RECORD, -c(latitud:diasemana)) %>%
  pivot_longer(-estacion, names_to = "key", values_to = "value") %>%
#  sample_n(5000) %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_density(aes(fill = estacion), alpha = 0.2)

df %>%
  select (-TIMESTAMP,-RECORD, -c(latitud:diasemana)) %>%
  pivot_longer(-estacion, names_to = "key", values_to = "value") %>%
  ggplot(aes(y = value)) +
  facet_wrap(~ key, scales = "free") +
  geom_boxplot(aes(colour = estacion ))

library(corrplot)
tablanum <- df %>%
  keep(is.numeric) %>% 
  select (-RECORD) ## es numerica pero no aporta nada

tablanum %>%  
  sample_n(300) %>%
  pairs()

matcor <- cor(tablanum, use = "complete.obs")
corrplot(matcor, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


#análisis de series
# agrupacion por meses 
library(lubridate)
df1 <- df %>% 
#  filter (estacion == "canizal") %>%
  group_by(fecha = floor_date(fecha, "month")) %>% 
  summarise(Hum_Rel_Avg = mean(Hum_Rel_Avg,  na.rm = TRUE))

tsgp <- ts(df1$Hum_Rel_Avg,frequency=12,start=c(2010,01))
plot(tsgp)
tsgpcompo <- decompose(tsgp)
plot(tsgpcompo)
