library (tidyverse)
library(scales) # para ggplot

df <- read.csv2("2019-11 lecturas.csv", as.is = TRUE)
names(df) <- c("fecha", "kWh_lectura", "origen", "facturada", "X")

df <- df %>%
    separate(fecha, c(NA, "dia", "mes", "año"), " ") %>%
    select(dia, mes, año, kWh_lectura) %>%
    mutate(mes = tolower(mes)) %>%
    drop_na() 

df$fecha <- as.Date(paste("20",df$año,"-", df$mes,".-", df$dia, sep=""), format = "%Y-%b-%d") 

df <- df %>% 
  select(fecha, kWh_lectura)

# los consumos son la diferencia entre dos lecturas sucesivas
# el primer valor no existe porque no se calcula
df$consumo <- c(-diff(df$kWh_lectura), NA)
#df$consumo[1] <- NA

ggplot(df, aes(x = fecha, y = consumo)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  scale_x_date(date_breaks = "2 month", labels = date_format("%b %y"))

