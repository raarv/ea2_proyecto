library(olsrr)
library(tseries)
library(ggplot2)
library(fpp3)
# Usaremos esta librería para generar algunos análisis directos
# usualmente requeridos en series de tiempo
# contiene variables económicas cargadas desde FRED
library(fable)
library(xtable)

# Load the dataset
data  <- read.csv('datos.csv')

# First model
m1 <- lm(house_price_of_unit_area ~  house_age , data=data)

summary(m1)
plot(m1)

jarque.bera.test(residuals(m1))

IC<-ols_eigen_cindex(m1)
IC

max(IC$`Condition Index`)

# Second model (controlamos utilizando todas las variables)
m2 <- lm(house_price_of_unit_area ~  house_age + number_of_convenience_stores + distance_to_the_nearest_MRT_station + latitude +  longitude, data=data)

summary(m2)
plot(m2)

jarque.bera.test(residuals(m2))
IC<-ols_eigen_cindex(m2)
IC

max(IC$`Condition Index`)

# Third model (Después del análisis vemos que latitud y longitud provocan multicolinealidad imperfecta y por lo tanto decidimos eliminarlas de la especificación del modelo)
m3 <- lm(house_price_of_unit_area ~  house_age + number_of_convenience_stores + distance_to_the_nearest_MRT_station, data=data)

summary(m3)
plot(m3)

jarque.bera.test(residuals(m3))
IC<-ols_eigen_cindex(m3)
IC

max(IC$`Condition Index`)
