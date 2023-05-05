datos  <- read.csv('~/Documents/Ultimo_Semestre/ea2/proyecto/datos.csv')

modelo1  <- lm(formula = holdback ~ groups + schools + teachers + students + schooling_type + oversight_type, data = datos)


datos$group_density = datos$students / datos$groups
datos$teacher_density = datos$teacher / datos$students
modelo2  <- lm(formula = holdback ~ group_density + schools + teacher_density +  + schooling_type + oversight_type, data = datos)


modelo3 <- lm(formula = holdback ~ group_density  + teacher_density, data = datos)


datos$remoto <- ifelse(datos$schooling_type == 'Remote schooling', 1, 0)
modelo4 <- lm(formula = holdback ~ remoto, data = datos)

datos$privada <- ifelse(datos$oversight_type == 'Private', 1, 0)
modelo5  <- lm(formula = holdback ~ group_density + teacher_density  + remoto + privada, data = datos)


datos$holdback_normalizado = datos$holdback / datos$students
modelo6  <- lm(formula = holdback_normalizado ~ group_density + teacher_density  + remoto + privada, data = datos)

m7 <- lm(formula = holdback ~ privada, data = datos)


datos$groups_teacher = datos$groups / datos$teachers
m8 <- lm(formula = holdback ~ groups_teacher, data = datos)

# Cada que en promedio un profesor deba atender un grupo más, hay 11 más atrasados
datos$groups_teacher_normalizado = datos$groups_teacher / datos$students
m9 <- lm(formula = holdback_normalizado ~ groups_teacher_normalizado, data = datos)

m10 <- lm(formula = holdback_normalizado ~ privada + remoto + groups_teacher_normalizado, data = datos)

datos$alumnos_por_escuela = datos$students / datos$schools


m11 <- lm(formula = holdback ~ students, data = datos)

# FINAL
datos$holdback_normalizado = datos$holdback / datos$students
datos$group_density = datos$students / datos$groups
datos$groups_per_teacher = datos$groups / datos$teachers
datos$remoto <- ifelse(datos$schooling_type == 'Remote schooling', 1, 0)
datos$privada <- ifelse(datos$oversight_type == 'Private', 1, 0)

m1  <- lm(holdback_normalizado ~ group_density + groups_per_teacher + remoto + privada, data=datos)

#Código 5 Análisis MCO.R

# install.packages("fpp3")
# install.packages("fable")
# install.packages("forecast")

library(fpp3)  # Usaremos esta librería para generar algunos análisis directos
               # usualmente requeridos en series de tiempo
               # contiene variables económicas cargadas desde FRED
library(fable)
library(forecast)

#us_change >> Cambios porcentuales en variables económicas en EE.UU. 1970 to 2016

us_change |>
  select(-Consumption, -Income) |>
  pivot_longer(-Quarter) |>
  ggplot(aes(Quarter, value, colour = name)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  guides(colour = "none") +
  labs(y="% change")


# MODELO QUE BUSCAMOS

# Consumption = b0 + b1 income + b2 production + b3 unemployment + b4 saving + u


#### UTILIZANDO REPORTEO DE LIBRERÍA fable

fit_consMR <- us_change |> model(tslm = TSLM(Consumption ~ Income + Production +
                      Unemployment + Savings))
report(fit_consMR)

# PERSPECTIVA GENERAL DEL MODELO AJUSTADO
augment(fit_consMR) |>
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Consumption, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = NULL,
       title = "Percent change in US consumption expenditure"
  ) +
  scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
  guides(colour = guide_legend(title = NULL))

# DIAGNÓSTICO GENERAL DE RESIDUALES
fit_consMR |> gg_tsresiduals()


# GRÁFICO DE RESIDUALES CONTRA VARIABLES PREDICTORAS
us_change |>
  left_join(residuals(fit_consMR), by = "Quarter") |>
  pivot_longer(Income:Unemployment,
               names_to = "regressor", values_to = "x") |>
  ggplot(aes(x = x, y = .resid)) +
  geom_point() +
  facet_wrap(. ~ regressor, scales = "free_x") +
  labs(y = "Residuals", x = "")


# GRÁFICO DE RESIDUALES CONTRA VARIABLES AJUSTADA
augment(fit_consMR) |>
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() + labs(x = "Fitted", y = "Residuals")


###############################################################################
###############################################################################
# PARTE 1: MULTICOLINEALIDAD
###############################################################################
###############################################################################

# matriz de correlación
us_change |>
  GGally::ggpairs(columns = 2:6)

fit<- lm(Consumption ~ Income + Production +
                Unemployment + Savings,data=us_change)

summary(fit)

#install.packages('olsrr')
library(olsrr)

####################
# índice de condición
####################

# Las combinaciones lineales son elegidas tales que la primera tenga
# la varianza más grande [el eigenvalue mayor] (sujeta a algunas restricciones
# que no nos son relevantes en este curso). La segunda combinación tiene el
# segundo eigenvalue mayor sujeto a que no est? correlacionado con el primero.
# Y así sucesivamente.

# Recordar Regla: Multicolinealidad es encontrada cuando dos o más variables
# tienen proporciones de varianza altas (>0.5) que corresponden como regla
# de dedo a un índice de condición mayor a 30.

IC<-ols_eigen_cindex(fit)
IC

max(IC$`Condition Index`)  # indica que no tenemos problemas de multicolinealidad


###############################################################################
###############################################################################
# PARTE 2: NORMALIDAD
###############################################################################
###############################################################################

### HISTOGRAM
hist(residuals(fit))

### QQPLOT
qqnorm(residuals(fit), pch = 1, frame = FALSE)
qqline(residuals(fit), col = "steelblue", lwd = 2)

### JARQUE BERA
### H0: NORMALIDAD
#install.packages("tseries")
library(tseries)
jarque.bera.test(residuals(fit))   #LOS RESIDUALES NO SIGUEN LA DIST. NORMAL


###############################################################################
###############################################################################
# PARTE 3: HOMOCEDASTICIDAD
###############################################################################
###############################################################################
library(lmtest)
gqtest(fit)   #Goldfeld-Quandt test..... No rechazo H0
bptest(fit)   #Breusch-Pagan test ..... Rechazo H0 al 5%
bptest(fit, Consumption ~ Income + Production + Unemployment + Savings
       + I(Income^2) + I(Production^2) + I(Unemployment^2)
       + I(Savings^2),data=us_change)  #White test.... Rechazo H0 al 1%


###############################################################################
###############################################################################
# PARTE 4: AUTOCORRELACIÓN SERIAL
###############################################################################
###############################################################################
#install.packages("lmtest")
library(lmtest)

# Durbin-Watson test
# H0: NO AUTOCORRELACI?N

dwtest(Consumption ~ Income + Production +
         Unemployment + Savings,data=us_change)

# Breusch-Godfrey test
# H0: NO AUTOCORRELACI?N

bgtest(Consumption ~ Income + Production +
         Unemployment + Savings,data=us_change, order=1)


###############################################################################
###############################################################################
# PARTE 5: ESTIMACIÓN VÍA MATRICES DE COVARIANZAS ROBUSTAS
###############################################################################
###############################################################################


# Comandos útiles (para nosotros en este momento) del paquete sandwich:
# vcovHAC(): Heteroskedasticity- and autocorrelation-consistent (HAC) estimators
# NeweyWest(): Estimador HAC de Newey-West

library(sandwich)
#?NeweyWest
m<-5
NW_VCOV <- NeweyWest(lm(Consumption ~ Income + Production +
                          Unemployment + Savings,data=us_change),lag=m-1, prewhite = F, adjust = T)

NW_VCOV   #Matriz de covarianza de Newey-West

# Error estandar MCO vs Newy-West
rbind(sqrt(diag(vcov(fit))),sqrt(diag(NW_VCOV)))

# OBTENCIÓN DE RESULTADOS CON MATRICES NEWEY-WEST
cl_robust<-lmtest::coeftest(fit, vcov. = NW_VCOV)

library(stargazer)
stargazer(fit, fit, se = list(NULL, sqrt(diag(NW_VCOV))),type="text")
