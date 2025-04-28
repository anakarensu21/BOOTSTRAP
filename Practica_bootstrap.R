
#PRACTICA DE BOOTSTRAP CLASE DEL 28/04/2025

set.seed(12345)

reclam <- rlnorm(20, meanlog=2.5, sdlog= 2.5)
reclam
bootstrap_mse <- function(data, n_bootstrap){
  mse_values <- numeric(n_bootstrap)
  for (i in 1:n_bootstrap) {
    sample_data <- sample(data, replace = TRUE)
    mse_values[i] <- mean((sample_data - mean(sample_data))^2)
  }
  return(mse_values)
}


#Estimacion del MSE usando bootstrap
n_bootstrap <- 100000
mse_estimate_mean <-mean(bootstrap_mse(reclam, n_bootstrap))
mse_estimate_sd <-sd(bootstrap_mse(reclam, n_bootstrap))

##CALCULAR LOS IC DEL MSE

#Intervalos de confianza al 95% con dist. normal
alpha <- 0.05
z <- qnorm(1-alpha/2)

ci_lower_n <- mse_estimate_mean - z*mse_estimate_sd
ci_upper_n <-  mse_estimate_mean + z*mse_estimate_sd

#Intervalos de confianza al 95% con quantiles
values <-bootstrap_mse(reclam,n_bootstrap)
alpha <- 0.05
z <- qnorm(1-alpha/2)

ci_lower <-quantile(values, 0.025)
ci_upper<- quantile(values, 0.975)

#Resultados de las media para ambos años
cat("La media del MSE es:", format(mse_estimate_mean, big.mark = ","), "\n")
cat("La desviación del MSE es", format(mse_estimate_sd, big.mark = ","), "\n")

#Resultados de los IC
cat("Los intervalos de las reclamaciones son: [",format(ci_lower, big.mark = ","), ",", format(ci_upper, big.mark = ","), "]\n")


## GRAFICAR LA DENSIDAD DEL MSE Y AGREGAR LA MEDIA DEL ERROR CUADRATICO Y LOS IC AL 95%
# Guardar los MSE en un vector

# Ejecutar bootstrap

mean_mse <- mse_estimate_mean
ci_mse <- quantile(values, c(0.025, 0.975))  # IC al 95%

# Graficar densidad

plot(density(values), 
     main = "Densidad del MSE Bootstrap",
     xlab = "MSE", 
     col = "blue", 
     lwd = 2)

# Agregar línea para la media del MSE
abline(v = mse_estimate_mean, col = "red", lwd = 2, lty = 2)

# Agregar líneas para los intervalos de confianza al 95%
abline(v = ci_mse[1], col = "darkgreen", lwd = 2, lty = 3)
abline(v = ci_mse[2], col = "darkgreen", lwd = 2, lty = 3)

# Leyenda
legend("topright", legend = c("Media MSE", "IC 95%"),
       col = c("red", "darkgreen"),
       lty = c(2, 3), lwd = 2)



########### REPETIR EL TEMA 3.5 PARA EL ARCHIVO RECLAM.CSV #######################################


install.packages("readr")
library(readr)
datos <- read.csv("reclam.csv",header = FALSE)
vector <-datos$V1

set.seed(12345)
bootstrap_mse <- function(vector, n_bootstrap){
  mse_values <- numeric(n_bootstrap)
  for (i in 1:n_bootstrap) {
    sample_data <- sample(vector, replace = TRUE)
    mse_values[i] <- mean((sample_data - mean(sample_data))^2)
  }
  return(mse_values)
}


#Estimacion del MSE usando bootstrap
n_bootstrap <- 10000
mse_estimate_mean <-mean(bootstrap_mse(vector, n_bootstrap))
mse_estimate_sd <-sd(bootstrap_mse(vector, n_bootstrap))

##CALCULAR LOS IC DEL MSE

#Intervalos de confianza al 95% con dist. normal
alpha <- 0.05
z <- qnorm(1-alpha/2)

ci_lower_n <- mse_estimate_mean - z*mse_estimate_sd
ci_upper_n <-  mse_estimate_mean + z*mse_estimate_sd

#Intervalos de confianza al 95% con quantiles
values <-bootstrap_mse(vector,n_bootstrap)
alpha <- 0.05
z <- qnorm(1-alpha/2)

ci_lower <-quantile(values, 0.025)
ci_upper<- quantile(values, 0.975)

#Resultados de las media para ambos años
cat("La media del MSE es:", format(mse_estimate_mean, big.mark = ","), "\n")
cat("La desviación del MSE es", format(mse_estimate_sd, big.mark = ","), "\n")

#Resultados de los IC
cat("Los intervalos de las reclamaciones son: [",format(ci_lower, big.mark = ","), ",", format(ci_upper, big.mark = ","), "]\n")


## GRAFICAR LA DENSIDAD DEL MSE Y AGREGAR LA MEDIA DEL ERROR CUADRATICO Y LOS IC AL 95%
# Guardar los MSE en un vector

# Ejecutar bootstrap

mean_mse <- mse_estimate_mean
ci_mse <- quantile(values, c(0.025, 0.975))  # IC al 95%

# Graficar densidad

plot(density(values), 
     main = "Densidad del MSE Bootstrap",
     xlab = "MSE", 
     col = "blue", 
     lwd = 2)

# Agregar línea para la media del MSE
abline(v = mse_estimate_mean, col = "red", lwd = 2, lty = 2)

# Agregar líneas para los intervalos de confianza al 95%
abline(v = ci_mse[1], col = "darkgreen", lwd = 2, lty = 3)
abline(v = ci_mse[2], col = "darkgreen", lwd = 2, lty = 3)

# Leyenda
legend("topright", legend = c("Media MSE", "IC 95%"),
       col = c("red", "darkgreen"),
       lty = c(2, 3), lwd = 2)

