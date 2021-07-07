# Parámetros
Lv       <- 10        # Esperanza de vida de los mosquitos (en días)
Lh       <- 50 * 365  # Esperanza de vida de los humanos (en días)
Iph      <- 7         # Periodo infeccioso en humanos (en días)
IP       <- 6         # Periodo infeccioso en vectores (en días)
EIP      <- 8.4       # Período de incubación extrínseco en mosquitos adultos
muv      <- 1/Lv      # Mortalidad en mosquitos
muh      <- 1/Lh      # Mortalidad en humanos
gamma    <- 1/Iph     # Tasa de recuperación en humanos
delta    <- 1/EIP     # Tasa de incubación extrínseca
# Tamaño de la población
Nh       <- 2.4 * 10^6# Número de humanos (Población de Cali, Colombia)
m        <- 2         # Proporción vector a humano
Nv       <- m * Nh    # Número de vectores
betah    <- 0.7       # Probabilidad de transmisión del vector al hospedador
betav    <- 0.7       # Probabilidad de transmisión del hospedador al vector 
R0       <- 3         # Número reproductivo
b        <- sqrt((R0 ^2 * muv*(muv+delta) * (muh+gamma)) /
                   (m * betah * betav * delta)) # tasa de picadura


lubridate:: epiweek("2021-04-15")
