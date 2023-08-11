source("rdocs/source/packages.R")

# ---------------------------------------------------------------------------- #

#        ______   _____  ________      ________ 
#      |  ____| / ____| |__   __| /\  |__   __|
#     | |__    | (___     | |   /  \    | |   
#    |  __|    \___ \    | |  / /\ \   | |   
#   | |____   ____) |   | |  /____ \  | |   
#  |______   |_____/   |_| /_/    \_\|_|   
#  
#         Consultoria estatística 
#

# ---------------------------------------------------------------------------- #
# ############################## README ###################################### #
# Consultor, favor utilizar este arquivo .R para realizar TODAS as análises
# alocadas a você neste projeto pelo gerente responsável, salvo instrução 
# explícita do gerente para mudança.
#
# Escreva seu código da forma mais clara e legível possível, eliminando códigos
# de teste depreciados, ou ao menos deixando como comentário. Dê preferência
# as funções dos pacotes contidos no Tidyverse para realizar suas análises.
# ---------------------------------------------------------------------------- #

# 1.0) Mudando a geometria do banco, para adaptar as funções que utilizaremos ----

tipo <- c("SUPRA","INFRA")
musculo <- c("SCMi","SCMc","TRAPi","TRAPc","SCi","SCc")

SUPRA <- read_excel("banco/ESTAT.xlsx", sheet = "SUPRA", 
                             range = "B2:M57", na = "*")

INFRA <- read_excel("banco/ESTAT.xlsx", sheet = "INFRA", 
                    range = "B2:M61", na = "*")

AMP <- c(1,3,5,7,9,11)
LAT <- c(2,4,6,8,10,12)

supra_amp <- c(t(SUPRA[,AMP]))
supra_lat <- c(t(SUPRA[,LAT]))

infra_amp <- c(t(INFRA[,AMP]))
infra_lat <- c(t(INFRA[,LAT]))

amp <- c(supra_amp,infra_amp)
lat <- c(supra_lat,infra_lat)

df <- data.frame(matrix(NA, nrow = 684, ncol = 4))
df[, 1] <- c(amp, rep(NA, 684 - length(amp)))
df[, 2] <- c(lat, rep(NA, 684 - length(lat)))

df[1:330, 3] <- "supra"
df[331:684, 3] <- "infra"

df[, 4] <- rep(musculo, length.out = 684)

valores <- c(df$X1,df$X2)
tipo <- rep(df$X3,2)
musculo <- rep(df$X4,2)

df <- data.frame(valores,tipo,musculo)
df$amp_lat <- NA
df[1:684, 4] <- "AMP"
df[685:1368, 4] <- "LAT"

rm(INFRA,SUPRA,amp,AMP,infra_amp,infra_lat,lat,LAT,musculo,supra_amp,supra_lat,
   tipo,valores)

df$tipo <- factor(df$tipo)
df$musculo <- factor(df$musculo)
df$amp_lat <- factor(df$amp_lat)

saveRDS(df,"banco/df.rds")

rm(df)

# ---------------------------------------------------------------------------- #




# 2.0) Análises ----


