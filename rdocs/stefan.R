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

Banco <- readRDS("banco/df.rds")

Supra <- Banco %>% filter(tipo == "supra")

#Latência
#Anova
SupraLat <- Supra %>% filter(amp_lat == "LAT")
AnovaLatSup <- aov(SupraLat$valores ~ as.factor(SupraLat$musculo))
summary(AnovaLatSup)
par(mfrow=c(1,3))

#Pressupostos (Usar resíduos Studentizados)
Res <- rstudent(AnovaLatSup)
yfit <- fitted(AnovaLatSup)
ResLatSup <- data.frame(Res=Res,yfit=yfit)
#Normalidade
ggplot(ResLatSup) +
  aes(x = yfit, y = Res) +
  geom_point(colour = "black", size = 3) +
  labs(
    x = "valores ajustados da resposta",
    y = "resíduos Studentizados"
  ) +
  theme_estat()
#ggsave("disp_ResValaj.pdf", width = 158, height = 93, units = "mm")
shapiro.test(ResLatSup)
qqnorm(ResLatSup)
qqline(ResLatSup)

#Independencia
plot(ResLatSup)

#Homocedasticidade
plot(ResLatSup, AnovaLatSup$fitted.values)