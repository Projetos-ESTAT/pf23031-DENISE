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

#Pressupostos (Usar resíduos Studentizados)
Res <- rstudent(AnovaLatSup)
yfit <- fitted(AnovaLatSup)
ResLatSup <- data.frame(Res=Res,yfit=yfit, Musculo = AnovaLatSup$model[[2]])
#Normalidade
shapiro.test(ResLatSup$Res)
ggplot(ResLatSup) +
  aes(sample = Res) +
  stat_qq(colour = "#A11D21", size = 3) + stat_qq_line() +
  theme_estat()
#ggsave("SupraLat_Norm.pdf", width = 158, height = 93, units = "mm")

#Independencia
ggplot(ResLatSup) +
  aes(x = 1:length(Res), y = Res) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Observação",
    y = "Resíduos Studentizados"
  ) +
  theme_estat()
#ggsave("SupraLat_Ind.pdf", width = 158, height = 93, units = "mm")

#Homocedasticidade
ggplot(ResLatSup) +
  aes(x = Musculo, y = Res) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Musculo",
    y = "Resíduos Studentizados"
  ) +
  theme_estat()
#ggsave("SupraLat_Homo.pdf", width = 158, height = 93, units = "mm")



#Amplitude (Precisa de uma análise diferente)
#Anova
SupraAmp <- Supra %>% filter(amp_lat == "AMP")
AnovaAmpSup <- aov(SupraAmp$valores ~ as.factor(SupraAmp$musculo))
summary(AnovaAmpSup)

#Pressupostos (Usar resíduos Studentizados)
Res <- rstudent(AnovaAmpSup)
yfit <- fitted(AnovaAmpSup)
ResAmpSup <- data.frame(Res=Res,yfit=yfit, Musculo = AnovaAmpSup$model[[2]])
#Normalidade
shapiro.test(ResAmpSup$Res)
ggplot(ResAmpSup) +
  aes(sample = Res) +
  stat_qq(colour = "#A11D21", size = 3) + stat_qq_line() +
  theme_estat()
#ggsave("SupraAmp_Norm.pdf", width = 158, height = 93, units = "mm")

#Independencia
ggplot(ResAmpSup) +
  aes(x = 1:length(Res), y = Res) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Observação",
    y = "Resíduos Studentizados"
  ) +
  theme_estat()
#ggsave("SupraAmp_Ind.pdf", width = 158, height = 93, units = "mm")

#Homocedasticidade
ggplot(ResAmpSup) +
  aes(x = Musculo, y = Res) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Musculo",
    y = "Resíduos Studentizados"
  ) +
  theme_estat()
#ggsave("SupraAmp_Homo.pdf", width = 158, height = 93, units = "mm")
#Pressupostos Rejeitados (Obaaa)


#Reprodutividade (Fazendo ainda)
SupraRep <- Supra %>% filter(amp_lat == "AMP") %>% select(c("valores","musculo"))
SupraRep$valores <- ifelse(is.na(SupraRep$valores), 0, 1)
AnovaRepSup <- aov(SupraRep$valores ~ as.factor(SupraRep$musculo))
summary(AnovaRepSup)


#na mão
SupraRepEst <- SupraRep %>% group_by(musculo) %>% summarise(Media = mean(valores),
                                                              Var = Media*(1-Media)/sqrt(n()))
