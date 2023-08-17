source("rdocs/source/packages.R")
pacman::p_load(DescTools, asbio, gridExtra)

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






#Latência (Falta teste para Homocedasticidade)
SupraLat <- Supra %>% filter(amp_lat == "LAT")
#Exploratória
#Sem transformação
ggplot(SupraLat) +  aes(x = musculo,y = valores) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Músculo", y = "Latência") +
  theme_estat() +
  scale_x_discrete(labels = function(x) str_wrap(x,width = 20))

#Com transformação
ggplot(SupraLat) +  aes(x = musculo,y = log(valores)) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Músculo", y = "Latência") +
  theme_estat() +
  scale_x_discrete(labels = function(x) str_wrap(x,width = 20))
#ggsave("resultados/Supra/SupraLat_Box.pdf", width = 158, height = 93, units = "mm")


#Anova
AnovaLatSup <- aov(log(SupraLat$valores) ~ as.factor(SupraLat$musculo))
summary(AnovaLatSup)
TukeyHSD(AnovaLatSup)

#Pressupostos (Usar resíduos Studentizados)
Res <- rstudent(AnovaLatSup)
yfit <- fitted(AnovaLatSup)
ResLatSup <- data.frame(Res=Res,yfit=yfit, Musculo = AnovaLatSup$model[[2]])
#Normalidade
shapiro.test(ResLatSup$Res)
#Verificar se tem o formato aproximado da Normal
hist(ResLatSup$Res)
ggplot(ResLatSup) +
  aes(sample = Res) +
  stat_qq(colour = "#A11D21") +
  stat_qq_line(linewidth = 0.8) + 
  labs(
    x = "Quantis da Teóricos da Normal",
    y = "Quantis da Amostra"
  ) +
  theme_estat()
#ggsave("resultados/Supra/SupraLat_Norm.pdf", width = 158, height = 93, units = "mm")

#Independencia
ggplot(ResLatSup) +
  aes(x = 1:length(Res), y = Res) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Observação",
    y = "Resíduos Studentizados"
  ) +
  theme_estat()
#ggsave("resultados/Supra/SupraLat_Ind.pdf", width = 158, height = 93, units = "mm")

#Homocedasticidade
LeveneTest(log(SupraLat$valores) ~ SupraLat$musculo)
bartlett.test(log(SupraLat$valores), SupraLat$musculo)
ggplot(ResLatSup) +
  aes(x = Musculo, y = Res) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Músculo",
    y = "Resíduos Studentizados"
  ) +
  theme_estat()
#ggsave("resultados/Supra/SupraLat_Homo.pdf", width = 158, height = 93, units = "mm")

#Pressupostos Rejeitados sem transformação (Normalidade), aceitos com transformação log (Obaaa)


#Amplitude (Precisa de uma análise do log)
SupraAmp <- Supra %>% filter(amp_lat == "AMP")
#Exploratória
#Sem transformação
ggplot(SupraAmp) +  aes(x = musculo,y = valores) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Músculo", y = "Amplitude") +
  theme_estat() +
  scale_x_discrete(labels = function(x) str_wrap(x,width = 20))

#Com transformação
ggplot(SupraAmp) +  aes(x = musculo,y = log(valores)) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Músculo", y = "Logarítimo Natural da Amplitude") +
  theme_estat() +
  scale_x_discrete(labels = function(x) str_wrap(x,width = 20))
#ggsave("resultados/Supra/SupraAmp_Box.pdf", width = 158, height = 93, units = "mm")


#Anova
AnovaAmpSup <- aov(log(SupraAmp$valores) ~ as.factor(SupraAmp$musculo))
summary(AnovaAmpSup)
TukeyHSD(AnovaAmpSup)

#Pressupostos (Usar resíduos Studentizados)
Res <- rstudent(AnovaAmpSup)
yfit <- fitted(AnovaAmpSup)
ResAmpSup <- data.frame(Res=Res,yfit=yfit, Musculo = AnovaAmpSup$model[[2]])
#Normalidade
shapiro.test(ResAmpSup$Res)
#Verificar se tem o formato aproximado da Normal
hist(ResAmpSup$Res)
ggplot(ResAmpSup) +
  aes(sample = Res) +
  stat_qq(colour = "#A11D21") +
  stat_qq_line(linewidth = 0.8) + 
  labs(
    x = "Quantis da Teóricos da Normal",
    y = "Quantis da Amostra"
  ) +
  theme_estat()
#ggsave("resultados/Supra/SupraAmp_Norm.pdf", width = 158, height = 93, units = "mm")

#Independencia
ggplot(ResAmpSup) +
  aes(x = 1:length(Res), y = Res) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Observação",
    y = "Resíduos Studentizados"
  ) +
  theme_estat()
#ggsave("resultados/Supra/SupraAmp_Ind.pdf", width = 158, height = 93, units = "mm")

#Homocedasticidade
LeveneTest(log(SupraAmp$valores) ~ SupraAmp$musculo)
bartlett.test(log(SupraAmp$valores), SupraAmp$musculo)
ggplot(ResAmpSup) +
  aes(x = Musculo, y = Res) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Musculo",
    y = "Resíduos Studentizados"
  ) +
  theme_estat()
#ggsave("resultados/Supra/SupraAmp_Homo.pdf", width = 158, height = 93, units = "mm")

#Pressupostos Rejeitados sem transformação, aceitos com transformação log (Obaaa)


#Reprodutividade
SupraRep <- Supra %>% filter(amp_lat == "AMP") %>% select(c("valores","musculo"))
SupraRep$valores <- ifelse(is.na(SupraRep$valores), 0, 1)

#Acho que Anova ta errada
# AnovaRepSup <- aov(SupraRep$valores ~ as.factor(SupraRep$musculo))
# summary(AnovaRepSup)
# TukeyHSD(AnovaRepSup)


#na mão
SupraRepEst <- SupraRep %>% group_by(musculo) %>% summarise(Media = mean(valores),
                                                            Soma = sum(valores),
                                                            n = n(),
                                                            Var = mean(valores)*(1-mean(valores))/n(),
                                                            DP = sqrt(mean(valores)*(1-mean(valores))/n()))
#Gráfico com repordutibilidade média e desvio padrão
ggplot(SupraRepEst) +
  aes(x = musculo, y = Media) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_errorbar(aes(ymin=Media-DP, ymax=Media+DP), width=.2) +
  labs(x = "Músculo", y = "Reprodutibilidade") +
  theme_estat()
#ggsave("resultados/Supra/SupraRep_MedDP.pdf", width = 158, height = 93, units = "mm")


PropSupraRep <- prop.test(x = SupraRepEst$Soma, n = SupraRepEst$n)
PropSupraRep

#Deve da pra fazer isso com um "for" mas to com preguiça
prop.test(x = SupraRepEst$Soma[c(1,2)], n = SupraRepEst$n[c(1,2)])
prop.test(x = SupraRepEst$Soma[c(1,3)], n = SupraRepEst$n[c(1,3)])
prop.test(x = SupraRepEst$Soma[c(1,4)], n = SupraRepEst$n[c(1,4)])
prop.test(x = SupraRepEst$Soma[c(1,5)], n = SupraRepEst$n[c(1,5)])
prop.test(x = SupraRepEst$Soma[c(1,6)], n = SupraRepEst$n[c(1,6)])
prop.test(x = SupraRepEst$Soma[c(2,3)], n = SupraRepEst$n[c(2,3)])
prop.test(x = SupraRepEst$Soma[c(2,4)], n = SupraRepEst$n[c(2,4)])
prop.test(x = SupraRepEst$Soma[c(2,5)], n = SupraRepEst$n[c(2,5)])
prop.test(x = SupraRepEst$Soma[c(2,6)], n = SupraRepEst$n[c(2,6)])
prop.test(x = SupraRepEst$Soma[c(3,4)], n = SupraRepEst$n[c(3,4)])
prop.test(x = SupraRepEst$Soma[c(3,5)], n = SupraRepEst$n[c(3,5)])
prop.test(x = SupraRepEst$Soma[c(3,6)], n = SupraRepEst$n[c(3,6)])
prop.test(x = SupraRepEst$Soma[c(4,5)], n = SupraRepEst$n[c(4,5)])
prop.test(x = SupraRepEst$Soma[c(4,6)], n = SupraRepEst$n[c(4,6)])
prop.test(x = SupraRepEst$Soma[c(5,6)], n = SupraRepEst$n[c(5,6)])

