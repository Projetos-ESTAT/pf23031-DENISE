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

p_load(DescTools, asbio, gridExtra, lmtest,agricolae)

Banco <- readRDS("banco/df.rds")

Infra <- Banco %>%
  filter(tipo == "infra")

#Latência (Falta teste para Homocedasticidade)
InfraLat <- Infra %>%
  filter(amp_lat == "LAT")

#Exploratória

#Sem transformação

ggplot(InfraLat) +  aes(x = musculo,y = valores) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Músculo", y = "Latência") +
  theme_estat() +
  scale_x_discrete(labels = function(x) str_wrap(x,width = 20))
#ggsave("resultados/Infra/InfraLat_Box.pdf", width = 158, height = 93, units = "mm")

#Com transformação
ggplot(InfraLat) +  aes(x = musculo,y = log(valores)) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Músculo", y = "Logaritmo Natural da Latência") +
  theme_estat() +
  scale_x_discrete(labels = function(x) str_wrap(x,width = 20))
#ggsave("resultados/Infra/InfraLat_BoxLog.pdf", width = 158, height = 93, units = "mm")

#Anova
AnovaLatInfra <- aov(log(InfraLat$valores) ~ as.factor(InfraLat$musculo))
summary(AnovaLatInfra)
TukeyHSD(AnovaLatInfra)

#Pressupostos (Usar resíduos Studentizados)
Res <- rstudent(AnovaLatInfra)
yfit <- fitted(AnovaLatInfra)
ResLatInfra <- data.frame(Res=Res,yfit=yfit, Musculo = AnovaLatInfra$model[[2]])

#Normalidade
shapiro.test(ResLatInfra$Res)
#Verificar se tem o formato aproximado da Normal
hist(ResLatInfra$Res)
ggplot(ResLatInfra) +
  aes(sample = Res) +
  stat_qq(colour = "#A11D21") +
  stat_qq_line(linewidth = 0.8) + 
  labs(
    x = "Quantis da Teóricos da Normal",
    y = "Quantis da Amostra"
  ) +
  theme_estat()
#ggsave("resultados/Infra/InfraLat_Norm.pdf", width = 158, height = 93, units = "mm")

#Independencia
#dwtest(AnovaLatInfra) #Tem algo de errado com esse teste

ggplot(ResLatInfra) +
  aes(x = 1:length(Res), y = Res) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Observação",
    y = "Resíduos Studentizados"
  ) +
  theme_estat()
#ggsave("resultados/Infra/InfraLat_Ind.pdf", width = 158, height = 93, units = "mm")

#Homocedasticidade
LeveneTest(log(InfraLat$valores) ~ InfraLat$musculo)
bartlett.test(log(InfraLat$valores), InfraLat$musculo)
ggplot(ResLatInfra) +
  aes(x = Musculo, y = Res) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Músculo",
    y = "Resíduos Studentizados"
  ) +
  theme_estat()
#ggsave("resultados/Infra/InfraLat_Homo.pdf", width = 158, height = 93, units = "mm")

#Pressupostos Rejeitados sem transformação (Normalidade), aceitos com transformação log (Obaaa)
# Verificar a frase acima depois ----

#estimações
for (i in unique(InfraLat$musculo)) {
  a <- shapiro.test(InfraLat$valores[InfraLat$musculo==i])$p.value
  b <- shapiro.test(log(InfraLat$valores[InfraLat$musculo==i]))$p.value
  if(a<0.05 & b>0.05)
  {print(paste(str(i), "Normal apenas com transformação log"))}
  if(a<0.05 & b<0.05)
  {print(paste(str(i), "Não Normal mesmo com transformação log"))}
}

LSDInfraLat <- LSD.test(AnovaLatInfra, "as.factor(InfraLat$musculo)")
LSDInfraLat
#Pegar o Exp pras estimativas
exp(LSDInfraLat)

InfraLatEst <- exp(LSDInfraLat$means[c(1,5,6)])
InfraLatEst$musculo <- rownames(InfraLatEst)

#Gráfico com latência média e IC
ggplot(InfraLatEst) +
  aes(x = musculo, y = `log(InfraLat$valores)`) +
  geom_errorbar(aes(ymin=LCL, ymax=UCL), width=.2) +
  geom_point(stat = "identity", color = "black", size= 3) +
  labs(x = "Músculo", y = "Latência") +
  theme_estat()
#ggsave("resultados/Infra/InfraLat_MedIC.pdf", width = 158, height = 93, units = "mm")

#Amplitude (Precisa de uma análise do log)
InfraAmp <- Infra %>% filter(amp_lat == "AMP")
#Exploratória
#Sem transformação
ggplot(InfraAmp) +  aes(x = musculo,y = valores) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Músculo", y = "Amplitude") +
  theme_estat() +
  scale_x_discrete(labels = function(x) str_wrap(x,width = 20))
#ggsave("resultados/Infra/InfraAmp_Box.pdf", width = 158, height = 93, units = "mm")

#Com transformação
ggplot(InfraAmp) +  aes(x = musculo,y = log(valores)) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Músculo", y = "Logaritmo Natural da Amplitude") +
  theme_estat() +
  scale_x_discrete(labels = function(x) str_wrap(x,width = 20))
#ggsave("resultados/Infra/InfraAmp_BoxLog.pdf", width = 158, height = 93, units = "mm")

#Anova
AnovaAmpInfra <- aov(log(InfraAmp$valores) ~ as.factor(InfraAmp$musculo))
summary(AnovaAmpInfra)
TukeyHSD(AnovaAmpInfra)

#Pressupostos (Usar resíduos Studentizados)
Res <- rstudent(AnovaAmpInfra)
yfit <- fitted(AnovaAmpInfra)
ResAmpInfra <- data.frame(Res=Res,yfit=yfit, Musculo = AnovaAmpInfra$model[[2]])

#Normalidade
shapiro.test(ResAmpInfra$Res)
#Verificar se tem o formato aproximado da Normal
hist(ResAmpInfra$Res)
ggplot(ResAmpInfra) +
  aes(sample = Res) +
  stat_qq(colour = "#A11D21") +
  stat_qq_line(linewidth = 0.8) + 
  labs(
    x = "Quantis da Teóricos da Normal",
    y = "Quantis da Amostra"
  ) +
  theme_estat()
#ggsave("resultados/Infra/InfraAmp_Norm.pdf", width = 158, height = 93, units = "mm")

#Independencia
#dwtest(AnovaAmpInfra) #Tem algo de errado com esse teste

ggplot(ResAmpInfra) +
  aes(x = 1:length(Res), y = Res) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Observação",
    y = "Resíduos Studentizados"
  ) +
  theme_estat()
#ggsave("resultados/Infra/InfraAmp_Ind.pdf", width = 158, height = 93, units = "mm")

#Homocedasticidade
LeveneTest(log(InfraAmp$valores) ~ InfraAmp$musculo)
bartlett.test(log(InfraAmp$valores), InfraAmp$musculo)
ggplot(ResAmpInfra) +
  aes(x = Musculo, y = Res) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Musculo",
    y = "Resíduos Studentizados"
  ) +
  theme_estat()
#ggsave("resultados/Infra/InfraAmp_Homo.pdf", width = 158, height = 93, units = "mm")

#Pressupostos Rejeitados sem transformação, aceitos com transformação log (Obaaa)

#estimações
for (i in unique(InfraAmp$musculo)) {
  a <- shapiro.test(InfraAmp$valores[InfraAmp$musculo==i])$p.value
  b <- shapiro.test(log(InfraAmp$valores[InfraAmp$musculo==i]))$p.value
  if(a<0.05 & b>0.05)
  {print(paste(str(i), "Normal apenas com transformação log"))}
  if(a<0.05 & b<0.05)
  {print(paste(str(i), "Não Normal mesmo com transformação log"))}
}

LSDInfraAmp <- LSD.test(AnovaAmpInfra, "as.factor(InfraAmp$musculo)")
LSDInfraAmp
#Pegar o Exp pras estimativas
exp(LSDInfraAmp)

InfraAmpEst <- exp(LSDInfraAmp$means[c(1,5,6)])
InfraAmpEst$musculo <- rownames(InfraAmpEst)

#Gráfico com amplitude média e IC
ggplot(InfraAmpEst) +
  aes(x = musculo, y = `log(InfraAmp$valores)`) +
  geom_point(stat = "identity", color = "black", size=3) +
  geom_errorbar(aes(ymin=LCL, ymax=UCL), width=.2) +
  labs(x = "Músculo", y = "Amplitude") +
  theme_estat()
#ggsave("resultados/Infra/InfraAmp_MedIC.pdf", width = 158, height = 93, units = "mm")

#Reprodutividade
InfraRep <- Infra %>% filter(amp_lat == "AMP") %>% select(c("valores","musculo"))
InfraRep$valores <- ifelse(is.na(InfraRep$valores), 0, 1)


#na mão
InfraRepEst <- InfraRep %>%
  group_by(musculo) %>%
  summarise(Media = mean(valores),
                                                            Soma = sum(valores),
                                                            n = n(),
                                                            Var = mean(valores)*(1-mean(valores))/n(),
                                                            DP = sqrt(mean(valores)*(1-mean(valores))/n()))
#Gráfico com repordutibilidade média e DP
ggplot(InfraRepEst) +
  aes(x = musculo, y = Media) +
  geom_point(stat = "identity", fill = "black", size=3) +
  geom_errorbar(aes(ymin=Media-DP, ymax=Media+DP), width=.2) +
  labs(x = "Músculo", y = "Reprodutibilidade") +
  theme_estat()
#ggsave("resultados/Infra/InfraRep_MedDP.pdf", width = 158, height = 93, units = "mm")


PropInfraRep <- prop.test(x = InfraRepEst$Soma, n = InfraRepEst$n)
PropInfraRep

#Deve da pra fazer isso com um "for" mas to com preguiça
prop.test(x = InfraRepEst$Soma[c(1,2)], n = InfraRepEst$n[c(1,2)],correct = F)
prop.test(x = InfraRepEst$Soma[c(1,3)], n = InfraRepEst$n[c(1,3)],correct = F)
prop.test(x = InfraRepEst$Soma[c(1,4)], n = InfraRepEst$n[c(1,4)],correct = F)
prop.test(x = InfraRepEst$Soma[c(1,5)], n = InfraRepEst$n[c(1,5)],correct = F)
prop.test(x = InfraRepEst$Soma[c(1,6)], n = InfraRepEst$n[c(1,6)],correct = F)
prop.test(x = InfraRepEst$Soma[c(2,3)], n = InfraRepEst$n[c(2,3)],correct = F)
prop.test(x = InfraRepEst$Soma[c(2,4)], n = InfraRepEst$n[c(2,4)],correct = F)
prop.test(x = InfraRepEst$Soma[c(2,5)], n = InfraRepEst$n[c(2,5)],correct = F)
prop.test(x = InfraRepEst$Soma[c(2,6)], n = InfraRepEst$n[c(2,6)],correct = F)
prop.test(x = InfraRepEst$Soma[c(3,4)], n = InfraRepEst$n[c(3,4)],correct = F)
prop.test(x = InfraRepEst$Soma[c(3,5)], n = InfraRepEst$n[c(3,5)],correct = F)
prop.test(x = InfraRepEst$Soma[c(3,6)], n = InfraRepEst$n[c(3,6)],correct = F)
prop.test(x = InfraRepEst$Soma[c(4,5)], n = InfraRepEst$n[c(4,5)],correct = F)
prop.test(x = InfraRepEst$Soma[c(4,6)], n = InfraRepEst$n[c(4,6)],correct = F)
prop.test(x = InfraRepEst$Soma[c(5,6)], n = InfraRepEst$n[c(5,6)],correct = F)
