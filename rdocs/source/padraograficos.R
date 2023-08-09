# > 0. Carregando pacotes ====
source("packages.R")

# > 1. Barras/Colunas ====
## >> 1.1 Colunas com duas frequências ====
### >>> 1.1.1 Univariado ====
classes <- mpg %>%
  filter(!is.na(class)) %>%
  count(class) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

ggplot(classes) +
  aes(
    x = fct_reorder(class, n, .desc = T),
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  labs(x = "manufacturer", y = "Frequência") +
  theme_estat()

ggsave("colunas-uni-freq.pdf", width = 158, height = 93, units = "mm")

### >>> 1.1.2 Bivariado ====
trans_drv <- mpg %>%
  mutate(trans = case_when(
    trans %>% str_detect("auto") ~ "auto",
    trans %>% str_detect("manual") ~ "manual"
  )) %>%
  group_by(trans, drv) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = freq %>% percent()
  )

porcentagens <- str_c(trans_drv$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(trans_drv$freq, " (", porcentagens, ")"))

ggplot(trans_drv) +
  aes(
    x = fct_reorder(trans, freq, .desc = T),
    y = freq,
    fill = drv,
    label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Transmissão", y = "Frequência") +
  theme_estat()
ggsave("colunas-bi-freq.pdf", width = 158, height = 93, units = "mm")

## >> 1.2 Barras com duas frequências ====
### >>> 1.2.1 Univariado ====

ggplot(classes) +
  aes(
    x = fct_reorder(class, n, .desc = T),
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = 0, hjust = -.1,
    size = 3
  ) +
  scale_y_continuous(breaks = seq(0, 70, 20), limits = c(0, 70)) +
  labs(x = "manufacturer", y = "Frequência") +
  theme_estat() +
  coord_flip()
ggsave("barras-uni-freq.pdf", width = 158, height = 93, units = "mm")

### >>> 1.2.2 Bivariado ====
class_drv <- mpg %>%
  group_by(class, drv) %>%
  summarise(freq = n()) %>%
  mutate(freq_relativa = freq %>% percent())

porcentagens <- str_c(class_drv$freq_relativa, "%") %>% str_replace("\\.", ",")
legendas <- str_squish(str_c(class_drv$freq, " (", porcentagens, ")"))

ggplot(class_drv) +
  aes(
    x = fct_reorder(class, freq, .desc = T),
    y = freq,
    fill = drv,
    label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = 0.2, hjust = -0.1,
    size = 3
  ) +
  labs(x = "Class", y = "Frequência") +
  theme_estat() +
  scale_y_continuous(name = "Speed of cars", limits = c(0, 60)) +
  coord_flip()

ggsave("barras-bi-freq.pdf", width = 158, height = 93, units = "mm")

## >> 1.3 Colunas ====
### >>> 1.3.1 Univariado ====
ggplot(mpg) +
  aes(x = class) +
  geom_bar(fill = "#A11D21") +
  labs(x = "Classe do automóvel", y = "Frequência") +
  theme_estat()
ggsave("colunas-uni-freq.pdf", width = 158, height = 93, units = "mm")

### >>> 1.3.2 Univariado com porcentagem no gráfico e no eixo
ggplot(mpg) +
  aes(x = class) +
  geom_bar(aes(y = prop.table(..count..) * 100), fill = "#A11D21") +
  geom_text(aes(
    y = prop.table(..count..) * 100 + 0.5,
    label = paste0(gsub("\\.", ",", round(prop.table(..count..) * 100, 2)), "%")
  ),
  stat = "count", vjust = 0, size = 4
  ) +
  labs(x = "Classe do automóvel", y = "Frequência Relativa") +
  theme_estat()
ggsave("colunas-uni-percent.pdf", width = 158, height = 93, units = "mm")

### >>> 1.3.3 Univariado com porcentagem no gráfico e freq absoluta no eixo ====
ggplot(mpg$class %>% vector_frequencies()) +
  aes(
    x = groups,
    y = absolute,
    label = relative
  ) +
  geom_bar(stat = "identity", fill = "#A11D21") +
  geom_text(vjust = -0.5, size = 4) +
  labs(x = "Classe do automóvel", y = "Frequência") +
  theme_estat()
ggsave("colunas-uni-freq-percent.pdf", width = 158, height = 93, units = "mm")

### >>> 1.3.4 Bivariado com dodge ====
class_trans <- as.data.frame(table(mpg$class, mpg$trans))
ggplot(class_trans) +
  aes(x = Var1, y = Freq, fill = Var2) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(name = "Transmissão") +
  labs(x = "Classe do automóvel", y = "Frequência") +
  theme_estat()
ggsave("colunas-bi-dodge.pdf", width = 158, height = 93, units = "mm")

### >>> 1.3.5 Bivariado com stack ====
ggplot(class_trans, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(name = "Transmissão") +
  labs(x = "Classe do automóvel", y = "Frequência") +
  theme_estat()
ggsave("colunas-bi-stack.pdf", width = 158, height = 93, units = "mm")


### >>> 1.3.6 Bivariado com fill ====
ggplot(class_trans, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(name = "Transmissão") +
  labs(x = "Classe do automóvel", y = "Frequência") +
  theme_estat()
ggsave("colunas-bi-fill.pdf", width = 158, height = 93, units = "mm")

### >>> 1.3.7 Bivariado com porcentagem ====

trans_class <- table(mpg$trans, mpg$class) %>%
  data.frame() %>%
  mutate(Pct = Freq / sum(Freq))

orderclass <- c(
  "2seater", "compact", "midsize",
  "minivan", "pickup", "subcompact",
  "suv"
)

ggplot(trans_class) +
  aes(
    x = factor(Var2, level = orderclass),
    y = Pct,
    fill = Var1
  ) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(name = "Transmissão") +
  scale_y_continuous(
    limits = c(0, .15), expand = c(0, 0), breaks = seq(0, .3, .05),
    labels = paste0(seq(0, 30, 5), "%")
  ) +
  labs(x = "Classe do Automóvel", y = "Frequência Relativa") +
  theme_estat()
ggsave("colunas-bivariado-percent.pdf", width = 158, height = 93, units = "mm")

## >>> 1.4 Barras ====
# Basta adicionar coord_flip() nos códigos para Colunas

# > 2. Setores ====
## >> 2.1 Com porcentagem ====
contagem <- mpg %>%
  group_by(drv) %>%
  summarise(Freq = n()) %>%
  mutate(Prop = round(100 * (Freq / sum(Freq)), 2)) %>%
  arrange(desc(drv)) %>%
  mutate(posicao = cumsum(Prop) - 0.5 * Prop,
         ymax = cumsum(data$fraction),
         ymin = c(0, head(data$ymax, n=-1)))

ggplot(contagem) +
  aes(
    x = factor(""),
    y = Prop,
    fill = factor(drv)
  ) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  scale_x_discrete() +
  scale_fill_manual(name = "DRV") +
  theme_estat() +
  geom_text(
    aes(x = 1.8, y = posicao, label = paste0(Prop, "%")),
    color = "black"
  )
ggsave("setor.pdf", width = 158, height = 93, units = "mm")

# Gráfico de roskinha kkk ----

ggplot(contagem, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=drv)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) + 
  theme_estat() + 
  theme_void()

# > 3. Boxplot ====
## >> 3.1 Univariado ====
ggplot(mpg) +
  aes(
    x = factor(""),
    y = cty
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  guides(fill = FALSE) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "", y = "Consumo em Cidade (milhas/galão)") +
  theme_estat()
ggsave("box_uni.pdf", width = 158, height = 93, units = "mm")

## >> 3.2 Bivariado ====
ggplot(mpg) +
  aes(
    x = trans,
    y = cty
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Transmissão", y = "Consumo em Cidade (milhas/galão)") +
  theme_estat()
ggsave("box_bi.pdf", width = 158, height = 93, units = "mm")




# > 4. Histograma ====
## >> 4.1 Univariado ====
ggplot(mpg) +
  aes(x = cty) +
  geom_histogram(colour = "white", fill = "#A11D21", binwidth = 7) +
  labs(x = "Consumo em Cidade (milhas/galão)", y = "Frequência Absoluta") +
  theme_estat()
ggsave("hist_uni.pdf", width = 158, height = 93, units = "mm")

## >> 4.2 Univariado em porcentagem ====
ggplot(mpg) +
  aes(x = cty) +
  geom_histogram(
    aes(y = 100 * (..count..) / sum(..count..)),
    colour = "white",
    fill = "#A11D21",
    binwidth = 7
  ) +
  labs(x = "Consumo em Cidade (milhas/galão)", y = "Porcentagem") +
  theme_estat()
ggsave("hist_uni_porc.pdf", width = 158, height = 93, units = "mm")

## >> 4.3 Bivariado com facet grid ====
ggplot(mpg) +
  aes(x = cty) +
  geom_histogram(colour = "white", fill = "#A11D21", binwidth = 7) +
  facet_grid(. ~ class) +
  labs(x = "Consumo em Cidade (milhas/galão)", y = "Frequência") +
  theme_estat() +
  theme(
    strip.text = element_text(size = 12),
    strip.background = element_rect(colour = "black", fill = "white")
  )
ggsave("hist_grid.pdf", width = 200, height = 93, units = "mm")

# > 5. Dispersão ====
## >> 5.1 Univariado com poucos pontos sobrepostos ====
ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Consumo em Cidade (milhas/galão)",
    y = "Consumo em Rodovias (milhas/galão)"
  ) +
  theme_estat()

## >> 5.2 Univariado com muitos pontos sobrepostos ====
### >>> 5.2.1 geom_jitter ====
ggplot(mpg, aes(x = cyl, y = cty)) +
  geom_jitter(colour = "#A11D21", size = 3) +
  labs(
    x = "Consumo em Cidade (milhas/galão)",
    y = "Consumo em Rodovias (milhas/galão)"
  ) +
  theme_estat()

### >>> 5.2.2 Alpha ====
ggplot(mpg, aes(x = cyl, y = cty)) +
  geom_point(
    colour = "#A11D21",
    size = 3,
    alpha = 0.3
  ) +
  labs(
    x = "Consumo em Cidade (milhas/galão)",
    y = "Consumo em Rodovias (milhas/galão)"
  ) +
  theme_estat()

## >> 5.3 Bivariado ====
mpg$trans <- factor(substr(mpg$trans, 1, nchar(mpg$trans) - 4))

ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point(aes(colour = trans)) +
  scale_colour_manual(
    name = "Transmissão", values = c("#A11D21", "#003366"),
    labels = c("Automático", "Manual")
  ) +
  labs(
    x = "Consumo em Cidade (milhas/galão)",
    y = "Consumo em Rodovias (milhas/galão)"
  ) +
  theme_estat()

# > 6. Linhas ====
dados <- tibble(
  ano = c(
    "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014",
    "2015", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013",
    "2014", "2015"
  ),
  preco = c(
    2.5, 5.7, 3.4, 3.7, 4.5, 4.8, 4.1, 4.6, 4.8,
    5, 4.5, 6.5, 3.5, 4.6, 4.7, 4.9, 5, 5.5, 3.5,
    7.5
  ),
  produto = c(
    "a", "a", "a", "a", "a", "a", "a", "a", "b",
    "b", "b", "b", "b", "b", "b", "b", "b", "b",
    "b", "b"
  )
)

## >> 6.1 Univariado ====
ggplot(dados) +
  aes(x = ano, y = preco, group = 1) +
  geom_line(size = 1, colour = "#A11D21") +
  geom_point(colour = "#A11D21", size = 2) +
  labs(x = "Ano", y = "Preço") +
  theme_estat()

## >> 6.2 Bivariado ====
ggplot(dados) +
  aes(x = ano, y = preco, group = produto, colour = produto) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_colour_manual(name = "Produto", labels = c("A", "B")) +
  labs(x = "Ano", y = "Preço") +
  theme_estat()

