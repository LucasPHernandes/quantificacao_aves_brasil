
# ANÁLISE DOS DADOS

install.packages("ggthemes")
library(ggthemes)
# Junta as linhas das tabelas selecionadas (precisa haver correspondência de nomes de colunas)
tabela_analise <- bind_rows(SP, SC_2009, SC_2011, RS_2002, RS_2014, RJ, PR_2004, PR_2018, PA, MG, ES_2005, ES_2022, BA)
view(tabela_analise)

plot_n_especiesAmeacadas <- bind_rows(SP, SC_2011, RS_2014, RJ, PR_2018, PA, MG, ES_2022, BA, lista_nacional, lista_global)

plot_n_especiesAmeacadas <- plot_n_especiesAmeacadas %>%
  filter(CATEGORIA_AMEACA == "CR" | CATEGORIA_AMEACA == "VU" | CATEGORIA_AMEACA == "EN" | CATEGORIA_AMEACA == "CR(RE)" | CATEGORIA_AMEACA == "CR-PEW" | CATEGORIA_AMEACA == "CR(PE)" | CATEGORIA_AMEACA == "CR (PEX)")

plot_n_especiesAmeacadas$CATEGORIA_AMEACA[plot_n_especiesAmeacadas$CATEGORIA_AMEACA == "CR(RE)"] = "CR"
plot_n_especiesAmeacadas$CATEGORIA_AMEACA[plot_n_especiesAmeacadas$CATEGORIA_AMEACA == "CR (PE)"] = "CR"
plot_n_especiesAmeacadas$CATEGORIA_AMEACA[plot_n_especiesAmeacadas$CATEGORIA_AMEACA == "CR(PE)"] = "CR"
plot_n_especiesAmeacadas$CATEGORIA_AMEACA[plot_n_especiesAmeacadas$CATEGORIA_AMEACA == "CR-PEW"] = "CR"
plot_n_especiesAmeacadas$CATEGORIA_AMEACA[plot_n_especiesAmeacadas$CATEGORIA_AMEACA == "CR (PEX)"] = "CR"

plot_n_especiesAmeacadas$CATEGORIA_AMEACA[plot_n_especiesAmeacadas$CATEGORIA_AMEACA == "CR"] = "Criticamente em Perigo (CR)"
plot_n_especiesAmeacadas$CATEGORIA_AMEACA[plot_n_especiesAmeacadas$CATEGORIA_AMEACA == "EN"] = "Em Perigo (EN)"
plot_n_especiesAmeacadas$CATEGORIA_AMEACA[plot_n_especiesAmeacadas$CATEGORIA_AMEACA == "VU"] = "Vulnerável (VU)"

quantitativo <- as.data.frame(table(plot_n_especiesAmeacadas$CATEGORIA_AMEACA, plot_n_especiesAmeacadas$NIVEL)) 



quantitativo <- rename(quantitativo, Categoria = Var1, Nivel = Var2)

plot_n_especiesAmeacadas$NIVEL <- factor(plot_n_especiesAmeacadas$NIVEL, levels = c("Global", "Brasil", "Espírito Santo", "Paraná", "Minas Gerais", "Santa Catarina", "Rio Grande do Sul", "Bahia", "São Paulo", "Rio de Janeiro","Pará"))
plot_n_especiesAmeacadas$NIVEL <- factor(plot_n_especiesAmeacadas$NIVEL, levels = c("Pará", "Rio de Janeiro", "São Paulo", "Bahia", "Rio Grande do Sul", "Santa Catarina", "Minas Gerais", "Paraná", "Espírito Santo", "Brasil", "Global"))

# Plot qnt espécies barra empilhada por categoria de ameaça
plot_n_especiesAmeacadas %>%
  group_by(NIVEL, CATEGORIA_AMEACA) %>%
  summarise(
    contgem = n()
  ) %>%
  ggplot(aes(x = NIVEL, y = contgem, fill = CATEGORIA_AMEACA, label = contgem)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(
    title = "Quantidade de espécies de aves do Brasil ameaçadas de extinção",
    subtitle = "Escala global, nacional e estadual",
    fill = "Categoria de ameaça",
  ) +
  geom_text(position = position_stack(vjust = 0.5)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.background = element_rect(fill = NA),
    panel.grid.major.x = element_line(colour = "#c2c2c2", linetype = "dashed"),
    axis.ticks.x = element_line(colour = "#c2c2c2", linetype = "dashed"),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 11, margin = margin(0, 5, 0, 0), color = "black"),
    legend.position = c(0.84, 0.15)
  ) +
  scale_fill_manual(values = plotCores)+
  coord_flip()


# OUTRA GRÁFICO DE BARRAS (ESPÉCIES AMEAÇADAS)

plot_n_especiesAmeacadas %>%
  group_by(NIVEL, CATEGORIA_AMEACA) %>%
  summarise(
    contgem = n()
  ) %>%
  ggplot(aes(x = NIVEL, y = contgem, fill = CATEGORIA_AMEACA, label = contgem)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(
    title = "Quantidade de espécies de aves do Brasil ameaçadas de extinção",
    subtitle = "Escala global, nacional e estadual",
    fill = "Categoria de ameaça",
  ) +
  geom_text(position = position_stack(vjust = 0.5)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.background = element_rect(fill = NA),
    panel.grid.major.x = element_line(colour = "#c2c2c2", linetype = "dashed"),
    axis.ticks.x = element_line(colour = "#c2c2c2", linetype = "dashed"),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 11, margin = margin(0, 5, 0, 0), color = "black"),
    legend.position = 'none'
  ) +
  scale_fill_manual(values = plotCores)+
  coord_flip() +
  facet_wrap(vars(CATEGORIA_AMEACA))




# Diagrama de Venn

install.packages("VennDiagram")

library(VennDiagram)

plot_n_especiesAmeacadas %>%
  group_by(NIVEL, CBRO_2021)

tabela_venn <- bind_rows(SP, SC_2011, RS_2014, RJ, PR_2018, PA, MG, ES_2022, BA, lista_nacional, lista_global)
tabela_venn <- tabela_venn %>%
  filter(CATEGORIA_AMEACA == "CR" | CATEGORIA_AMEACA == "VU" | CATEGORIA_AMEACA == "EN" | CATEGORIA_AMEACA == "CR(RE)" | CATEGORIA_AMEACA == "CR-PEW" | CATEGORIA_AMEACA == "CR(PE)" | CATEGORIA_AMEACA == "CR (PEX)")

tabela_venn$CATEGORIA_AMEACA[tabela_venn$CATEGORIA_AMEACA == "CR(RE)"] = "CR"
tabela_venn$CATEGORIA_AMEACA[tabela_venn$CATEGORIA_AMEACA == "CR (PE)"] = "CR"
tabela_venn$CATEGORIA_AMEACA[tabela_venn$CATEGORIA_AMEACA == "CR(PE)"] = "CR"
tabela_venn$CATEGORIA_AMEACA[tabela_venn$CATEGORIA_AMEACA == "CR-PEW"] = "CR"
tabela_venn$CATEGORIA_AMEACA[tabela_venn$CATEGORIA_AMEACA == "CR (PEX)"] = "CR"

tabela_venn <- select(tabela_venn, CBRO_2021, NIVEL, CATEGORIA_AMEACA)

tabela_venn_estadual <- tabela_venn %>%
  filter(NIVEL == "São Paulo" | NIVEL == "Santa Catarina" | NIVEL == "Rio Grande do Sul" | NIVEL == "Rio de Janeiro" | NIVEL == "Paraná" | NIVEL == "Pará" | NIVEL == "Minas Gerais" | NIVEL == "Espírito Santo" | NIVEL == "Bahia" )

tabela_venn_estadual <- select(tabela_venn_estadual, CBRO_2021)


tabela_venn_global <- tabela_venn %>%
  filter(NIVEL == "Global")

tabela_venn_global <- select(tabela_venn_global, CBRO_2021)

tabela_venn_nacional <- tabela_venn %>%
  filter(NIVEL == "Brasil")

tabela_venn_nacional <- select(tabela_venn_nacional, CBRO_2021)

tabela_venn_estadual <- unique(tabela_venn_estadual)

tabela_venn_estadual <- distinct(tabela_venn_estadual, CBRO_2021)
tabela_venn_global <- distinct(tabela_venn_global, CBRO_2021)
tabela_venn_nacional <- distinct(tabela_venn_nacional, CBRO_2021)


tabela_totalizacao <- bind_rows(tabela_venn_estadual, tabela_venn_global, tabela_venn_nacional)
tabela_totalizacao <- distinct(tabela_totalizacao, CBRO_2021)


# INICIO PLOTAGEM DIAGRAM DE VENN
teste_nacional <- array(teste_nacional, dim = c(189, 1, 1))
teste_nacional <- NULL
i = 0
while (i < 190) {
  teste_nacional[i] <- "nopeN"
  i <- i + 1
}


teste_global <- array(teste_global, dim = c(292, 1, 1))
teste_global <- NULL
i = 0
while (i < 292) {
  teste_global <- data.frame(CBRO_2021="nopeG")
  i <- i + 1
}

teste_global <- as.data.frame(teste_global)
teste_global <- rename(teste_global, CBRO_2021 = V1)



tabGlobal <- rbind(tabela_venn_global, teste_global)

tabGlobal$CBRO_2021[tabGlobal$CBRO_2021 == "nopeG"] = NA




teste_nacional <- as.data.frame(teste_nacional)
teste_nacional <- rename(teste_nacional, CBRO_2021 = V1)


tabN_novaLinha <- data.frame(CBRO_2021="npeN")
tabNaciona <- rbind(tabela_venn_nacional, teste_nacional)
tabNaciona <- rbind(tabNaciona, tabN_novaLinha)


tabNaciona$CBRO_2021[tabNaciona$CBRO_2021 == "npeN"] = NA
tabNaciona$CBRO_2021[tabNaciona$CBRO_2021 == "nopeN"] = NA


teste_global <- array(teste_global, dim = c(292, 1, 1))
i = 0
while (i < 292) {
  teste_global[i] <- "nopeG"
  i <- i + 1
}

teste_global <- as.data.frame(teste_global)
teste_global <- rename(teste_global, CBRO_2021 = V1)

tabGlobal <- rbind(tabela_venn_global, teste_global)

tabGlobal$CBRO_2021[tabGlobal$CBRO_2021 == "nopeG"] = NA

install.packages("ggvenn")
install.packages("gplots")
library(ggvenn)

library(gplots)

newTab <- bind_cols(tabela_venn_estadual, tabNaciona, tabGlobal)
newTab <- rename(newTab, Estadual = CBRO_2021...1, Nacional = CBRO_2021...2, Global = CBRO_2021...3)


newTab <- distinct(newTab, Nacional, Global, Estadual)

newTab <- as.list(newTab)

# create venn diagram and display all the sets
ggvenn(newTab, c('Estadual', 'Nacional', 'Global'))



filter(newTab, newTab$Estadual %in% c(newTab$Nacional) & newTab$Estadual %in% c(newTab$Global))








# NÚMERO DE ESPÉCIES AMEAÇADAS AUMENTOU OU DIMINUIU?




ES_2005_anal3 <- select(ES_2005, CBRO_2021, CATEGORIA_AMEACA)
ES_2005_anal3 <- ES_2005_anal3 %>%
  filter(CATEGORIA_AMEACA == "CR" | CATEGORIA_AMEACA == "VU" | CATEGORIA_AMEACA == "EN" | CATEGORIA_AMEACA == "CR(RE)" | CATEGORIA_AMEACA == "CR-PEW" | CATEGORIA_AMEACA == "CR(PE)" | CATEGORIA_AMEACA == "CR (PEX)")

summarise(
  ES_2005_anal3,
  cont = n()
)


ES_2022_anal3 <- select(ES_2022, CBRO_2021, CATEGORIA_AMEACA)









tabela_anual_ES <- filter(tabela_analise, NIVEL == "Espírito Santo")

tabela_anual_ES$CATEGORIA_AMEACA[tabela_anual_ES$CATEGORIA_AMEACA == "CR(RE)"] = "CR"
tabela_anual_ES$CATEGORIA_AMEACA[tabela_anual_ES$CATEGORIA_AMEACA == "CR (PE)"] = "CR"
tabela_anual_ES$CATEGORIA_AMEACA[tabela_anual_ES$CATEGORIA_AMEACA == "CR(PE)"] = "CR"
tabela_anual_ES$CATEGORIA_AMEACA[tabela_anual_ES$CATEGORIA_AMEACA == "CR-PEW"] = "CR"
tabela_anual_ES$CATEGORIA_AMEACA[tabela_anual_ES$CATEGORIA_AMEACA == "CR (PEX)"] = "CR"


tabela_anual_ES <- tabela_anual_ES %>%
  filter(CATEGORIA_AMEACA == "CR" | CATEGORIA_AMEACA == "VU" | CATEGORIA_AMEACA == "EN" | CATEGORIA_AMEACA == "CR(RE)" | CATEGORIA_AMEACA == "CR-PEW" | CATEGORIA_AMEACA == "CR(PE)" | CATEGORIA_AMEACA == "CR (PEX)")

tabela_anual_ES <- tabela_anual_ES %>%
  select(CBRO_2021, CATEGORIA_AMEACA,NIVEL, ANO)



tabela_anual_RS <- filter(tabela_analise, NIVEL == "Rio Grande do Sul")

tabela_anual_RS$CATEGORIA_AMEACA[tabela_anual_RS$CATEGORIA_AMEACA == "CR(RE)"] = "CR"
tabela_anual_RS$CATEGORIA_AMEACA[tabela_anual_RS$CATEGORIA_AMEACA == "CR (PE)"] = "CR"
tabela_anual_RS$CATEGORIA_AMEACA[tabela_anual_RS$CATEGORIA_AMEACA == "CR(PE)"] = "CR"
tabela_anual_RS$CATEGORIA_AMEACA[tabela_anual_RS$CATEGORIA_AMEACA == "CR PE"] = "CR"
tabela_anual_RS$CATEGORIA_AMEACA[tabela_anual_RS$CATEGORIA_AMEACA == "CR-PEW"] = "CR"
tabela_anual_RS$CATEGORIA_AMEACA[tabela_anual_RS$CATEGORIA_AMEACA == "CR (PEX)"] = "CR"


tabela_anual_RS <- tabela_anual_RS %>%
  filter(CATEGORIA_AMEACA == "CR" | CATEGORIA_AMEACA == "VU" | CATEGORIA_AMEACA == "EN" | CATEGORIA_AMEACA == "CR(RE)" | CATEGORIA_AMEACA == "CR-PEW" | CATEGORIA_AMEACA == "CR(PE)" | CATEGORIA_AMEACA == "CR (PEX)")

tabela_anual_RS <- tabela_anual_RS %>%
  select(CBRO_2021, CATEGORIA_AMEACA,NIVEL, ANO)



tabela_anual_SC <- filter(tabela_analise, NIVEL == "Santa Catarina")

tabela_anual_SC$CATEGORIA_AMEACA[tabela_anual_SC$CATEGORIA_AMEACA == "CR (PE)"] = "CR"
tabela_anual_SC$CATEGORIA_AMEACA[tabela_anual_SC$CATEGORIA_AMEACA == "CR(PE)"] = "CR"
tabela_anual_SC$CATEGORIA_AMEACA[tabela_anual_SC$CATEGORIA_AMEACA == "CR-PEW"] = "CR"
tabela_anual_SC$CATEGORIA_AMEACA[tabela_anual_SC$CATEGORIA_AMEACA == "CR (PEX)"] = "CR"

tabela_anual_SC$CATEGORIA_AMEACA <- str_trim(tabela_anual_SC$CATEGORIA_AMEACA)


tabela_anual_SC <- tabela_anual_SC %>%
  filter(CATEGORIA_AMEACA == "CR" | CATEGORIA_AMEACA == "VU" | CATEGORIA_AMEACA == "EN" | CATEGORIA_AMEACA == "CR-PEW" | CATEGORIA_AMEACA == "CR(PE)" | CATEGORIA_AMEACA == "CR (PEX)")

tabela_anual_SC <- tabela_anual_SC %>%
  select(CBRO_2021, CATEGORIA_AMEACA,NIVEL, ANO)



tabela_anual_PR <- filter(tabela_analise, NIVEL == "Paraná")


tabela_anual_PR$CATEGORIA_AMEACA[tabela_anual_PR$CATEGORIA_AMEACA == "CR (PE)"] = "CR"
tabela_anual_PR$CATEGORIA_AMEACA[tabela_anual_PR$CATEGORIA_AMEACA == "CR(PE)"] = "CR"
tabela_anual_PR$CATEGORIA_AMEACA[tabela_anual_PR$CATEGORIA_AMEACA == "CR-PEW"] = "CR"
tabela_anual_PR$CATEGORIA_AMEACA[tabela_anual_PR$CATEGORIA_AMEACA == "CR (PEX)"] = "CR"

tabela_anual_PR$CATEGORIA_AMEACA[tabela_anual_PR$CATEGORIA_AMEACA == "EM"] = "EN"


tabela_anual_PR <- tabela_anual_PR %>%
  filter(CATEGORIA_AMEACA == "CR" | CATEGORIA_AMEACA == "VU" | CATEGORIA_AMEACA == "EN" | CATEGORIA_AMEACA == "CR-PEW" | CATEGORIA_AMEACA == "CR(PE)" | CATEGORIA_AMEACA == "CR (PEX)")


tabela_anual_PR <- tabela_anual_PR %>%
  select(CBRO_2021, CATEGORIA_AMEACA,NIVEL, ANO)


tabela_anual <- bind_rows(tabela_anual_ES, tabela_anual_PR, tabela_anual_RS, tabela_anual_SC)

















tabela_anual %>%
  ggplot(aes(x = ANO, y = contgem, group=NIVEL, color = factor(NIVEL))) +
  scale_x_continuous(breaks = c(2002, 2004, 2005, 2009, 2011, 2014, 2018, 2019, 2022))+
  geom_line(size = 2, alpha = 0.5)+
  geom_point(size = 3)+
  geom_label(aes(label = contgem), size = 4, label.size = 0.01)+
  labs(
    title = "Diferença no número de espécies ameaçadas entre anos de avaliação",
    color = "Níveis: "
  )+
  theme_fivethirtyeight()+
  theme(
    panel.grid.major.y = element_blank(),
    panel.background = element_rect(fill = NA),
    panel.grid.major.x = element_line(colour = "#c2c2c2", linetype = "dashed"),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
  )




CBRO = read.csv("cbro_nomes.tsv", sep = "\t")




lista_nacional_2014 <- read.csv("nacional_2014.tsv", sep = "\t", encoding = "UTF-8", stringsAsFactors = FALSE)
lista_nacional_2014$TAXON <- gsub("\\s*\\([^\\)]+\\)","",as.character(lista_nacional_2014$Nome.cientifico))

lista_nacional_2014$CBRO_2021 <- CBRO$CBRO[match(lista_nacional_2014$TAXON, CBRO$Nomes)]
lista_nacional_2014$CATEGORIA_AMEACA <- lista_nacional_2014$Categoria.de.Ameaca
lista_nacional_2014$NIVEL <- "Brasil"
lista_nacional_2014$ANO <- 2014

lista_nacional_2014 <- lista_nacional_2014 %>%
  select(TAXON, CBRO_2021, CATEGORIA_AMEACA, ANO, NIVEL)

lista_nacional <- lista_nacional %>%
  select(TAXON, CBRO_2021, CATEGORIA_AMEACA, ANO, NIVEL)


lista_nacional$CATEGORIA_AMEACA[lista_nacional$CATEGORIA_AMEACA == "CR (PE)"] = "CR"
lista_nacional$CATEGORIA_AMEACA[lista_nacional$CATEGORIA_AMEACA == "CR(PE)"] = "CR"
lista_nacional$CATEGORIA_AMEACA[lista_nacional$CATEGORIA_AMEACA == "CR-PEW"] = "CR"
lista_nacional$CATEGORIA_AMEACA[lista_nacional$CATEGORIA_AMEACA == "CR (PEX)"] = "CR"
lista_nacional$CATEGORIA_AMEACA[lista_nacional$CATEGORIA_AMEACA == "CR (PEw)"] = "CR"


lista_nacional <- lista_nacional %>%
  filter(CATEGORIA_AMEACA == "CR" | CATEGORIA_AMEACA == "VU" | CATEGORIA_AMEACA == "EN")





tabela_anual$CATEGORIA_AMEACA[tabela_anual$CATEGORIA_AMEACA == "CR (PE)"] = "CR"
tabela_anual$CATEGORIA_AMEACA[tabela_anual$CATEGORIA_AMEACA == "CR(PE)"] = "CR"
tabela_anual$CATEGORIA_AMEACA[tabela_anual$CATEGORIA_AMEACA == "CR-PEW"] = "CR"
tabela_anual$CATEGORIA_AMEACA[tabela_anual$CATEGORIA_AMEACA == "CR (PEX)"] = "CR"
tabela_anual$CATEGORIA_AMEACA[tabela_anual$CATEGORIA_AMEACA == "CR (PEw)"] = "CR"


lista_nacional_2014$CATEGORIA_AMEACA[lista_nacional_2014$CATEGORIA_AMEACA == "CR (PE)"] = "CR"
lista_nacional_2014$CATEGORIA_AMEACA[lista_nacional_2014$CATEGORIA_AMEACA == "CR(PE)"] = "CR"
lista_nacional_2014$CATEGORIA_AMEACA[lista_nacional_2014$CATEGORIA_AMEACA == "CR-PEW"] = "CR"
lista_nacional_2014$CATEGORIA_AMEACA[lista_nacional_2014$CATEGORIA_AMEACA == "CR (PEX)"] = "CR"
lista_nacional_2014$CATEGORIA_AMEACA[lista_nacional_2014$CATEGORIA_AMEACA == "CR (PEW)"] = "CR"

MG_95 <- read.csv("MG_95.txt", sep = "\t", encoding = "UTF-8", stringsAsFactors = FALSE)

MG_95 <- rename(MG_95, CBRO_2021 = scientificNameCBRO2021, CATEGORIA_AMEACA=category, NIVEL = scale, ANO = year)


MG_95 <- MG_95 %>%
  filter(CATEGORIA_AMEACA == "CR" | CATEGORIA_AMEACA == "VU" | CATEGORIA_AMEACA == "EN" | CATEGORIA_AMEACA == "CR(RE)" | CATEGORIA_AMEACA == "CR-PEW" | CATEGORIA_AMEACA == "CR(PE)" | CATEGORIA_AMEACA == "CR (PEX)")

MG_95$NIVEL <- "Minas Gerais"

MG_95 <- MG_95 %>%
  select(CBRO_2021, CATEGORIA_AMEACA,NIVEL, ANO)

MG <- MG %>%
  select(CBRO_2021, CATEGORIA_AMEACA,NIVEL, ANO)

tabela_anual_MG <- rbind(MG, MG_95)


lista_nacional_2014$TEMP <- lista_nacional_2014$Nome.cientifico

lista_nacional_2014$TAXON <- gsub("\\s*\\([^\\)]+\\)","",as.character(lista_nacional_2014$TEMP)) 
lista_nacional_2014$TEMP <- NULL
lista_nacional_2014$CBRO_2021 <- names_CBRO$CBRO[match(lista_nacional_2014$TAXON, names_CBRO$Nomes)]
lista_nacional_2014$CATEGORIA_AMEACA <- lista_nacional_2014$Categoria.de.Ameaca
lista_nacional_2014$ANO <- 2014

lista_nacional_2014 <- lista_nacional_2014 %>%
  select(CBRO_2021, CATEGORIA_AMEACA, NIVEL, ANO)



lista_nacional <- lista_nacional %>%
  select(CBRO_2021, CATEGORIA_AMEACA, NIVEL, ANO)

lista_nacional$NIVEL <- "Brasil"
lista_nacional_nova <- rbind(lista_nacional, lista_nacional_2014)

lista_nacional_nova$TAXON[lista_nacional_nova$TAXON == "Pyrrhura lepida lepida"] = NA

lista_nacional_nova <- slice(lista_nacional_nova, -350)





SP_2010$NIVEL <- "São Paulo"
SP_2010$ANO <- 2010

SP_2010 <- SP_2010 %>%
  select(CBRO_2021, CATEGORIA_AMEACA, NIVEL, ANO)

SP$NIVEL <- "São Paulo"
SP$ANO <- 2018

SP <- SP %>%
  select(CBRO_2021, CATEGORIA_AMEACA, NIVEL, ANO)


tabela_anual_SP <- rbind(SP, SP_2010)






tabela_anual <- bind_rows(tabela_anual_ES, tabela_anual_MG, tabela_anual_PR, tabela_anual_RS, tabela_anual_SP, lista_nacional_nova)




tabela_anual <- tabela_anual %>%
  filter(CATEGORIA_AMEACA == "CR" | CATEGORIA_AMEACA == "VU" | CATEGORIA_AMEACA == "EN" | CATEGORIA_AMEACA == "CR-PEW" | CATEGORIA_AMEACA == "CR(PE)" | CATEGORIA_AMEACA == "CR (PEX)" | CATEGORIA_AMEACA == "CR (PEW)")




tabela_anual <- tabela_anual %>%
  group_by(ANO, NIVEL) %>%
  summarise(
    contgem = n()
  )


tabela_anual %>%
  ggplot(aes(x = ANO, y = contgem, group=NIVEL, color = factor(NIVEL))) +
  scale_x_continuous(breaks = c(1995, 2002, 2004, 2005, 2009, 2010, 2011, 2014, 2018, 2019, 2022))+
  geom_line(size = 2, alpha = 0.5)+
  geom_point(size = 3)+
  geom_label(aes(label = contgem), size = 4, label.size = 0.01)+
  labs(
    title = "Diferença no número de espécies ameaçadas entre anos de avaliação",
    color = "Níveis: "
  )+
  theme_fivethirtyeight()+
  theme(
    panel.grid.major.y = element_blank(),
    panel.background = element_rect(fill = NA),
    panel.grid.major.x = element_line(colour = "#c2c2c2", linetype = "dashed"),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
  )






# OUTRAS COISAS

SP_2010 <- read.csv("SP_2010.tsv", sep = "\t", encoding = "UTF-8", stringsAsFactors = FALSE)


SP_2010$TEMP <- SP_2010$Táxon

SP_2010$TAXON <- gsub("\\s*\\([^\\)]+\\)","",as.character(SP_2010$TEMP)) 
SP_2010$TEMP <- NULL
SP_2010$CBRO_2021 <- names_CBRO$CBRO[match(SP_2010$TAXON, names_CBRO$Nomes)]
SP_2010$CATEGORIA_AMEACA <- SP_2010$Categoria
SP_2010$ANO <- 2014

SP_2010 <- SP_2010 %>%
  select(CBRO_2021, CATEGORIA_AMEACA, ANO)




SP <- SP %>%
  select(CBRO_2021, NIVEL, CATEGORIA_AMEACA)

SP$CBRO_2021 <- names_CBRO$CBRO[match(SP$TAXON, names_CBRO$Nomes)]


tabela_venn_estadual <- bind_rows(tabela_venn, SP)










# Carrega os pacotes
library(VennDiagram)
library(readxl)

# Importa a lista de especies ameacadas do Brasil (2022)
br2022 = as.data.frame(read_xlsx("listas_brasil_aves.xlsx", sheet = 2))

# Remove especies extintas
br2022 = br2022[-which(br2022$category == "RE" |
                         br2022$category == "EW" |
                         br2022$category == "EX"), ]

# Importa a lista de especies ameacadas da BirdLife (2022)
bl2022 = as.data.frame(read_xlsx("listas_brasil_aves.xlsx", sheet = 7))

# Remove especies quase ameacadas, extintas e deficiente de dados
bl2022 = bl2022[-which(bl2022$category == "NT" | 
                         bl2022$category == "EW" |
                         bl2022$category == "EX" |
                         bl2022$category == "DD"), ]

# Importa a lista de especies ameacadas de MG (1995)
mg1995 = as.data.frame(read_xlsx("listas_brasil_aves.xlsx", sheet = 9))

# Prepare a palette of 3 colors with R colorbrewer:
myCol <- c("#b3e2cd", "#fdcdac", "#cbd5e8")

# Chart
venn.diagram(
  x = list(br2022$scientificNameCBRO2021,
           bl2022$scientificNameCBRO2021,
           tabela_venn_estadual$CBRO_2021),
  category.names = c("National" , "Global" , "State"),
  filename = 'especies_ameacadas_escalas.png',
  output = TRUE,
  
  # Output features
  imagetype = "png",
  height = 480*3, 
  width = 480*3, 
  resolution = 900,
  compression = "lzw",
  
  # Circles
  lwd = 2,
  lty = 'blank',
  fill = myCol,
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  
  # Set names
  cat.cex = 0.6,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.pos = c(-27, 27, 135),
  cat.dist = c(0.055, 0.055, 0.085),
  cat.fontfamily = "sans",
  rotation = 1)






url_base = "https://www.wikiaves.com.br/wiki/"
nome_popular = "papagaio-charao"
url = paste0(url_base, nome_popular)

p1 = readLines(url)
p2 = p1[grepl("(IUCN 3.1)", p1)]
p3 = word(p2, 2, sep = "0.9em\"><b>")
p4 = word(p3, 1, sep = "</b><img src")
p4






SP_prof <- read.csv("teste/listas_SP.txt", sep = "\t", encoding = "UTF-8", stringsAsFactors = FALSE)
SP_EU <- read.csv("teste/SP_2018.tsv", sep = "\t", encoding = "UTF-8", stringsAsFactors = FALSE)




SP_EU <- select(SP_EU, Táxon)
SP_prof <- select(SP_prof, scientificNameCBRO2021)
SP_prof <- rename(SP_prof, Táxon = scientificNameCBRO2021)

Sp_new <- bind_rows(SP_EU,SP_prof)


Sp_new <- unique(Sp_new)










