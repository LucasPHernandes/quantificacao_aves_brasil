
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

quantitativo <- as.data.frame(table(plot_n_especiesAmeacadas$CATEGORIA_AMEACA, plot_n_especiesAmeacadas$NIVEL)) 

qnt_BA <- quantitativo %>%
  filter(Var2 == "Bahia")

qnt_BA <- sum(qnt_BA$Freq)

qnt_ES <- quantitativo %>%
  filter(Var2 == "Espírito Santo")

qnt_ES <-  sum(qnt_ES$Freq)

qnt_MG <- quantitativo %>%
  filter(Var2 == "Minas Gerais")

qnt_MG <-  sum(qnt_MG$Freq)

qnt_PA <- quantitativo %>%
  filter(Var2 == "Pará")

qnt_PA <-  sum(qnt_PA$Freq)

qnt_PR <- quantitativo %>%
  filter(Var2 == "Paraná")

qnt_PR <-  sum(qnt_PR$Freq)

qnt_RJ <- quantitativo %>%
  filter(Var2 == "Rio de Janeiro")

qnt_RJ <-  sum(qnt_RJ$Freq)

qnt_RS <- quantitativo %>%
  filter(Var2 == "Rio Grande do Sul")

qnt_RS <-  sum(qnt_RS$Freq)

qnt_SC <- quantitativo %>%
  filter(Var2 == "Santa Catarina")

qnt_SC <-  sum(qnt_SC$Freq)

qnt_SP <- quantitativo %>%
  filter(Var2 == "São Paulo")

qnt_SP <-  sum(qnt_SP$Freq)

qnt_GLOBAL <- quantitativo %>%
  filter(Var2 == "Global")

qnt_GLOBAL <-  sum(qnt_GLOBAL$Freq)

qnt_NACIONAL <- quantitativo %>%
  filter(Var2 == "Brasil")

qnt_NACIONAL <-  sum(qnt_NACIONAL$Freq)

qnts$Estado <- "Bahia"
qnts$Freq <- qnt_BA

qnts <- add_row(qnts, Estado="Espírito Santo", Freq=qnt_ES)
qnts <- add_row(qnts, Estado="Minas Gerais", Freq=qnt_MG)
qnts <- add_row(qnts, Estado="Pará", Freq=qnt_PA)
qnts <- add_row(qnts, Estado="Paraná", Freq=qnt_PR)
qnts <- add_row(qnts, Estado="Rio de Janeiro", Freq=qnt_RJ)
qnts <- add_row(qnts, Estado="Rio Grande do Sul", Freq=qnt_RS)
qnts <- add_row(qnts, Estado="Santa Catarina", Freq=qnt_SC)
qnts <- add_row(qnts, Estado="São Paulo", Freq=qnt_SP)
qnts <- add_row(qnts, Estado="Global", Freq=qnt_GLOBAL)
qnts <- add_row(qnts, Estado="Brasil", Freq=qnt_NACIONAL)


qnts <- mutate(qnts, Estado = fct_reorder(Estado, Freq, .desc = TRUE))

plotCores <- c("#D62D2D", "#F48263", "#EBD360") # Cria própria paleta de cor
# Plotagem gráfico de barras
ggplot(qnts, aes(x=Estado, y=Freq))+
  geom_col(aes(fill = Estado)) +
  labs(
    title = "Quantificação de espécies ameçadas por estado",
    subtitle = "Classificação CR,VU e EN",
    x = "Estados",
    y = "Quantidade de espécies ameaçadas"
  ) +
  theme_solarized() +
  theme(axis.text.x = element_text(size = 9)) +
  scale_colour_solarized() +
  scale_fill_manual(values = plotCores)


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
  geom_bar(stat = "identity") +
  labs(
    title = "Quantificação de espécies ameçadas por nível de análise",
    subtitle = "Classificação CR,VU e EN",
    x = "Nivel de análise",
    y = "Quantidade de espécies ameaçadas"
  ) +
  geom_text(position = position_stack(vjust = 0.5)) +
  theme_solarized() +
  scale_colour_solarized() +
  scale_fill_manual(values = plotCores) 



