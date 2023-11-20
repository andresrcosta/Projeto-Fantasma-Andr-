######################################################
#### Analise 1 - Faturamento anual por categoria #####
######################################################

# Clonando o dataset para não aplicar erros aos dados originais

# Está sendo utilizado o datset sem as devoluções, pois os produtos devolvidos
# não entram no faturamento #

vendas_analise_1 <-vendas_sem_devolucao

# Retirando os NAS#

vendas_analise_1 <- subset(vendas_analise_1, complete.cases(`Data da venda`,Categoria,Preço))

# Garantido (novamente) que os valores da coluna preço sejam numericos #

vendas_analise_1$Preço <- as.numeric(vendas_analise_1$Preço)
vendas_analise_1$Preço

# Agrupando o faturamento anual por categoria
faturamento_anual <- vendas_analise_1 %>%
  group_by(Categoria) %>%
  summarise(Faturamento = sum(Preço))

# Gráfico de barras para o faturamento anual por categoria

ggplot(faturamento_anual, aes(x = Categoria, y = Faturamento)) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    aes(label = paste(Faturamento, " (", scales::percent(Faturamento / sum(Faturamento)), ")"),
        group = Categoria),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3
  ) +
  labs(x = "Categoria", y = "Faturamento") +
  scale_y_continuous(limits = c(0, 15000), breaks = seq(0, 15000, by = 5000)) +
  theme_estat()
ggsave("faturamento_anual_categoria.pdf", width = 158, height = 93, units = "mm")

# Agrupar o dataframe por Mês da compra e Categoria
# para calcular o faturamento mensal #

faturamento_mensal <- vendas_analise_1 %>%
  group_by(`Mês da compra`, Categoria) %>%
  summarise(Faturamento = sum(Preço))

# Gráfico de linhas multivariado para o fafturamento mensal por categoria

ggplot(faturamento_mensal) +
  aes(x = `Mês da compra`, y = Faturamento, group = Categoria, color = Categoria) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Mês", y = "Faturamento", color = "Categoria") +
  scale_y_continuous(limits = c(0, 3000), breaks = seq(0, 3000, by = 500)) +
  theme_estat()
ggsave("faturamento_mensal_categoria.pdf", width = 158, height = 93, units = "mm")

# Quadro de medidas resumo

quadro_resumo_analise_1 <- faturamento_mensal %>%
  group_by( Categoria ) %>% # caso mais de uma categoria
  summarize (Média = round(mean(Faturamento),2),
             `Desvio Padrão ` = round(sd(Faturamento),2),
             `Mínimo ` = round(min(Faturamento),2),
             `1º Quartil ` = round(quantile(Faturamento , probs = .25),2),
             Mediana = round(quantile(Faturamento , probs = .5),2),
             `3º Quartil ` = round(quantile(Faturamento , probs = .75),2),
             `Máximo ` = round(max(Faturamento),2)) %>% t() %>% as.data.frame()

xtable :: xtable(quadro_resumo_analise_1)