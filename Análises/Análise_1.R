######################
##### Importante ##### 
######################

# 1 - o banco de dados utilizado foi formatado no código formatacao.R
# 2 - O tema dos gráficos está no começo do código completo do github

######################################################
#### Analise 1 - Faturamento anual por categoria #####
######################################################

# Clonando o dataset para não aplicar erros aos dados originais

# Está sendo utilizado o datset sem as devoluções, pois os produtos devolvidos
# não entram no faturamento #

vendas_analise_1 <-vendas_sem_devolucao

# Listando as categorias

tipos_categorias <- unique(vendas_analise_1$Categoria)

print(tipos_categorias)

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

# Teste

# Testando a normalidade dos dados para implementar a ANOVA

categorias_analise_1 <- unique(vendas_analise_1$Categoria)

for (marca in Categorias_analise_1) {
  subset_data <- vendas_analise_2$Preço[vendas_analise_2$Marca == marca]
  result_ad_test <- ad.test(subset_data)
  print(paste("Teste Anderson-Darling para", marca))
  print(result_ad_test)
}

# Critério de normalidade satisfeito

#Anova para a média de preços entre as marcas

modelo_anova <- aov(Preço ~ Marca, data = vendas_analise_2)
resultado_anova <- summary(modelo_anova)

# Exibir os resultados da ANOVA
print(resultado_anova)

# Analisando as devoluções de produtos com defeito

# Usando o dataset completo

vendas_analise_1_parte_2 <-vendas_final

# Retirando NAS

vendas_analise_1_parte_2 <- subset(vendas_analise_1_parte_2, complete.cases(Categoria,`Motivo de devolução 2`,`Mês da compra`))

# Tipos de devoluções

tipos_devolucao <- unique(vendas_analise_1_parte_2$`Motivo de devolução 2`)

print(tipos_devolucao)

# Agrupando as devoluções

devolucoes_mensais_defeito <- vendas_analise_1_parte_2 %>%
  filter(`Motivo de devolução 2` == "Produto com defeito") %>%
  group_by(`Mês da compra`, Categoria) %>%
  summarise(Total_Devolucoes = n()) %>%
  ungroup()

xtable :: xtable(devolucoes_mensais_defeito)

# Gráfico de linhas multivariado para o total de devoluções por mês para cada motivo

ggplot(devolucoes_mensais_defeito) +
  aes(x = `Mês da compra`, y = Total_Devolucoes, group = Categoria, color = Categoria) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Mês", y = "Total de devoluções", color = "Categoria") +
  scale_y_continuous(limits = c(0, max(devolucoes_mensais_defeito$Total_Devolucoes) + 5), breaks = seq(0, max(devolucoes_mensais_defeito$Total_Devolucoes) + 5, by = 5)) +
  theme_estat()
ggsave("devolucoes_mensais_defeito_final.pdf", width = 158, height = 93, units = "mm")

# Somando todos os preço

soma_precos_defeito_por_categoria <- vendas_analise_1_parte_2 %>%
  filter(`Motivo de devolução 2` == "Produto com defeito") %>%
  group_by(Categoria) %>%
  summarise(Total_Preco = sum(Preço, na.rm = TRUE)) %>%
  ungroup()

# Exibindo a soma dos preços por categoria

print(soma_precos_defeito_por_categoria)

xtable :: xtable(soma_precos_defeito_por_categoria)

# Avaliando cada marca individualmente

# Retirando NAS

vendas_analise_1_parte_3_1 <- subset(vendas_analise_1, complete.cases(Categoria,Marca))

# Grafico de setories com todas as marcas

contagem <- vendas_analise_1_parte_3_1 %>%
  group_by(Categoria,Marca) %>%
  summarise (Freq = n()) %>%
  mutate(Prop = round(100*(Freq/sum(Freq)),2)) %>%
  arrange(desc(Marca)) %>%
  mutate(posicao = cumsum(Prop) - 0.5*Prop)

contagem %>%
  ggplot (aes(x = factor(""), y = Prop , fill = (Marca))) +
  geom_bar(width = 1, stat = "identity") +
  facet_wrap(~ Categoria) +
  coord_polar("y", start = 0) +
  geom_text(
    aes(x = 2, y = posicao , label = paste0(Prop , "%")),
    color = "black") +
  theme_void () +
  theme(legend.position = "top") +
  scale_fill_manual(values = cores_estat , name = 'Marca')
ggsave("setor_final_1.pdf", width = 180, height = 93, units = "mm")

# Gráficos de linhas multivariados individuais para cada marca

# Função para criar os gráficos

gerar_grafico <- function(data, nome_arquivo) {
  grafico <- ggplot(data) +
    aes(x = `Mês da compra`, y = Faturamento, group = Marca, color = Marca) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(x = "Mês", y = "Faturamento", color = "Marca") +
    scale_y_continuous(limits = c(0, max(data$Faturamento) + 100), breaks = seq(0, max(data$Faturamento) + 100, by = 100)) +
    theme_minimal()
  
  ggsave(nome_arquivo, plot = grafico, width = 158, height = 93, units = "mm")
}

# Gráficos de linhas multivariados para diferentes colunas: marca, cor e tamanho

# Listando das colunas de interesse

colunas_interesse <- c("Marca", "Cor", "Tamanho")

for (coluna in colunas_interesse) {
  # Filtrando dados sem valores ausentes para a coluna atual e Categoria
  dados_filtrados <- subset(vendas_analise_1, complete.cases(Categoria, {{coluna}}))
  
  # Agrupando o faturamento anual por coluna atual e Categoria
  categorias <- c("Women's Fashion", "Men's Fashion", "Kids' Fashion")
  
  for (cat in categorias) {
    dataset_name <- paste("faturamento_anual_", gsub("'", "", gsub(" ", "_", cat)), "_", gsub("'", "", gsub(" ", "_", coluna)), sep = "")
    assign(dataset_name, calcular_faturamento(dados_filtrados, cat, coluna))
  }
  
  # Criando lista de dataframes e nomes de arquivos para a coluna atual
  nomes_arquivos <- paste("faturamento_", gsub("'", "", gsub(" ", "_", coluna)), "_", gsub("'", "", gsub(" ", "_", coluna)), "_final.pdf", sep = "")
  nomes_arquivos <- rep(nomes_arquivos, length(categorias))
  dataframes <- mget(grep(paste("^faturamento_anual_", gsub("'", "", gsub(" ", "_", coluna)), "_", sep = ""), ls(), value = TRUE))
  
  # Iterando para criar os gráficos para a coluna atual
  for (i in seq_along(dataframes)) {
    gerar_grafico(dataframes[[i]], nomes_arquivos[i])
  }
}
