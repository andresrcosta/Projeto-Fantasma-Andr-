######################################################
#### Analise 1 - Faturamento anual por categoria #####
######################################################

# Pacotes (olhar o código completo)

# Está sendo utilizado o datset sem as devoluções, pois os produtos devolvidos
# não entram no faturamento #

# Clonando o dataset para não aplicar erros aos dados originais

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

# Análise das marcas, cores e tamanhos inidividualmente

# Retirando NAS

vendas_analise_1_marcas_parte_setores <- subset(vendas_analise_1, complete.cases(Categoria,Marca))

# Grafico de setores para categoria de marca, cor e tamanho

# Função para criar contagem e gráfico de barras para Marca, Cor ou Tamanho

gerar_contagem_grafico <- function(dataset, variavel) {
  contagem <- dataset %>%
    group_by(Categoria, !!sym(variavel)) %>%
    summarise(Freq = n()) %>%
    mutate(Prop = round(100 * (Freq / sum(Freq)), 2)) %>%
    arrange(desc(!!sym(variavel))) %>%
    mutate(posicao = cumsum(Prop) - 0.5 * Prop)

  ggplot(contagem, aes(x = factor(""), y = Prop, fill = !!sym(variavel))) +
    geom_bar(width = 1, stat = "identity") +
    facet_wrap(~ Categoria) +
    coord_polar("y", start = 0) +
    geom_text(
      aes(x = 2, y = posicao, label = paste0(Prop, "%")),
      color = "black") +
    theme_estat() +
    theme(legend.position = "top") +
    scale_fill_manual(values = cores_estat, name = variavel)
}

# Uso da função para criar contagem e gráfico de barras para Marca, Cor e Tamanho

contagem_marca <- gerar_contagem_grafico(vendas_analise_1_marcas_setores, "Marca")
ggsave("setor_final_marca.pdf", plot = contagem_marca, width = 180, height = 93, units = "mm")

contagem_cor <- gerar_contagem_grafico(vendas_analise_1_marcas_setores, "Cor")
ggsave("setor_final_cor.pdf", plot = contagem_cor, width = 180, height = 93, units = "mm")

contagem_tamanho <- gerar_contagem_grafico(vendas_analise_1_marcas_setores, "Tamanho")
ggsave("setor_final_tamanho.pdf", plot = contagem_tamanho, width = 180, height = 93, units = "mm")

# Função para criar conjuntos de dados e gráficos com base em uma variável (Marca, Cor ou Tamanho)
# Ou seja um grafico de linha multivariado para cada categoria em marca, cor e tamanho

gerar_grafico <- function(dataset, variavel) {
  lista_faturamento_anual <- dataset %>%
    group_split(Categoria) %>%
    map(~ .x %>%
          group_by(`Mês da compra`, !!sym(variavel)) %>%
          summarise(Faturamento = sum(Preço)))

  nomes_categorias <- unique(dataset$Categoria)
  nomes_lista <- paste0("faturamento_anual_", gsub("'", "", nomes_categorias), "_", variavel)
  names(lista_faturamento_anual) <- nomes_lista

  lista_graficos <- map(lista_faturamento_anual, ~ ggplot(.x) +
                          aes(x = `Mês da compra`, y = Faturamento, group = !!sym(variavel), color = !!sym(variavel)) +
                          geom_line(size = 1) +
                          geom_point(size = 2) +
                          labs(x = "Mês", y = "Faturamento", color = variavel) +
                          scale_y_continuous(limits = c(0, max(.x$Faturamento) + 100), breaks = seq(0, max(.x$Faturamento) + 100, by = 100)) +
                          theme_estat())

  nomes_graficos <- names(lista_faturamento_anual)
  names(lista_graficos) <- paste0("grafico_", nomes_graficos)

  map(names(lista_graficos), ~ ggsave(paste0(.x, "_", variavel, ".pdf"), plot = lista_graficos[[.x]], width = 158, height = 93, units = "mm"))
}

# Uso da função para criar conjuntos de dados e gráficos para Marca, Cor e Tamanho

vendas_analise_1_marcas_linhas <- subset(vendas_analise_1, complete.cases(Categoria,Cor))

gerar_grafico(vendas_analise_1_marcas_linhas, "Marca")
gerar_grafico(vendas_analise_1_marcas_linhas, "Cor")
gerar_grafico(vendas_analise_1_marcas_linhas, "Tamanho")
