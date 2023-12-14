#################################################
####Analise 2 - Variação do preço por marca #####
#################################################

# Pacotes (olhar o código completo)

# Clonando o dataset para não aplicar erros aos dados originais
# Dataset completo, pois as devoluções não impactam nesta análise

vendas_analise_2 <- vendas_final

# Retirando NAs

vendas_analise_2 <- subset(vendas_analise_2, complete.cases(Marca, Preço))

# Gartantindo que os valores da coluna Preço sejam numéricos

vendas_analise_2$Preço <- as.numeric(vendas_analise_2$Preço)

# Listando as marcas

nomes_marcas <- unique(vendas_analise_2$`Marca`)
print(nomes_marcas)

# Boxplot multivaria do preço por marca

ggplot(vendas_analise_2) +
  aes(x = Marca , y = Preço) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Marca", y = "Preço") +
  theme_estat ()
ggsave("box_multi_marca_preço.pdf", width = 158, height = 93, units = "mm")

# Quadro de medidas resumo

quadro_resumo_analise_2 <- vendas_analise_2 %>%
  group_by( Marca ) %>% # caso mais de uma categoria
  summarize (Média = round(mean(Preço),2),
             `Desvio Padrão ` = round(sd(Preço),2),
             `Mínimo ` = round(min(Preço),2),
             `1º Quartil ` = round(quantile(Preço , probs = .25),2),
             Mediana = round(quantile(Preço , probs = .5),2),
             `3º Quartil ` = round(quantile(Preço , probs = .75),2),
             `Máximo` = round(max(Preço),2))  %>% t() %>% as.data.frame()

xtable :: xtable(quadro_resumo_analise_2)

# Testando se as médias dos preços em cada uma das marcas são diferentes

# Testando a normalidade dos dados para implementar a ANOVA

marcas_analise_2 <- unique(vendas_analise_2$Marca)

for (marca in marcas_analise_2) {
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

# Lista de marcas de interesse
marcas_interesse <- c("Adidas", "H&M", "Gucci", "Zara", "Nike")

# Criando uma lista vazia para armazenar os dataframes filtrados
lista_vendas_por_marca <- list()

# Loop para criar um dataframe para cada marca
for (marca in marcas_interesse) {
  nome_dataset <- paste("vendas_", tolower(gsub("&", "", gsub("'", "", gsub(" ", "_", marca)))), sep = "")
  dataset_filtrado <- vendas_analise_2 %>%
    filter(Marca == marca)
  
  # Armazenando o dataframe na lista usando o nome da marca como identificação
  lista_vendas_por_marca[[nome_dataset]] <- dataset_filtrado
}

# Graficos e testes para cada marca

# Lista de marcas de interesse

marcas_interesse <- c("Nike", "H&M", "Adidas", "Gucci")

# Lista de categorias de interesse

categorias_interesse <- c("Cor", "Categoria", "Tamanho")

# Função para criar boxplots e realizar testes ANOVA

gerar_boxplot_anova <- function(data, var_x, var_y, nome_arquivo) {
  ggplot(data) +
    aes_string(x = var_x, y = var_y) +
    geom_boxplot(fill = c("#A11D21"), width = 0.5) +
    stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
    labs(x = var_x, y = var_y) +
    theme_minimal()
  
  ggsave(nome_arquivo, width = 158, height = 93, units = "mm")
  
  modelo_anova <- aov(as.formula(paste(var_y, "~", var_x)), data = data)
  resultado_anova <- summary(modelo_anova)
  print(resultado_anova)
}

# Loop para criar boxplots e realizar testes ANOVA para cada marca e categoria

for (marca in marcas_interesse) {
  for (categoria in categorias_interesse) {
    nome_dataset <- paste("vendas_", tolower(gsub("&", "", gsub("'", "", gsub(" ", "_", marca)))), sep = "")
    dataset_filtrado <- subset(vendas_analise_2, Marca == marca & complete.cases({{categoria}}, Preço))
    
    nome_arquivo <- paste("box_", tolower(gsub("&", "", gsub("'", "", gsub(" ", "_", marca)))), "_", categoria, ".pdf", sep = "")
    
    gerar_boxplot_anova(dataset_filtrado, categoria, "Preço", nome_arquivo)
  }
}
