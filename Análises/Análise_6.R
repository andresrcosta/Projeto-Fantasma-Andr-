###############################################
#### Análise 6 - Avaliação média por marca ####
###############################################

# Clonando os datasets

vendas_analise_6 <- vendas_final

# Retirando Nas

vendas_analise_6 <- subset(vendas_analise_6, complete.cases(Marca, Avaliação))

# Calculando a média das avaliações por marca

media_avaliacao_por_marca_6 <- aggregate(vendas_analise_6$Avaliação, by = list(Marca = vendas_analise_6$Marca), FUN = mean)

# Renomeando as colunas

colnames(media_avaliacao_por_marca_6) <- c("Marca", "Media Avaliacao")

# Mostrando o resultado

print(media_avaliacao_por_marca_6)

xtable :: xtable(media_avaliacao_por_marca_6)

# Testando a normalidade dos dados

marcas_analise_6 <- unique(vendas_analise_6$Marca)

for (marca in marcas_analise_6) {
  subset_data <- vendas_analise_6$Avaliação[vendas_analise_2$Marca == marca]
  result_ad_test <- ad.test(subset_data)
  print(paste("Teste Anderson-Darling para", marca))
  print(result_ad_test)
}

# Normalidade satisfeita

# Anova para a média de avaliação entre as marcas

modelo_anova_6 <- aov(Avaliação ~ Marca, data = vendas_analise_6)

resultado_anova_6 <- summary(modelo_anova_6)

print(resultado_anova_6)