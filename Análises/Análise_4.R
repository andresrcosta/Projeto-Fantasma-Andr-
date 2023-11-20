#####################################################
#### Análise 4 - Relação entre preço e avaliação ####
#####################################################

# Clonando os datasets

vendas_analise_4 <- vendas_final

# Retirando NAs

vendas_analise_4 <- subset(vendas_analise_4, complete.cases(Preço,Avaliação))

# Testando a normalidade dos dados

# Teste de normalidade para a variável Preço

resultado_preco_4 <- ad.test(vendas_analise_4$Preço)

print(resultado_preco_4)

# Teste de normalidade para a variável Avaliação

resultado_avaliacao_4 <- ad.test(vendas_analise_4$Avaliação)

print(resultado_avaliacao_4)

# Calculando a correlação de Pearson e realizando o teste

correlacao <- cor.test(vendas_analise_4$Preço, vendas_analise_4$Avaliação, method = "pearson")

print(correlacao)

# Gráfico de disperção

ggplot(vendas_analise_4) +
  aes(x = Preço , y = Avaliação) +
  geom_point(colour = "#A11D21", size = 2) +
  labs(
    x = "Preço",
    y = "Avaliação"
  ) +
  theme_estat ()
ggsave("disperção_analise_4.pdf", width = 158, height = 93, units = "mm")