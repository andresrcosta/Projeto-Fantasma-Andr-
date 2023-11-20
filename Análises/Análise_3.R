############################################################################
#### Análise 3 - Relação entre categoira (Masculino e feminino) e marca ####
############################################################################

# Clonando os dataset para não corromper o banco original

vendas_analise_3 <- vendas_final

# Retirando NAs

vendas_analise_3 <- subset(vendas_analise_3, complete.cases(Cor,Categoria))

# Listando as cores e Categorias

nomes_cores <- unique(vendas_analise_3$`Cor`)

nomes_categorias <- unique(vendas_analise_3$Categoria)

print(nomes_cores)

print(nomes_categorias)

# Retirando a categoria infatil

vendas_analise_3 <- subset(vendas_analise_3, Categoria != "Kids' Fashion")

# Criando a tabela de frequência entre Categoria e Cor

tabela_frequencia_analise_3 <- table(vendas_analise_3$Categoria, vendas_analise_3$Cor)

tabela_freq_com_margens_3 <- addmargins(tabela_frequencia_analise_3, FUN = list(Total = sum))

print(tabela_frequencia_analise_3)

print(tabela_freq_com_margens_3)

xtable :: xtable(tabela_frequencia_analise_3)

xtable :: xtable(tabela_freq_com_margens_3)

# Criando uma tabela de proporções

PT = prop.table(tabela_frequencia_analise_3, margin=1)

round(PT, 3)

xtable :: xtable(PT)

# Coeficiente V de cramer

if(!require(vcd)){install.packages("vcd")}

library(vcd)

assocstats(tabela_frequencia_analise_3)

# Teste qui-quadrado

resultado_chisq <- chisq.test(tabela_frequencia_analise_3)

print(resultado_chisq)