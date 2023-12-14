####################################################################
#### Análise 5 - Frequência de cada tipo de devolução por marca ####
####################################################################

# Pacotes (olhar o código completo)

# Clonando os datasets para não corromper o banco original

devolução_analise_5 <- devolução_atualizado_secundário
vendas_analise_5_inicio <- vendas_final

# Formatando o banco de devolução

novo_nome_colunas_5 <- c(
  "X" = "X",
  "Unique.ID" = "ID exclusivo",
  "Motivo.devolução" = "Motivo de devolução 2"
)

colnames(devolução_analise_5) <- novo_nome_colunas_5

# Juntando os bancos (com e sem devolução - após a correção da cliente)

# Unindo os dataframes com base na coluna "ID Exclusivo"

vendas_analise_5<- merge(vendas_analise_5_inicio, devolução_analise_5, by = "ID exclusivo")

# Listando tipos de motivos de devolução

nomes_motivos_5 <- unique(vendas_analise_5$`Motivo de devolução 2`)

print(nomes_motivos_5)

# Tabela de frequência

tabela_freqequencia_5 <- table(vendas_analise_5$Marca, vendas_analise_5$`Motivo de devolução 2`)

tabela_freq_com_margens_5 <- addmargins(tabela_freqequencia_5, FUN = list(Total = sum))

# Visualizar a tabela de frequências com margens

print(tabela_freq_com_margens_5)

xtable :: xtable(tabela_freq_com_margens_5)

# Criando uma tabela de proporções

PT = prop.table(tabela_freqequencia_5, margin=1)

round(PT, 3)

xtable :: xtable(tabela_freqequencia_5)

# Teste qui-quadrado

resultado_chisq <- chisq.test(tabela_freqequencia_5)

print(resultado_chisq)
