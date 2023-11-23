# Avaliando o faturamento (devouluções)

# Clonando os datasets para não corromper o banco original

devolução_analise_1 <- devolução_atualizado_secundário
vendas_analise_1_inicio <- vendas_final

# Formatando o banco de devolução

novo_nome_colunas_1 <- c(
  "X" = "X",
  "Unique.ID" = "ID exclusivo",
  "Motivo.devolução" = "Motivo de devolução 2"
)

colnames(devolução_analise_1) <- novo_nome_colunas_1

# Juntando os bancos (com e sem devolução - após a correção da cliente)

# Unindo os dataframes com base na coluna "ID Exclusivo"

vendas_analise_1_2 <- left_join(vendas_analise_1_inicio, devolução_analise_1, by = "ID exclusivo")

# Listando tipos de motivos de devolução

nomes_motivos_1 <- unique(vendas_analise_1_2$`Motivo de devolução 2`)

print(nomes_motivos_1)

# Agrupando as devoluções por mes

vendas_analise_1_2_dev <- subset(vendas_analise_1_2, complete.cases(`Motivo de devolução 2`))

devolucoes_mes <- vendas_analise_1_2_dev %>%
  group_by(`Mês da compra`, `Motivo de devolução 2`) %>%
  summarise(Contagem = n())

# Gráfico

ggplot(devolucoes_mes) +
  aes(x = `Mês da compra`, y = Contagem, group = `Motivo de devolução 2`, color = `Motivo de devolução 2`) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Mês", y = "Contagem de devoluções", color = "Motivo da devolução") +
  scale_y_continuous(limits = c(0, 35), breaks = seq(0, 35, by = 5)) +
  theme_estat()
