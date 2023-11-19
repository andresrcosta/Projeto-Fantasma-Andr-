################################
# Organizando o banco de dados #
################################

# Carregando os Pacotes #

library(tidyverse)

# Defindo o Diretório

setwd("")

# Importando os arquivos

vendas <- read.csv("vendas.csv", encoding = "UTF-8")
vendas_secundario <- vendas
devolução <- read.csv("devolução.csv" , encoding = "UTF-8")
devolucao_secundario <- devolução

# Confirmando que todas as compras são de 2022

# Filtrando as compras de 2022 usando expressoes regulares

vendas_secundario <- vendas_secundario %>%
  filter(grepl("2022", Data.Venda))

# Formatando as as colunas

novo_nome_colunas <- c(
  "X" = "X",
  "...1.x" = "Índice X",
  "Data.Venda" = "Data da venda",
  "User.ID" = "ID do usuário",
  "Product.ID" = "ID do produto",
  "Product.Name" = "Nome do produto",
  "Brand" = "Marca",
  "Category" = "Categoria",
  "Price" = "Preço",
  "Rating" = "Classificação",
  "Color" = "Cor",
  "Size" = "Tamanho",
  "Unique.ID" = "ID exclusivo",
  "...1.y" = "Índice Y",
  "Motivo.devolução" = "Motivo de devolução"
)

colnames(vendas_secundario) <- novo_nome_colunas


# Formatando as datas das vendas

# Função para padronizar as datas

padronizar_data <- function(data) {
  if (is.na(data)) {
    return(NA)  # Se a data for NA, retorne NA
  } else if (nchar(data) == 10) {
    return(data)  # Já está no formato desejado
  } else {
    partes_data <- unlist(strsplit(data, "/"))
    if (length(partes_data) == 3) {
      mes <- sprintf("%02d", as.numeric(partes_data[1]))
      dia <- sprintf("%02d", as.numeric(partes_data[2]))
      ano <- partes_data[3]
      data_formatada <- paste(mes, dia, ano, sep = "/")
      return(data_formatada)
    } else {
      return(NA)  # Se os componentes da data não puderem ser extraídos, retorne NA
    }
  }
}

# Aplicando a função de padronização na coluna "Data_da_venda"

vendas_secundario$`Data da venda` <- sapply(vendas_secundario$`Data da venda`, padronizar_data)

# Função para extrair o nome do mês
extrair_nome_mes <- function(data) {
  data <- mdy(data)  # Converte a data para o formato padrão mês/dia/ano
  nome_mes <- month(data, label = TRUE)
  return(nome_mes)
}

# Aplicando a função e criando uma coluna com o mês da compra

vendas_secundario$`Mês da compra` <- sapply(vendas_secundario$`Data da venda`, extrair_nome_mes)

# Garatindo que os valores da coluna preço sejam numéricos

vendas_secundario$Preço <- as.numeric(vendas_secundario$Preço)

# Criando um dataset sem devolução

vendas_sem_devolucao <- vendas_secundario[is.na(vendas_secundario$`Motivo de devolução`), ]
vendas_sem_devolucao <- vendas_sem_devolucao[, !(names(vendas_sem_devolucao) %in% c("Índice Y", "Motivo de devolução"))]

# Dataset completo formatado

vendas_final <- vendas_secundario[, !(names(vendas_secundario) %in% c("X"))]
