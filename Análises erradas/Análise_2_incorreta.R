################################
# Organizando o banco de dados #
#    (Termina na linha 104)    #
################################

# Carregando os Pacotes #

library(tidyverse)
library(xtable)
library(broom)
library(nortest)
library(car)
library(reshape2)

# Defindo o Diretório

setwd("C:\\Users\\andre\\OneDrive\\0 - EST PROGRAMAÇÃO\\0.0 - ESTAT\\PS\\Análises para o shopping da Barbie")

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

#Criando um dataset sem devolução

vendas_devolvidas <- vendas_secundario[!is.na(vendas_secundario$`Motivo de devolução`), ]

# Dataset completo formatado

vendas_final <- vendas_secundario[, !(names(vendas_secundario) %in% c("X"))]

##################
#FINAL FORMATAÇÃO#
##################

# Tema dos graficos
cores_estat <- c("#A11D21", "#003366", "#CC9900", "#663333", "#FF6600", "#CC9966", "#999966", "#006606", "#008091", "#041835", "#666666")
theme_estat <- function(...) {
  theme <- ggplot2:: theme_bw() +
    ggplot2:: theme(
      axis.title.y = ggplot2:: element_text(colour = "black",
                                            size = 12),
      axis.title.x = ggplot2:: element_text(colour = "black",
                                            size = 12),
      axis.text = ggplot2:: element_text(colour = "black", size
                                         = 9.5),
      panel.border = ggplot2:: element_blank (),
      axis.line = ggplot2:: element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  return(
    list(
      theme ,
      scale_fill_manual(values = cores_estat),
      scale_colour_manual(values = cores_estat)
    )
  )
}

##############
###ANALISES###
##############

# Retirando NAs  #

vendas_2 <- vendas_final
vendas_2 <- subset(vendas_2, complete.cases(Marca, Preço, Categoria, `Nome do produto`, Cor, Tamanho))

# Listando as marcas

nomes_marcas <- unique(vendas_2$`Marca`)
print(nomes_marcas)

# Criando conjuntos de dados para cada marca

vendas_adidas <- vendas_2 %>% filter(Marca == "Adidas")
vendas_hm <- vendas_2 %>% filter(Marca == "H&M")
vendas_gucci <- vendas_2 %>% filter(Marca == "Gucci")
vendas_zara <- vendas_2 %>% filter(Marca == "Zara")
vendas_nike <- vendas_2 %>% filter(Marca == "Nike")

# Box plot - multivariado (Preço mensal por marca) #
# (Como são 5 gráficos optei por fazer uma função) #

# Função boxplot preço mensal por marca

funcao_boxplot_marcas <- function(data, nome_arquivo) {
  p <- ggplot(data) +
    aes(x = `Mês da compra`, y = Preço) +
    geom_boxplot(fill = c("#A11D21"), width = 0.5) +
    stat_summary(
      fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
    ) +
    labs(x = "Mês da compra", y = "Preço") +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25)) +
    theme_estat()
  
  ggsave(nome_arquivo, plot = p, width = 158, height = 93, units = "mm")
}

# Chamando a função para cada um dos dataframes

funcao_boxplot_marcas(vendas_adidas, "box_adidas.pdf")
funcao_boxplot_marcas(vendas_hm, "box_hm.pdf")
funcao_boxplot_marcas(vendas_gucci, "box_gucci.pdf")
funcao_boxplot_marcas(vendas_zara, "box_zara.pdf")
funcao_boxplot_marcas(vendas_nike, "box_nike.pdf")

# Tabela cv mensal por marca #

# Função para calcular o coeficiente de variação (CV)
calc_cv <- function(x) {
  cv <- (sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)) * 100
  return(cv)
}

tabela_cv_preco <- vendas_2 %>%
  group_by(`Mês da compra`, Marca) %>%
  summarise(CV_Preco = calc_cv(Preço)) %>%
  mutate(CV_Preco = round(CV_Preco, digits = 2)) %>%
  pivot_wider(names_from = Marca, values_from = CV_Preco)


xtable :: xtable(tabela_cv_preco)

# Boxplot do preço mensal por categoria da marca #

# Listando as marcas

nomes_categorias <- unique(vendas_2$Categoria)
print(nomes_categorias)

# Criando conjuntos de dados para cada categoria

# Função para criar dataframes para cada marca e categoria
criar_dataframes_categorias <- function(data, marca) {
  data %>%
    filter(Categoria == "Men's Fashion") %>%
    assign(paste0("vendas_", marca, "_mens"), ., envir = .GlobalEnv)
  data %>%
    filter(Categoria == "Women's Fashion") %>%
    assign(paste0("vendas_", marca, "_womens"), ., envir = .GlobalEnv)
  data %>%
    filter(Categoria == "Kids' Fashion") %>%
    assign(paste0("vendas_", marca, "_kids"), ., envir = .GlobalEnv)
}

# Aplicando a função para cada marca
criar_dataframes_categorias(vendas_adidas, "adidas")
criar_dataframes_categorias(vendas_hm, "hm")
criar_dataframes_categorias(vendas_gucci, "gucci")
criar_dataframes_categorias(vendas_zara, "zara")
criar_dataframes_categorias(vendas_nike, "nike")

# Criando um gráfico para cada um dos datasets

# Lista de marcas
marcas <- c("adidas", "hm", "gucci", "zara", "nike")

# Lista de categorias
categorias <- c("kids", "mens", "womens")

# Loop para criar e salvar os gráficos em PDF
for (marca in marcas) {
  for (categoria in categorias) {
    data <- get(paste0("vendas_", marca, "_", categoria))
    filename <- paste0("box_", marca, "_", categoria, ".pdf")
    
    p <- ggplot(data) +
      aes(x = `Mês da compra`, y = Preço) +
      geom_boxplot(fill = c("#A11D21"), width = 0.5) +
      stat_summary(
        fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
      ) +
      labs(x = "Mês da compra", y = "Preço") +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25)) +
      theme_estat()
    
    ggsave(filename, plot = p, width = 158, height = 93, units = "mm")
  }
}

# Boxplot do preço mensal  por produto da marca #

# Listando os produtos

nomes_produtos <- unique(vendas_2$`Nome do produto`)
print(nomes_produtos)

# Função para criar dataframes para cada marca e produto
criar_dataframes_produtos <- function(data, marca) {
  produtos <- c("Dress", "Shoes", "T-shirt", "Jeans", "Sweater")
  for (produto in produtos) {
    assign(paste0("vendas_", marca, "_", produto), data %>% filter(`Nome do produto` == produto), envir = .GlobalEnv)
  }
}

# Aplicar a função para cada marca
criar_dataframes_produtos(vendas_adidas, "adidas")
criar_dataframes_produtos(vendas_hm, "hm")
criar_dataframes_produtos(vendas_gucci, "gucci")
criar_dataframes_produtos(vendas_zara, "zara")
criar_dataframes_produtos(vendas_nike, "nike")

# Lista de marcas
marcas <- c("adidas", "hm", "gucci", "zara", "nike")

# Lista de produtos
produtos <- c("Dress", "Shoes", "T-shirt", "Jeans", "Sweater")

# Loop para criar e salvar os gráficos em PDF
for (marca in marcas) {
  for (produto in produtos) {
    data <- get(paste0("vendas_", marca, "_", produto))
    filename <- paste0("box_", marca, "_", produto, ".pdf")
    
    p <- ggplot(data) +
      aes(x = `Mês da compra`, y = Preço) +
      geom_boxplot(fill = c("#A11D21"), width = 0.5) +
      stat_summary(
        fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
      ) +
      labs(x = "Mês da compra", y = "Preço") +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25)) +
      theme_estat()
    
    ggsave(filename, plot = p, width = 158, height = 93, units = "mm")
  }
}

# Boxplot do preço mensal por tamanho do produto da marca #

# Listando os tamanhos

nomes_tamanhos <- unique(vendas_2$Tamanho)
print(nomes_tamanhos)

# Função para criar dataframes para cada marca e tamanho
criar_dataframes_tamanhos <- function(data, marca) {
  tamanhos <- c("XL", "L", "M", "S")
  for (tamanho in tamanhos) {
    assign(paste0("vendas_", marca, "_", tamanho), data %>% filter(Tamanho == tamanho), envir = .GlobalEnv)
  }
}

# Aplicar a função para cada marca
criar_dataframes_tamanhos(vendas_adidas, "adidas")
criar_dataframes_tamanhos(vendas_hm, "hm")
criar_dataframes_tamanhos(vendas_gucci, "gucci")
criar_dataframes_tamanhos(vendas_zara, "zara")
criar_dataframes_tamanhos(vendas_nike, "nike")

# Lista de marcas
marcas <- c("adidas", "hm", "gucci", "zara", "nike")

# Lista de tamanhos
tamanhos <- c("XL", "L", "M", "S")

# Loop para criar e salvar os gráficos em PDF
for (marca in marcas) {
  for (tamanho in tamanhos) {
    data <- get(paste0("vendas_", marca, "_", tamanho))
    filename <- paste0("box_", marca, "_", tamanho, ".pdf")
    
    p <- ggplot(data) +
      aes(x = `Mês da compra`, y = Preço) +
      geom_boxplot(fill = c("#A11D21"), width = 0.5) +
      stat_summary(
        fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
      ) +
      labs(x = "Mês da compra", y = "Preço") +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25)) +
      theme_estat()
    
    ggsave(filename, plot = p, width = 158, height = 93, units = "mm")
  }
}

# Boxplot do preço mensal por cor do produto da marca #

# Listando as cores

nomes_cores <- unique(vendas_2$Cor)
print(nomes_cores)

# Função para criar dataframes para cada marca e cor

criar_dataframes_cores <- function(data, marca) {
  cores <- c("Black", "Yellow", "White", "Blue", "Green", "Red")
  for (cor in cores) {
    assign(paste0("vendas_", marca, "_", cor), data %>% filter(Cor == cor), envir = .GlobalEnv)
  }
}

# Aplicando a função para cada marca
criar_dataframes_cores(vendas_adidas, "adidas")
criar_dataframes_cores(vendas_hm, "hm")
criar_dataframes_cores(vendas_gucci, "gucci")
criar_dataframes_cores(vendas_zara, "zara")
criar_dataframes_cores(vendas_nike, "nike")

# Lista de marcas
marcas <- c("adidas", "hm", "gucci", "zara", "nike")

# Lista de cores
cores <- c("Black", "Yellow", "White", "Blue", "Green", "Red")

# Loop para criar e salvar os gráficos em PDF
for (marca in marcas) {
  for (cor in cores) {
    data <- get(paste0("vendas_", marca, "_", cor))
    filename <- paste0("box_", marca, "_", cor, ".pdf")
    
    p <- ggplot(data) +
      aes(x = `Mês da compra`, y = Preço) +
      geom_boxplot(fill = c("#A11D21"), width = 0.5) +
      stat_summary(
        fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
      ) +
      labs(x = "Mês da compra", y = "Preço") +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25)) +
      theme_estat()
    
    ggsave(filename, plot = p, width = 158, height = 93, units = "mm")
  }
}

# Testes

# Testando a variância do Preço da Marca por Cor, Categoria, Tamanho e Produto

# Crindo uma lista de nomes dos bancos de dados e os próprios bancos de dados
nomes_bancos <- c("vendas_adidas", "vendas_hm", "vendas_gucci", "vendas_zara", "vendas_nike")
bancos_de_dados <- list(vendas_adidas, vendas_hm, vendas_gucci, vendas_zara, vendas_nike)

# Criando uma lista de fórmulas do teste
formulas <- list(
  Preço ~ Cor,
  Preço ~ Tamanho,
  Preço ~ Categoria,
  Preço ~ `Nome do produto`
)

# Criando uma matriz para armazenar os p-values
p_values_matrix <- matrix(NA, nrow = length(nomes_bancos), ncol = length(formulas))

# Realizando os testes de Levene e armazenando os p-values
for (i in 1:length(nomes_bancos)) {
  for (j in 1:length(formulas)) {
    levene_result <- leveneTest(formulas[[j]], data = bancos_de_dados[[i]])
    p_values_matrix[i, j] <- levene_result$Pr[1]
  }
}

# Criando uma tabela com os nomes dos bancos de dados, as variáveis e os p-values
colnames(matriz_p_valor) <- sapply(formulas, deparse)
rownames(matriz_p_valor) <- nomes_bancos

# Convertendo a matriz em um data frame
tabela_p_values <- as.data.frame(matriz_p_valor)

# Exibindo a tabela
print(tabela_p_valores)
