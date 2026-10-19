# script roteiro do BDAM - no repositório Treino_Extensao
# Antes de começar a fazer qualquer coisa:
# a) commit este roteiro com a mensagem "script roteiro BDAM" e envie para o repositório Treino_Extensao
# b) salve o script com outro nome (script_BDAM.R) e commit com a mensagem "script BDAM" e envie para o repositório Treino_Extensao

# Ao inserir os comandos em cada Tarefa de cada Etapa, mantenha as linhas de comentários e orientações colocadas pela professora


##### ETAPA 1 - banco 1 - equivalente ao SIM ######
##### Você deve criar e estar na branch banco-1 antes de inserir os comandos #####
##### NÃO altere as linhas de qualquer outra ETAPA do script e nem do cabeçalho ###

# Tarefa 1: Leitura do banco de dados banco 1 = SIM.csv com o nome de dados_bd1
# Ler o arquivo, verificar estrutura dos dados e dar uma olhada nos dados
library(dplyr)
dados_bd1 = read.csv2("banco 1 SIM.csv")
glimpse(dados_bd1)

# Ao terminar a Tarefa 1 commit com a mensagem " script - tarefa 1" e envie para o repositório Treino_Extensao


# Tarefa 2: Manipulação dos dados
# Padronizar as categorias VEICULO_CAUSADOR para Carro e Moto e indicar que branco é NA
dados_bd1 = dados_bd1 |>
  mutate(
    VEICULO_CAUSADOR = case_when(
      is.na(VEICULO_CAUSADOR) | trimws(VEICULO_CAUSADOR) == "" ~ NA_character_,
      grepl("carro", tolower(VEICULO_CAUSADOR)) ~ "Carro",
      grepl("moto", tolower(VEICULO_CAUSADOR)) ~ "Moto",
      TRUE ~ VEICULO_CAUSADOR
    ),
    
    # Atribuir legendas para a variável SEXO_CONDUTOR_CAUSADOR, sendo 1: Masculino e 2: Feminino
    SEXO_CONDUTOR_CAUSADOR = case_when(
      SEXO_CONDUTOR_CAUSADOR == 1 ~ "Masculino",
      SEXO_CONDUTOR_CAUSADOR == 2 ~ "Feminino",
      TRUE ~ as.character(SEXO_CONDUTOR_CAUSADOR)
    ),
    
    # Criar uma nova variável em dados_bd1 F_IDADE categorizando as idades em: 22 a 34, 35 a 45
    F_IDADE = case_when(
      IDADE_CONDUTOR_CAUSADOR >= 22 & IDADE_CONDUTOR_CAUSADOR <= 34 ~ "22 a 34",
      IDADE_CONDUTOR_CAUSADOR >= 35 & IDADE_CONDUTOR_CAUSADOR <= 45 ~ "35 a 45",
      TRUE ~ NA_character_)
  )


# Ao terminar a Tarefa 2 commit com a mensagem " script - tarefa 1 a 2" e envie para o repositório Treino_Extensao


# Tarefa 3: Criar o banco de dados BANCO1_RJ, POR MUNICÍPIO, com as seguintes variáveis listadas abaixo. 
# Variáveis que se referem a medidas de posição e de dispersão devem ser calculadas sem considerar NAs

# Atenção: a 1a linha do banco deve ser da UF 33
# ANO: 2025
# NIVEL: UF ou MUNICIPIO
# CODIGO: código do municipio (ou da UF)
# TV: total de veículos causadores de acidentes
# TC: total de carros causadores do acidente
# TM: total de motos causadoras do acidente
# TVCF: total de veículos causadores de acidentes com condutor mulher
# TVCM: total de veículos causadores de acidentes com condutor homem
# TC_22_34: total de condutores causadores de acidentes na faixa etária de 22 a 34 anos
# TC_35_45: total de condutores causadores de acidentes na faixa etária de 35 a 45 anos
# NMF: número médio de feridos
# DPF: desvio-padrão de feridos
# F_P25: percentil 25 do número de feridos
# F_P50: percentil 50 do número de feridos
# F_P75: percentil 75 do número de feridos
# TAFA: total de acidentes cuja causa foi falta de atenção
# TADS: total de acidentes cuja causa foi desrespeito à sinalização
# TADA: total de acidentes cuja causa foi o uso de drogas ou álcool
# TACO: total de acidentes cuja causa foi outros

BANCO1_RJ <- dados_bd1 |>
  mutate(UF = as.integer(substr(as.character(MUNICIPIO), 1, 2))) |>
  group_by(UF, CODIGO = MUNICIPIO) |>
  summarise(
    ANO = 2025,
    NIVEL = "MUNICIPIO",
    TV = n(),
    TC = sum(VEICULO_CAUSADOR == "Carro", na.rm = TRUE),
    TM = sum(VEICULO_CAUSADOR == "Moto", na.rm = TRUE),
    TVCF = sum(SEXO_CONDUTOR_CAUSADOR == "Feminino", na.rm = TRUE),
    TVCM = sum(SEXO_CONDUTOR_CAUSADOR == "Masculino", na.rm = TRUE),
    TC_22_34 = sum(F_IDADE == "22 a 34", na.rm = TRUE),
    TC_35_45 = sum(F_IDADE == "35 a 45", na.rm = TRUE),
    NMF = mean(NUM_FERIDOS_GRAVES, na.rm = TRUE),
    DPF = sd(NUM_FERIDOS_GRAVES, na.rm = TRUE),
    F_P25 = quantile(NUM_FERIDOS_GRAVES, 0.25, na.rm = TRUE),
    F_P50 = quantile(NUM_FERIDOS_GRAVES, 0.50, na.rm = TRUE),
    F_P75 = quantile(NUM_FERIDOS_GRAVES, 0.75, na.rm = TRUE),
    TAFA = sum(CAUSA_ACIDENTE == "falta de atencao", na.rm = TRUE),
    TADS = sum(CAUSA_ACIDENTE == "desrespeito a sinalizacao", na.rm = TRUE),
    TADA = sum(CAUSA_ACIDENTE == "alcool ou drogas", na.rm = TRUE),
    TACO = sum(!CAUSA_ACIDENTE %in% c("falta de atencao", "desrespeito a sinalizacao", "alcool ou drogas"), na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(UF == 33), UF, CODIGO)

# Ao terminar a Tarefa 3 commit com a mensagem " script - tarefa 1 a 3" e envie para o repositório Treino_Extensao


# Tarefa 4: Exportar o banco de dados BANCO1_RJ com o nome BANCO1_RJ.csv
write.csv2(BANCO1_RJ, 'BANCO1_RJ.csv')

# Ao terminar a Tarefa 4 commit com a mensagem "dados e script - Etapa 1"



##### ETAPA 2 - banco 2 - equivalente ao SINASC ######
##### Você deve criar e estar na branch banco-2 antes de inserir os comandos #####
##### NÃO altere as linhas de qualquer outra ETAPA do script e nem do cabeçalho ###

# Tarefa 1: Leitura do banco de dados banco 2 = SINASC.csv com o nome de dados_bd2
library(readxl)
dados_bd2= read.csv2(file = "banco 1 SIM.csv", na.strings = '')

# Ler o arquivo, verificar estrutura dos dados e dar uma olhada nos dados
dados_bd2
# Ao terminar a Tarefa 1 commit com a mensagem " script - tarefa 1" e envie para o repositório Treino_Extensao

summary(dados_bd2)
# Tarefa 2: Manipulação dos dados
# Padronizar as categorias SEXO_PROPRIETARIO para Masculino e Feminino
library(dplyr)

dados_bd2$SEXO_CONDUTOR_CAUSADOR= factor(dados_bd2$SEXO_CONDUTOR_CAUSADOR,labels = c("Masculino",'Feminino'), levels = c(1,2))

# Atribuir legendas para a variável TIPO_VEICULO, sendo 1: Carro e 2: Moto
dados_bd2 = dados_bd2|>
  rename(TIPO_VEICULO = VEICULO_CAUSADOR)

dados_bd2 = dados_bd2|> 
  mutate(TIPO_VEICULO = case_match(TIPO_VEICULO, c('moto', 'MOTO','Moto') ~2,
                                   c('carro','Carro','CARRO') ~ 1))


dados_bd2 = dados_bd2|>
  mutate(TIPO_VEICULO = factor(TIPO_VEICULO, levels = c(1,2), labels=c('Carro', "Moto")))

# Criar uma nova variável em dados_bd2 F_IDADE categorizando as idades em: 22 a 34, 35 a 45
dados_bd2 = dados_bd2|>
  mutate(F_IDADE = cut(dados_bd2$IDADE_CONDUTOR_CAUSADOR, breaks = c(21,34,45), labels = c('A','B')))

# Ao terminar a Tarefa 2 commit com a mensagem " script - tarefa 1 a 2" e envie para o repositório Treino_Extensao

# Tarefa 3: Criar o banco de dados BANCO2_RJ, POR MUNICÍPIO, com as seguintes variáveis listadas abaixo. 
# Variáveis que se referem a medidas de posição e de dispersão devem ser calculadas sem considerar NAs

# Atenção: a 1a linha do banco deve ser da UF 33
# ANO: 2025
# NIVEL: UF ou MUNICIPIO
# CODIGO: código do municipio (ou da UF)
# TVV: total de veiculos vendidos
# TCV: total de carros vendidos
# TMV: total de motos vendidas
# TVVF: total de veículos vendidos para mulher
# TVVM: total de veículos vendidos para homem
# TVC_22_34: total de veiculos vendidos para pessoas na faixa etária de 22 a 34 anos
# TVC_35_45: total de veiculos vendidos para pessoas na faixa etária de 35 a 45 anos
# VMV: valor médio dos veículos vendidos
# DPV: desvio-padrão do valor dos veículos vendidos
# V_P25: percentil 25 do valor dos veículos vendidos
# V_P50: percentil 50 do valor dos veículos vendidos
# V_P75: percentil 75 do valor dos veículos vendidos

BANCO2_RJ <- dados_bd2 |>
  mutate(UF = as.integer(substr(as.character(MUNICIPIO), 1, 2))) |>
  group_by(UF, CODIGO = MUNICIPIO) |>
  summarise(
    ANO = 2025,
    NIVEL = "MUNICIPIO",
    TVV = n(),
    TCV = sum(TIPO_VEICULO == "Carro", na.rm = TRUE),
    TMV = sum(TIPO_VEICULO == "Moto", na.rm = TRUE),
    TVVF = sum(SEXO_CONDUTOR_CAUSADOR == "Feminino", na.rm = TRUE),
    TVVM = sum(SEXO_CONDUTOR_CAUSADOR == "Masculino", na.rm = TRUE),
    TVC_22_34 = sum(F_IDADE == "A", na.rm = TRUE),
    TVC_35_45 = sum(F_IDADE == "B", na.rm = TRUE),
    VMV = mean(NUM_FERIDOS_GRAVES, na.rm = TRUE),
    DPV = sd(NUM_FERIDOS_GRAVES, na.rm = TRUE),
    V_P25 = quantile(NUM_FERIDOS_GRAVES, 0.25, na.rm = TRUE),
    V_P50 = quantile(NUM_FERIDOS_GRAVES, 0.50, na.rm = TRUE),
    V_P75 = quantile(NUM_FERIDOS_GRAVES, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(UF == 33), UF, CODIGO)

# Ao terminar a Tarefa 3 commit com a mensagem " script - tarefa 1 a 3" e envie para o repositório Treino_Extensao


# Tarefa 4: Exportar o banco de dados BANCO2_RJ com o nome BANCO2_RJ.csv

write.csv(BANCO2_RJ, "BANCO2_RJ.csv")

# Ao terminar a Tarefa 4 commit com a mensagem "dados e script - Etapa 2" e envie para o repositório Treino_Extensao


##### ETAPA 3 - banco 3 - equivalente ao SIDRA ######
##### Você deve criar e estar na branch banco-3 antes de inserir os comandos #####
##### NÃO altere as linhas de qualquer outra ETAPA do script e nem do cabeçalho ###

# Tarefa 1: Leitura do banco de dados banco 3 = SIDRA.csv com o nome de dados_bd3
# Ler o arquivo, verificar estrutura dos dados e dar uma olhada nos dados
dados_bd3 = read.csv2('banco 3 SIDRA.csv')
# Ao terminar a Tarefa 1 commit com a mensagem " script - tarefa 1" e envie para o repositório Treino_Extensao


# Tarefa 2: Manipulação dos dados
# Criar a variável MUNICIPIOS = MUNICIPIO em dados_bd3, sendo que agora com 6 dígitos (em vez de 7 dígitos), desprezando o último dígito verificador
library(dplyr)
dados_bd3 = dados_bd3 |> 
  mutate(MUNICIPIOS = MUNICIPIO %/% 10)

# Ao terminar a Tarefa 2 commit com a mensagem " script - tarefa 1 a 2" e envie para o repositório Treino_Extensao


# Tarefa 3: Criar o banco de dados BANCO3_RJ, POR MUNICÍPIO, com as seguintes variáveis listadas abaixo. 
# Atenção: a 1a linha do banco deve ser da UF 33
# ANO: 2025
# NIVEL: UF ou MUNICIPIO
# CODIGO: código do municipio (ou da UF)
# POPH: população total de habilitados
# POPHF: população total feminina de habilitadas
# POPHM: população total masculina de habilitadas
BANCO3_RJ = dados_bd3 |> group_by(MUNICIPIOS)|>
  summarise(
    ANO = 2025,
    NIVEL = 'MUNICÍPIO',
    CODIGO = MUNICIPIOS,
    POPH = sum(HABILITADOS_GERAL_2025),
    POPHF = sum(POP_FEM_HABILITADA_2020),
    POPHM = sum(POP_MASC_HABILITADA_2020)
  )

# Ao terminar a Tarefa 3 commit com a mensagem " script - tarefa 1 a 3" e envie para o repositório Treino_Extensao


# Tarefa 4: Exportar o banco de dados BANCO3_RJ com o nome BANCO3_RJ.csv
write.csv2(BANCO3_RJ, 'BANCO3_RJ.csv')
# Ao terminar a Tarefa 4 commit com a mensagem "dados e script - Etapa 3" e envie para o repositório Treino_Extensao


##### ETAPA 4 - banco 4 - equivalente ao ATLAS ######
##### Você deve criar e estar na branch banco-4 antes de inserir os comandos #####
##### NÃO altere as linhas de qualquer outra ETAPA do script e nem do cabeçalho ###

# Tarefa 1: Leitura do banco de dados banco 4 = ATLAS.csv com o nome de dados_bd4 e do arquivo com tabela de códigos do IBGE
# códigos dos municípios - 2010.csv" com os códigos do IBGE para os municípios do Brasil
# Ler os arquivos, verificar estruturas dos dados e dar uma olhada nos dados
library(dplyr)

dados_bd4 = read.csv2('banco 4 ATLAS.csv', encoding = 'latin1')
codigos_ibge = read.csv2('códigos dos municípios - 2010.csv')

glimpse(dados_bd4)
glimpse(codigos_ibge)
# Ao terminar a Tarefa 1 commit com a mensagem " script - tarefa 1" e envie para o repositório Treino_Extensao


# Tarefa 2: Manipulação dos dados
# Criar uma nova variável em dados_bd4 MUNICIPIOS atribuindo os códigos dos municípios, de forma a ficar
# coerente com os nomes dos municipios e códigos IBGE
dados_bd4 = dados_bd4 |> 
  left_join(codigos_ibge, by = c("MUNICIPIO" = "município")) |> 
  mutate(MUNICIPIOS = CODMUNRES %/% 10) |> 
  mutate(
    QUALIDADE_RODOVIAS_2020 = as.numeric(as.character(QUALIDADE_RODOVIAS_2020)),
    QUALIDADE_URBANA_2025   = as.numeric(as.character(QUALIDADE_URBANA_2025)),
    QUALIDADE_RURAL_2025    = as.numeric(as.character(QUALIDADE_RURAL_2025))
  )
# Ao terminar a Tarefa 2 commit com a mensagem " script - tarefa 1 a 2" e envie para o repositório Treino_Extensao


# Tarefa 3: Criar o banco de dados BANCO4_RJ, POR MUNICÍPIO, com as seguintes variáveis listadas abaixo. 
# Atenção: a 1a linha do banco deve ser da UF 33
# ANO: 2025
# NIVEL: UF ou MUNICIPIO
# CODIGO: código do municipio (ou da UF)
# QR_CA: qualidade da rodovia em 2020
# QRU: qualidade das rodovias urbanas
# QRR: qualidade das rodovias rurais

linha_uf = data.frame(
  ANO = 2025,
  NIVEL = 'UF',
  CODIGO = 33,
  QR_CA = mean(dados_bd4$QUALIDADE_RODOVIAS_2020, na.rm = TRUE),
  QRU = mean(dados_bd4$QUALIDADE_URBANA_2025, na.rm = TRUE),
  QRR = mean(dados_bd4$QUALIDADE_RURAL_2025, na.rm = TRUE)
)

tabela_municipios = dados_bd4 |> 
  group_by(MUNICIPIOS) |> 
  summarise(
    ANO = 2025,
    NIVEL = 'MUNICÍPIO',
    CODIGO = MUNICIPIOS,
    QR_CA = mean(QUALIDADE_RODOVIAS_2020, na.rm = TRUE),
    QRU = mean(QUALIDADE_URBANA_2025, na.rm = TRUE),
    QRR = mean(QUALIDADE_RURAL_2025, na.rm = TRUE)
  )

BANCO4_RJ = bind_rows(linha_uf, tabela_municipios)
# Ao terminar a Tarefa 3 commit com a mensagem " script - tarefa 1 a 3" e envie para o repositório Treino_Extensao


# Tarefa 4: Exportar o banco de dados BANCO4_RJ com o nome BANCO4_RJ.csv
write.csv2(BANCO4_RJ, 'BANCO4_RJ.csv')
# Ao terminar a Tarefa 4 commit com a mensagem "dados e script - Etapa 4" e envie para o repositório Treino_Extensao



##### ETAPA 5 - banco 5 - equivalente ao SINISA ######
##### Você deve criar e estar na branch banco-5 antes de inserir os comandos #####
##### NÃO altere as linhas de qualquer outra ETAPA do script e nem do cabeçalho ###

# Tarefa 1: Leitura do banco de dados banco 5 = SINISA.csv com o nome de dados_bd5 
# Ler os arquivos, verificar estruturas dos dados e dar uma olhada nos dados
library(dplyr)

dados_bd5 = read.csv2('banco 5 SINISA.csv')
glimpse(dados_bd5)

# Ao terminar a Tarefa 1 commit com a mensagem " script - tarefa 1" e envie para o repositório Treino_Extensao


# Tarefa 2: Manipulação dos dados
# Observe que os números estão com ponto indicando milhar. Estes pontos devem ser extraídos para não confundir com decimal
num_cols = c('NUM_CARROS_REGISTRADOS','NUM_MOTOS_REGISTRADAS')

dados_bd5[num_cols] = lapply(dados_bd5[num_cols], function(x) {
  as.numeric(gsub(".", "", x, fixed = TRUE))})

summary(dados_bd5)
# Ao terminar a Tarefa 2 commit com a mensagem " script - tarefa 1 a 2" e envie para o repositório Treino_Extensao


# Tarefa 3: Criar o banco de dados BANCO5_RJ, POR MUNICÍPIO, com as seguintes variáveis listadas abaixo. 
# Atenção: a 1a linha do banco deve ser da UF 33
# ANO: 2025
# NIVEL: UF ou MUNICIPIO
# CODIGO: código do municipio (ou da UF)
# NCR: número de carros registrados
# NMR: número de motos registradas

BANCO5_RJ = data.frame( ANO = 2025,
                        NIVEL = 'MUNICIPIO',
                        CODIGO = dados_bd5$MUNICIPIO,
                        NCR = dados_bd5$NUM_CARROS_REGISTRADOS,
                        NMR = dados_bd5$NUM_MOTOS_REGISTRADAS)

# Ao terminar a Tarefa 3 commit com a mensagem " script - tarefa 1 a 3" e envie para o repositório Treino_Extensao


# Tarefa 4: Exportar o banco de dados BANCO5_RJ com o nome BANCO5_RJ.csv
write.csv2(BANCO5_RJ, 'BANCO5_RJ.csv') 

# Ao terminar a Tarefa 4 commit com a mensagem "dados e script - Etapa 5" e envie para o repositório Treino_Extensao


##### Merge para a branch main ######
##### Após terminar todas as 5 etapas acima você deve ir para a branch main e fazer merge de cada branch para o Git ajustar tudo


##### ETAPA 6 - criação do BDAM - equivalente ao BDEM ######
##### Você deve estar na branch main #####

# Tarefa 1: Concatenar (merge) os 5 arquivos gerados em cada uma das 5 etapas num único arquivo chamado BDAM_RJ, de modo que
# as variáveis fiquem dispostas da seguinte descrita abaixo

# Atenção: a 1a linha do banco deve ser da UF 33
# ANO: 2025
# NIVEL: UF ou MUNICIPIO
# CODIGO: código do municipio (ou da UF)
# POPH: população total de habilitados
# POPHF: população total feminina de habilitadas
# POPHM: população total masculina de habilitadas
# QR_CA: qualidade da rodovia em 2020
# QRU: qualidade das rodovias urbanas
# QRR: qualidade das rodovias rurais
# TVV: total de veiculos vendidos
# TCV: total de carros vendidos
# TMV: total de motos vendidas
# TVVF: total de veículos vendidos para mulher
# TVVM: total de veículos vendidos para homem
# TVC_22_34: total de veiculos vendidos para pessoas na faixa etária de 22 a 34 anos
# TVC_35_45: total de veiculos vendidos para pessoas na faixa etária de 35 a 45 anos
# VMV: valor médio dos veículos vendidos
# DPV: desvio-padrão do valor dos veículos vendidos
# V_P25: percentil 25 do valor dos veículos vendidos
# V_P50: percentil 50 do valor dos veículos vendidos
# V_P75: percentil 75 do valor dos veículos vendidos
# TV: total de veículos causadores de acidentes
# TC: total de carros causadores do acidente
# TM: total de motos causadoras do acidente
# TVCF: total de veículos causadores de acidentes com condutor mulher
# TVCM: total de veículos causadores de acidentes com condutor homem
# TC_22_34: total de condutores causadores de acidentes na faixa etária de 22 a 34 anos
# TC_35_45: total de condutores causadores de acidentes na faixa etária de 35 a 45 anos
# NMF: número médio de feridos
# DPF: desvio-padrão de feridos
# F_P25: percentil 25 do número de feridos
# F_P50: percentil 50 do número de feridos
# F_P75: percentil 75 do número de feridos
# TAFA: total de acidentes cuja causa foi falta de atenção
# TADS: total de acidentes cuja causa foi desrespeito à sinalização
# TADA: total de acidentes cuja causa foi o uso de drogas ou álcool
# TACO: total de acidentes cuja causa foi outros
# NCR: número de carros registrados
# NMR: número de motos registradas
banco1 = read.csv2('BANCO1_RJ.csv')
banco2 = read.csv2('BANCO2_RJ.csv')
banco3 = read.csv2('BANCO3_RJ.csv')
banco4 = read.csv2('BANCO4_RJ.csv')
banco5 = read.csv2('BANCO5_RJ.csv')

BDAM_RJ = banco3 |> 
  left_join(banco4, by = c("ANO", "NIVEL", "CODIGO")) |> 
  left_join(banco1, by = c("ANO", "NIVEL", "CODIGO")) |> 
  left_join(banco2, by = c("ANO", "NIVEL", "CODIGO")) |> 
  left_join(banco5, by = c("ANO", "NIVEL", "CODIGO")) |> 
  arrange(desc(NIVEL), CODIGO)
# Ao terminar a Tarefa 1 commit com a mensagem " script - tarefa 1" e envie para o repositório Treino_Extensao


# Tarefa 2: Exportar o banco de dados BDAM_RJ com o nome BDAM_RJ.csv
write.csv2(BDAM_RJ, 'BDAM_RJ.csv')
# Ao terminar a Tarefa 2 commit com a mensagem "dados e script - Etapa 6" e envie para o repositório Treino_Extensao

