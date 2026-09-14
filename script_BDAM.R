
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
# TCV: total de carros vendidoschmod 600 ~/.git-credentials && git push
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




# Ao terminar a Tarefa 3 commit com a mensagem " script - tarefa 1 a 3" e envie para o repositório Treino_Extensao


# Tarefa 4: Exportar o banco de dados BANCO2_RJ com o nome BANCO2_RJ.csv

# Ao terminar a Tarefa 4 commit com a mensagem "dados e script - Etapa 2" e envie para o repositório Treino_Extensao
