#################################
#         Curso Fiocruz         #
# Preparando as bases de dados  #
#################################

rm(list = ls())

# R packages #
if(!require(janitor))install.packages("janitor");library(janitor)
if(!require(skimr))install.packages("skimr");library(skimr)
if(!require(lubridate))install.packages("lubridate");library(lubridate)
if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)
library(ggplot2)

#################################
# 1. Carregar as bases de dados #
#################################

## Clima - todos os anos; Regiao Nordeste
clima <- read.csv("C:/Users/RenataYokota/Documents/Other/Projeto Cnpq/Curso Fiocruz/Base de dados/INMET/northeast.csv")
head(clima)

## SRAG: 2019; todos os municipios
srag <- read.csv("C:/Users/RenataYokota/Documents/Other/Projeto Cnpq/Curso Fiocruz/Base de dados/SRAG/INFLUD19.csv", sep = ";")
head(srag)

## SIH: 2019; Regiao Nordeste
sih <- read.csv("C:/Users/RenataYokota/Documents/Other/Projeto Cnpq/Curso Fiocruz/Base de dados/SIH/sihsus.csv")

## SIM: 2016-2019; Regiao Nordeste
sim <- read.csv("C:/Users/RenataYokota/Documents/Other/Projeto Cnpq/Curso Fiocruz/Base de dados/SIM/sim.csv")

##################################################################
# 2. Preparando o SIH - Script "SIHSUScurso" (autor: Andre Peres)#
##################################################################
head(sih)

## 2.1 Substituir os asteriscos nas variaveis de causas do giagnostico principal usando gsub()
sih$DIAG_PRINC <- gsub("\\*", "", sih$DIAG_PRINC)

## 2.2 Excluir simbolos e transformar em letras minúsculas os nomes das variáveis
sih <- clean_names(sih) 

## 2.4 Alterar o tipo das variaveis
sih<- sih |> 
  mutate(sexo=as.character(sexo)) |>
  mutate(ano_cmpt=as.character(ano_cmpt)) |>
  mutate(ident=as.character(ident)) |>
  mutate(munic_res=as.character(munic_res)) |>
  mutate(cobranca=as.character(cobranca)) |>
  mutate(munic_mov=as.character(munic_mov)) |>
  mutate(cod_idade=as.character(cod_idade)) |>
  mutate(morte=as.character(morte))

summary(sih) # apresente um sumário dos dados

## 2.5 criar a variavel Ano do Óbito (anoobito) e ano do nascimento (anonasc)

sih$anointerna <- as.character(format(as.Date(sih$dt_inter, format = "%Y-%m-%d"), "%Y"))
sih$anonasc <- as.character(format(as.Date(sih$nasc, format = "%Y-%m-%d"), "%Y"))

## 2.6 criar a variavel ufres (unidade da federação de residência) e ufservico (unidade da federação do serviço)
sih$ufres <- substr(sih$munic_res, 1, 2)  # Usando substr para extrair os dois primeiros dígitos
sih$ufservico <- substr(sih$munic_mov, 1, 2)  # Usando substr para extrair os dois primeiros dígitos

## 2.7 criar as variveis região de residencia (regres) e região de ocorrência (regocor)
sih <- sih |>
  mutate(
    regres = case_when(
      ufres == '21' ~ "nordeste", 
      ufres == '22' ~ "nordeste", 
      ufres == '23' ~ "nordeste", 
      ufres == '24' ~ "nordeste", 
      ufres == '25' ~ "nordeste",
      ufres == '26' ~ "nordeste", 
      ufres == '27' ~ "nordeste", 
      ufres == '28' ~ "nordeste", 
      ufres == '29' ~ "nordeste", 
      ufres == '999' | ufres == '0' ~ "ignorado",
      is.na(ufres) ~ "ignorado",
      TRUE ~ NA_character_))|>
  mutate(
    regserv = case_when(
      ufservico == '11' ~ "norte", 
      ufservico == '12' ~ "norte", 
      ufservico == '13' ~ "norte", 
      ufservico == '14' ~ "norte", 
      ufservico == '15' ~ "norte", 
      ufservico == '16' ~ "norte", 
      ufservico == '17' ~ "norte", 
      ufservico == '21' ~ "nordeste", 
      ufservico == '22' ~ "nordeste", 
      ufservico == '23' ~ "nordeste", 
      ufservico == '24' ~ "nordeste", 
      ufservico == '25' ~ "nordeste",
      ufservico == '26' ~ "nordeste", 
      ufservico == '27' ~ "nordeste", 
      ufservico == '28' ~ "nordeste", 
      ufservico == '29' ~ "nordeste", 
      ufservico == '31' ~ "sudeste", 
      ufservico == '32' ~ "sudeste", 
      ufservico == '33' ~ "sudeste", 
      ufservico == '35' ~ "sudeste", 
      ufservico == '41' ~ "sul", 
      ufservico == '42' ~ "sul", 
      ufservico == '43' ~ "sul", 
      ufservico == '50' ~ "centro-oeste", 
      ufservico == '51' ~ "centro-oeste", 
      ufservico == '52' ~ "centro-oeste",
      ufservico == '53' ~ "centro-oeste",
      ufservico == '999' | ufservico == '0' ~ "ignorado",
      is.na(ufservico) ~ "ignorado",
      TRUE ~ NA_character_))

## 2.8  recodificar a idade em anos

# criar a variável sih$idade_anos com base nas condições corrigidas
# Se cod_idade for "0", retorna NA
# Se cod_idade for "1", "2" ou "3", retorna 0
# Se cod_idade for "4", retorna sih$idade, caso contrário, retorna NA

sih <- sih %>% 
  mutate(idade_anos = case_when(
    cod_idade == "0" ~ NA_integer_,
    cod_idade %in% c("2", "3") ~ 0,
    cod_idade == "4" ~ idade,
    TRUE ~ NA_integer_
  ))

## 2.9 Criar a nova variável sih$grupo com os dois primeiros caracteres de sih$diag_princ
sih$cid <- substr(sih$diag_princ, start = 1, stop = 3)

# 2.10 Renomear os valores das variáveis usando a função `mutate()` e `case_when()`
sih <- sih |>
  mutate(
    mes_cmpt = case_when(
      mes_cmpt == '01' ~ "jan", 
      mes_cmpt == '02' ~ "fev", 
      mes_cmpt == '03' ~ "mar", 
      mes_cmpt == '04' ~ "abr", 
      mes_cmpt == '05' ~ "mai",
      mes_cmpt == '06' ~ "jun", 
      mes_cmpt == '07' ~ "jul", 
      mes_cmpt == '08' ~ "ago", 
      mes_cmpt == '09' ~ "set",
      mes_cmpt == '10' ~ "out",
      mes_cmpt == '11' ~ "nov",
      mes_cmpt == '12' ~ "dez",
      mes_cmpt == '00' ~ "ignorado",
      mes_cmpt == "" |is.na(mes_cmpt) ~ "ignorado",
      TRUE ~ NA_character_))|>
  mutate(
    espec = case_when(
      espec == '01' ~ "clínica cirugica", 
      espec == '02' ~ "obstetrícia", 
      espec == '03' ~ "clínica médica", 
      espec == '04' ~ "cuidados prolongados", 
      espec == '05' ~ "psiquiatria",
      espec == '06' ~ "pneumologia sanitária", 
      espec == '07' ~ "pediatria", 
      espec == '08' ~ "reabilitação", 
      espec == '09' ~ "leito dia/cirúrgico",
      espec == '10' ~ "leito dia/aids",
      espec == '11' ~ "leito dia/fibrose cística",
      espec == '12' ~ "leito dia/interc. pós transplante",
      espec == '13' ~ "leito dia/geriatria",
      espec == '14' ~ "leito dia/saúde mental",
      espec == '00' ~ "ignorado",
      ident == "" | is.na(espec) ~ "ignorado",
      TRUE ~ NA_character_))|>
  mutate(
    ident = case_when(
      ident == "1" ~ "aih normal",
      ident == "5" ~ "aih longa permanência",
      ident == "0" ~ "ignorado",
      ident == "" | is.na(ident) ~ "ignorado",
      TRUE ~ NA_character_))|>
  mutate(
    sexo = case_when(
      sexo == "1" ~ "masculino",
      sexo == "3" ~ "feminino",
      sexo == "0" ~ "ignorado",
      sexo == "" | is.na(sexo) ~ "ignorado",
      TRUE ~ NA_character_))|>
  mutate(
    marca_uti = case_when(
      marca_uti == "00" ~ "sem especialidade",
      marca_uti == "74" ~ "uti I",
      marca_uti == "75" ~ "uti adulto I",
      marca_uti == "76" ~ "uti adulto II",
      marca_uti == "77" ~ "uti infantil I",
      marca_uti == "78" ~ "uti infantil II",
      marca_uti == "79" ~ "uti infantil III",
      marca_uti == "80" ~ "uti neonatal I",
      marca_uti == "81" ~ "uti neonatal II",
      marca_uti == "82" ~ "uti neonatal III",
      marca_uti == "83" ~ "uti queimados",
      marca_uti == "99" ~ "não utilizou",
      marca_uti == ""| is.na(marca_uti) ~ "ignorado",
      TRUE ~ NA_character_))|>
  #  mutate(
  #     cobranca = case_when(
  #     cobranca == "11" ~ "alta curado",
  #     cobranca == "12" ~ "alta melhorado",
  #     cobranca == "13" ~ "alta da Puérpera e permanência do recém-nascido",
  #     cobranca == "14" ~ "alta à pedido",
  #     cobranca == "15" ~ "alta com previsão de retorno para acompanhamento do paciente",
  #     cobranca == "16" ~ "alta por evasão",
  #     cobranca == "17" ~ "alta da Puérpera e recém-nascido",
  #     cobranca == "18" ~ "alta por outros motivos",
  #     cobranca == "21" ~ "por características próprias da doença",
  #     cobranca == "22" ~ "por intercorrência",
  #     cobranca == "23" ~ "por impossibilidade sócio-familiar",
  #     cobranca == "24" ~ "por processo de doação de órgãos, tecidos e células - doador vivo",
  #     cobranca == "25" ~ "por processo de doação de órgãos, tecidos e células - doador morto",
  #     cobranca == "26" ~ "por mudança de procedimento",
  #     cobranca == "27" ~ "por reoperação",
  #     cobranca == "28" ~ "outros motivos",
  #     cobranca == "31" ~ "transferido para outro estabelecimento",
  #     cobranca == "41" ~ "com D.O. fornecida pelo médico assistente",
  #     cobranca == "42" ~ "com D.O. fornecida pelo Instituto Médico Legal - IML",
  #     cobranca == "43" ~ "com D.O. fornecida pelo Serviço de Verificação de Óbito – SVO.",
  #     cobranca == "51" ~ "encerramento administrativo",
  #     cobranca == ""| is.na(cobranca) ~ "ignorado",
  #     TRUE ~ NA_character_))|>
  mutate(
    cobranca = case_when(
      cobranca == "11" ~ "alta",
      cobranca == "12" ~ "alta",
      cobranca == "13" ~ "alta",
      cobranca == "14" ~ "alta",
      cobranca == "15" ~ "alta",
      cobranca == "16" ~ "alta",
      cobranca == "17" ~ "alta",
      cobranca == "18" ~ "alta",
      cobranca == "21" ~ "permanência",
      cobranca == "22" ~ "permanência",
      cobranca == "23" ~ "permanência",
      cobranca == "24" ~ "permanência",
      cobranca == "25" ~ "permanência",
      cobranca == "26" ~ "permanência",
      cobranca == "27" ~ "permanência",
      cobranca == "28" ~ "permanência",
      cobranca == "31" ~ "transferência",
      cobranca == "41" ~ "óbito",
      cobranca == "42" ~ "óbito",
      cobranca == "43" ~ "óbito",
      cobranca == "51" ~ "outros motivos",
      cobranca == ""| is.na(cobranca) ~ "ignorado",
      TRUE ~ NA_character_))|>
  mutate(
    cod_idade = case_when(
      cod_idade == "0" ~ "ignorada",
      cod_idade == "2" ~ "dias",
      cod_idade == "3" ~ "meses",
      cod_idade == "4" ~ "anos",
      cod_idade == ""| is.na(cod_idade) ~ "ignorado",
      TRUE ~ NA_character_))|>
  mutate(
    morte = case_when(
      morte == "0" ~ "não",
      morte == "1" ~ "sim",
      morte == ""| is.na(morte) ~ "ignorado",
      TRUE ~ NA_character_))|>
  #  mutate(
  #    nacional = case_when(
  #      nacional == "10" ~ "brasileiro",
  #      nacional == "20" ~ "naturalizado brasileiro",
  #      nacional == "21" ~ "argentino",
  #      nacional == "22" ~ "boliviano",
  #      nacional == "23" ~ "chileno",
  #      nacional == "24" ~ "paraguaio",
  #      nacional == "25" ~ "uruguaio",
  #      nacional == "30" ~ "alemão",
  #      nacional == "31" ~ "belga",
  #      nacional == "32" ~ "britânico",
  #      nacional == "34" ~ "canadense",
  #      nacional == "35" ~ "espanhol",
  #      nacional == "36" ~ "norte-americano(EUA)",
  #      nacional == "37" ~ "francês",
  #      nacional == "38" ~ "suiço",
  #      nacional == "39" ~ "italiano",
  #      nacional == "41" ~ "japonês",
  #      nacional == "42" ~ "chinês",
  #      nacional == "43" ~ "coreano",
  #      nacional == "45" ~ "português",
  #      nacional == "48" ~ "outros latino-americanos",
  #      nacional == "49" ~ "outros asiáticos",
  #      nacional == "50" ~ "outros",
  #      nacional == ""| is.na(nacional) ~ "ignorado",
  #      TRUE ~ NA_character_))|>
  mutate(
    nacional = case_when(
      nacional == "10" ~ "brasileiro",
      nacional == "20" ~ "brasileiro",
      nacional == "21" ~ "não brasileiros",
      nacional == "22" ~ "não brasileiros",
      nacional == "23" ~ "não brasileiros",
      nacional == "24" ~ "não brasileiros",
      nacional == "25" ~ "não brasileiros",
      nacional == "30" ~ "não brasileiros",
      nacional == "31" ~ "não brasileiros",
      nacional == "32" ~ "não brasileiros",
      nacional == "34" ~ "não brasileiros",
      nacional == "35" ~ "não brasileiros",
      nacional == "36" ~ "não brasileiros",
      nacional == "37" ~ "não brasileiros",
      nacional == "38" ~ "não brasileiros",
      nacional == "39" ~ "não brasileiros",
      nacional == "41" ~ "não brasileiros",
      nacional == "42" ~ "não brasileiros",
      nacional == "43" ~ "não brasileiros",
      nacional == "45" ~ "não brasileiros",
      nacional == "48" ~ "não brasileiros",
      nacional == "49" ~ "não brasileiros",
      nacional == "50" ~ "não brasileiros",
      nacional == ""| is.na(nacional) ~ "ignorado",
      TRUE ~ NA_character_))|>
  mutate(
    car_int = case_when(
      car_int == "01" ~ "eletivo",
      car_int == "02" ~ "urgência",
      car_int == "03" ~ "acidente no local de trabalho ou a serviço da empresa",
      car_int == "04" ~ "acidente no trajeto para o trabalho",
      car_int == "05" ~ "outros tipos de acidente de transito",
      car_int == "06" ~ "outros tipos de lesões e envenenamentos por agentes químicos ou físicos",
      car_int == ""| is.na(car_int) ~ "ignorado",
      TRUE ~ NA_character_))|>
  mutate(
    raca_cor = case_when(
      raca_cor == "01" ~ "branca",
      raca_cor == "02" ~ "negra",
      raca_cor == "03" ~ "parda",
      raca_cor == "04" ~ "amarela",
      raca_cor == "05" ~ "indígena",
      raca_cor == "99" ~ "sem informação",
      raca_cor == ""| is.na(raca_cor) ~ "ignorado",
      TRUE ~ NA_character_))|>
  
  mutate(
    ufres = case_when(
      ufres == '21' ~ "MA", 
      ufres == '22' ~ "PI", 
      ufres == '23' ~ "CE", 
      ufres == '24' ~ "RN", 
      ufres == '25' ~ "PB",
      ufres == '26' ~ "PE", 
      ufres == '27' ~ "AL", 
      ufres == '28' ~ "SE", 
      ufres == '29' ~ "BA", 
      ufres == '999' | ufres == '0' ~ "ignorado",
      is.na(ufres) ~ "ignorado",
      TRUE ~ NA_character_))|>
  mutate(
    ufservico = case_when(
      ufservico == '11' ~ "RO", 
      ufservico == '12' ~ "AC", 
      ufservico == '13' ~ "AM", 
      ufservico == '14' ~ "RR", 
      ufservico == '15' ~ "PA", 
      ufservico == '16' ~ "AP", 
      ufservico == '17' ~ "TO", 
      ufservico == '21' ~ "MA", 
      ufservico == '22' ~ "PI", 
      ufservico == '23' ~ "CE", 
      ufservico == '24' ~ "RN", 
      ufservico == '25' ~ "PB",
      ufservico == '26' ~ "PE", 
      ufservico == '27' ~ "AL", 
      ufservico == '28' ~ "SE", 
      ufservico == '29' ~ "BA", 
      ufservico == '31' ~ "MG", 
      ufservico == '32' ~ "ES", 
      ufservico == '33' ~ "RJ", 
      ufservico == '35' ~ "SP", 
      ufservico == '41' ~ "PR", 
      ufservico == '42' ~ "SC", 
      ufservico == '43' ~ "RS", 
      ufservico == '50' ~ "MS", 
      ufservico == '51' ~ "MT", 
      ufservico == '52' ~ "GO",
      ufservico == '53' ~ "DF",
      ufservico == '999' | ufservico == '0' ~ "ignorado",
      is.na(ufservico) ~ "ignorado",
      TRUE ~ NA_character_))|>
  mutate(
    grupo = case_when(
      cid == "J03" ~ "amigdalite aguda",
      cid == "J04" ~ "laringite e traqueíte aguda",
      cid == "J05" ~ "laringite obstrutiva aguda/epiglofite",
      cid == "J06" ~ "Infecções agudas das VAS",
      cid == "J09" ~ "Influenza (gripe aviária)",
      cid == "J10" ~ "Influenza por outro vírus",
      cid == "J11" ~ "Influenza por vírus n.id.",
      cid == "J12" ~ "Pneumonia viral não classificada",
      cid == "J13" ~ "Pneumonia - Streptococcus Pneumoniae",
      cid == "J15" ~ "Pneumonia bacteriana não classificada",
      cid == "J18" ~ "Pneumonia por microrganismo n.e.",
      cid == "J20" ~ "Bronquite aguda",
      cid == "J21" ~ "Bronquiolite Aguda",
      cid == "J22" ~ "Infecções agudas n.e. VAI",
      cid == '999' | cid == '0' ~ "ignorado",
      is.na(cid) ~ "ignorado",
      TRUE ~ NA_character_))|>
  mutate(
    subgrupo = case_when(
      cid == "J03" ~ "Infecções agudas das VAS",
      cid == "J04" ~ "Infecções agudas das VAS",
      cid == "J05" ~ "Infecções agudas das VAS",
      cid == "J06" ~ "Infecções agudas das VAS",
      cid == "J09" ~ "Influenza [gripe] e pneumonia",
      cid == "J10" ~ "Influenza [gripe] e pneumonia",
      cid == "J11" ~ "Influenza [gripe] e pneumonia",
      cid == "J12" ~ "Influenza [gripe] e pneumonia",
      cid == "J13" ~ "Influenza [gripe] e pneumonia",
      cid == "J15" ~ "Influenza [gripe] e pneumonia",
      cid == "J18" ~ "Influenza [gripe] e pneumonia",
      cid == "J20" ~ "Outras infecções agudas das VAI",
      cid == "J21" ~ "Outras infecções agudas das VAI",
      cid == "J22" ~ "Outras infecções agudas das VAI",
      cid == '999' | cid == '0' ~ "ignorado",
      is.na(cid) ~ "ignorado",
      TRUE ~ NA_character_))|>
  mutate(
    causa = case_when(
      diag_princ == "J039" ~ "Amigdalite aguda n.e.",
      diag_princ == "J040" ~ "Laringite aguda",
      diag_princ == "J041" ~ "Traqueíte aguda",
      diag_princ == "J042" ~ "Laringotraqueíte aguda",
      diag_princ == "J050" ~ "Laringite obstrutiva aguda (crupe)",
      diag_princ == "J051" ~ "Epiglotite aguda",
      diag_princ == "J069" ~ "Infecção aguda das VAS n.e.",
      diag_princ == "J09"  ~ "Influenza (gripe aviária)",
      diag_princ == "J100" ~ "Influenza com pneumonia outro vírus da influenza",
      diag_princ == "J108" ~ "Influenza com outras manifestações, outro vírus da influenza",
      diag_princ == "J110" ~ "Influenza com pneumonia - vírus n.id.",
      diag_princ == "J111" ~ "Influenza com outras manifestações respiratórias - vírus n.id.",
      diag_princ == "J118" ~ "Influenza com outras manifestações - vírus n.id.",
      diag_princ == "J121" ~ "Pneumonia - VSR ",
      diag_princ == "J128" ~ "Outras pneumonias virais",
      diag_princ == "J129" ~ "Pneumonia viral n.e.",
      diag_princ == "J13"  ~ "Pneumonia - Streptococcus Pneumoniae",
      diag_princ == "J150" ~ "Pneumonia - Klebsiella Pneumoniae",
      diag_princ == "J151" ~ "Pneumonia - Pseudomonas",
      diag_princ == "J152" ~ "Pneumonia - Staphylococcus",
      diag_princ == "J153" ~ "Pneumonia - Streptococcus do Grupo B",
      diag_princ == "J154" ~ "Pneumonia - Outros Estreptococos",
      diag_princ == "J156" ~ "Pneumonia - Outras Bactérias Aeróbicas Gram-",
      diag_princ == "J158" ~ "Outras pneumonias bacterianas",
      diag_princ == "J159" ~ "Pneumonia bacteriana n.e.",
      diag_princ == "J180" ~ "Broncopneumonia n.e.",
      diag_princ == "J181" ~ "Pneumonia Lobar n.e.",
      diag_princ == "J182" ~ "Pneumonia Hipostática n.e.",
      diag_princ == "J188" ~ "Outras Pneumonias devidas a microrganismos n.esp.",
      diag_princ == "J189" ~ "Pneumonia n.e.",
      diag_princ == "J200" ~ "Bronquite aguda - Mycoplasma Pneumoniae",
      diag_princ == "J208" ~ "Bronquite aguda devida a outros microrganismos especificados",
      diag_princ == "J209" ~ "Bronquite aguda n.e.",
      diag_princ == "J210" ~ "Bronquiolite aguda devida a VSR",
      diag_princ == "J218" ~ "Bronquiolite aguda devida a outros microrganismos especificados",
      diag_princ == "J219" ~ "Bronquite aguda n.e.",
      diag_princ == "J22"  ~ "Infecções agudas n.e. das VAI",
      diag_princ == '999' | diag_princ == '0' ~ "ignorado",
      is.na(diag_princ) ~ "ignorado",
      TRUE ~ NA_character_))

## n.e. = Não especificada, n.id. = Não identificado, VSR = virus sincicial respiratório, 
## n.esp. = não especificados, VAS = vias aéreas superiores, VAI = vias aéreas inferiores

## 2.11 Identificar os municipios de internacao com maior numero de casos de internacao por bronquiolite
table(sih$munic_mov) %>% 
  as.data.frame() %>% 
  arrange(desc(Freq))
# 230440 (Fortaleza) = 6099
# 261160 (Recife) = 3979
# 292740 (Salvador) = 2757

## 2.12 Criar variavel de mes - factor ##
sih$mes_interna <- factor(month(sih$dt_inter), levels = c(1:12),
                           labels = c("Jan", "Feb","Mar","Apr","May",
                                      "Jun","Jul","Aug","Sep","Oct",
                                      "Nov","Dec"))
table(sih$mes_interna)
class(sih$mes_interna)

## 2.13 Selecionar os 3 municipios com maior numero de hospitalizacoes e menores de 5 anos
# Fortaleza excluida por missing data nas variaveis climaticas #
table(sih$idade_anos)
sih_ap <- sih[which((sih$munic_mov =='292740' | sih$munic_mov =='261160')),]

## 2.14 Criar variavel de municipio de internacao - factor
table(sih_ap$munic_res)
table(sih_ap$munic_mov)
sih_ap$munic_interna <- factor(sih_ap$munic_mov, 
                               levels = c("261160", "292740"),
                               labels = c("Recife", "Salvador"))

table(sih_ap$munic_interna)


## 2.15 Criar data.frame com # internacoes por mes por municipio
tab1 <- as.data.frame(table(sih_ap$mes_interna, sih_ap$munic_interna,
              dnn = c("Mes", "Municipio"))) # Tabela para discussao
data.frame(tab1)
class(tab1)

## 2.16 Grafico com o numero de internacoes por mes por municipio
grf_sih <- ggplot(tab1, aes(x = Mes, y = Freq, group = Municipio,
                            colour = Municipio)) +
  geom_line() + 
  xlab("Mes de internacao") + 
  ylab("Numero de internacoes")
grf_sih

####################################################################
# 3. Preparando os dados de clima - todos os anos; Regiao Nordeste #
####################################################################

head(clima)
clima <- clean_names(clima) 
table(clima$state)
colnames(clima)

## 3.1 Renomeando as variaveis climaticas
clima <- clima |> 
  mutate(precip = as.numeric(precipitacao_total_horario_mm)) |>
  mutate(pa_min = as.numeric(pressao_atmosferica_min_na_hora_ant_aut_m_b)) |>
  mutate(pa_max = as.numeric(pressao_atmosferica_max_na_hora_ant_aut_m_b)) |>
  mutate(radiacao = as.numeric(radiacao_global_kj_m)) |>
  mutate(t_min = as.numeric(temperatura_minima_na_hora_ant_aut_c)) |>
  mutate(t_max = as.numeric(temperatura_maxima_na_hora_ant_aut_c)) |>
  mutate(umid_min = as.numeric(umidade_rel_min_na_hora_ant_aut)) |>
  mutate(umid_max = as.numeric(umidade_rel_max_na_hora_ant_aut)) |>
  mutate(vento_vel = as.numeric(vento_velocidade_horaria_m_s)) 
  

## 3.2. Convertendo os valores -9999 das variaveis climaticas para NA
clima <- clima |> 
  mutate(across(where(is.numeric), ~na_if(., -9999)))
summary(clima)

## 3.3. Criando a variavel de mes e ano a partir da data
clima$month <- factor(month(clima$data), levels = c(1:12),
                      labels = c("Jan", "Feb","Mar","Apr","May",
                                 "Jun","Jul","Aug","Sep","Oct",
                                 "Nov","Dec"))
clima$year <- year(clima$data)

## 3.4. Criando as variaveis de temperatura, pressao atmosferica e umidade relativa media
clima <- clima %>% 
  mutate(t_media = rowMeans(across(c(t_min,t_max)), na.rm = TRUE)) |>
  mutate(umid_media = rowMeans(across(c(umid_min,umid_max)), na.rm = TRUE)) |>        
  mutate(pa_media = rowMeans(across(c(pa_min,pa_max)), na.rm = TRUE))       

## 3.5 Selecionando stations: Salvador, Recife e Fortaleza & ano = 2019
clima_ap <- clima[which((clima$station =='SALVADOR' | clima$station =='RECIFE' | 
                           clima$station =='FORTALEZA') & clima$year == 2019),]

## 3.6. Specify data frame
group_mean <- clima_ap %>%
  group_by(station, month) %>%
  summarise_at(vars(c(precip, t_media, umid_media, pa_media, vento_vel, radiacao)),
               list(Mean_Frequency = mean), na.rm = TRUE)
# Fortaleza does not have any measure in March and April and will be excluded

fortaleza <- clima_ap[c(clima_ap$station == "FORTALEZA" & 
                          clima_ap$month == "Mar"), ]
view(fortaleza)

## 3.7. Selecionando stations: Salvador, Recife e Fortaleza & ano = 2019
clima_ap <- clima[which((clima$station =='SALVADOR' | clima$station =='RECIFE') 
                        & clima$year == 2019),]

table(clima_ap$station)

## 3.8. Media de temp, umidade e pa e velocidade do vento e radiacao para grafico
clima_mean <- clima_ap %>%
  group_by(station, month) %>%
  summarise_at(vars(c(precip, t_media, umid_media, pa_media, vento_vel, radiacao)),
               list(Mean_Frequency = mean), na.rm = TRUE)
view(clima_mean)
colnames(clima_mean)

## 3.9. Graficos de variaveis climaticas por mes por municipio

# Precipitacao (mm)
grf_precip <- ggplot(clima_mean, aes(x = month, y = precip_Mean_Frequency, group = station,
                                   colour = station)) +
  geom_line() + 
  xlab("Mes de internacao") + 
  ylab("Precipitacao media (mm)")
grf_precip

# Temperatura
grf_temp <- ggplot(clima_mean, aes(x = month, y = t_media_Mean_Frequency, group = station,
                            colour = station)) +
  geom_line() + 
  xlab("Mes de internacao") + 
  ylab("Temperatura media (\u00B0C)")
grf_temp

# Pressao atmosferica
grf_pa <- ggplot(clima_mean, aes(x = month, y = pa_media_Mean_Frequency, group = station,
                                   colour = station)) +
  geom_line() + 
  xlab("Mes") + 
  ylab("Pressao atmosferica (mb)")
grf_pa

# Radiacao
grf_ra <- ggplot(clima_mean, aes(x = month, y = radiacao_Mean_Frequency, group = station,
                                 colour = station)) +
  geom_line() + 
  xlab("Mes") + 
  ylab("Radiacao media mensal (Kj/m")
grf_ra

# Umidade relativa
grf_umid <- ggplot(clima_mean, aes(x = month, y = umid_media_Mean_Frequency, group = station,
                                 colour = station)) +
  geom_line() + 
  xlab("Mes") + 
  ylab("Umidade relativa (%)")
grf_umid

# Vento
grf_vento <- ggplot(clima_mean, aes(x = month, y = vento_vel_Mean_Frequency, group = station,
                                   colour = station)) +
  geom_line() + 
  xlab("Mes") + 
  ylab("Velocidade do vento (m/s)")
grf_vento

## 3.9. Exportar o novo banco de dados de clima:
# station = Salvador, Recife 
# Ano = 2019
write.csv(clima_ap, file = "C:/Users/RenataYokota/Documents/Other/Projeto Cnpq/Curso Fiocruz/Base de dados/INMET/clima.csv", row.names = FALSE)

###########################################################
# 4. Preparando banco de SRAG - 2019; todos os municipios #
###########################################################
head(srag)
table(srag$cod_idade)
class(srag$dt_nasc)

## 4.1. Excluir simbolos e transformar em letras minúsculas os nomes das variáveis
srag <-clean_names(srag) 

## 4.2. Criando idade em anos - data nascimento - data internacao
#srag$dt_nasc <- ymd(srag$dt_nasc)
#srag$idade <- difftime(srag$dt_interna, srag$dt_nasc)

## 4.3. Subseting o banco com menores de 5 anos 
srag_5 <- srag[which((srag$cod_idade >= 3000 & srag$cod_idade < 3005) & 
                 (srag$co_mu_inte == 261160 | srag$co_mu_inte == 292740)),]
dim(srag_5)
table(srag_5$cod_idade) # n = 1065

## 4.4. Criando a variavel de mes de internacao
srag_5$dt_interna <- as.Date(srag_5$dt_interna)
srag_5$mes_interna <- factor(month(srag_5$dt_interna), levels = c(1:12),
                      labels = c("Jan", "Feb","Mar","Apr","May",
                                 "Jun","Jul","Aug","Sep","Oct",
                                 "Nov","Dec"))
table(srag_5$mes_interna)

## 4.5 Criar variavel de municipio de internacao - factor
srag_5$munic_interna <- factor(srag_5$co_mu_inte, 
                               levels = c("261160", "292740"),
                               labels = c("Recife", "Salvador"))
table(srag_5$munic_intern)

## 4.6 Criar data.frame com # internacoes por mes por municipio
tab2 <- as.data.frame(table(srag_5$mes_interna, srag_5$munic_interna,
                            dnn = c("Mes", "Municipio"))) # Tabela para discussao
data.frame(tab2)
class(tab2)

## 4.7 Grafico com o numero de internacoes por mes por municipio
grf_srag <- ggplot(tab2, aes(x = Mes, y = Freq, group = Municipio,
                            colour = Municipio)) +
  geom_line() + 
  xlab("Mes de internacao") + 
  ylab("Numero de internacoes")
grf_srag

###########################################################
# 5. Preparando banco de SIM - 2016-2019; Regiao Nordeste #
###########################################################
head(sim)
dim(sim)
table(sim$idade)

## 5.1. Excluir simbolos e transformar em letras minúsculas os nomes das variáveis
sim <- clean_names(sim) 
sim$dtobito <- as.Date(sim$dtobito)
sim$ano_obt <- year(sim$dtobito)
sim$mes_obt <- factor(month(sim$dtobito), levels = c(1:12),
       labels = c("Jan", "Feb","Mar","Apr","May",
                  "Jun","Jul","Aug","Sep","Oct",
                  "Nov","Dec"))

table(sim$ano_obt)
table(sim$mes_obt)

## 5.2. Subseting o banco com ano do obito = 2019 e municipio ocorrencia = Salvador e Recife
sim19 <- sim[which((sim$ano_obt == 2019) & (sim$codmunocor == 261160 | sim$codmunocor == 292740)),]
dim(sim19)
table(sim19$) # n = 1065

## 5.3.Criar variavel de municipio de internacao - factor
sim19$munic_interna <- factor(sim19$codmunocor, 
                               levels = c("261160", "292740"),
                               labels = c("Recife", "Salvador"))
table(sim19$munic_interna)

## 5.4 Criar data.frame com # internacoes por mes por municipio
tab3 <- as.data.frame(table(sim19$mes_obt, sim19$munic_interna,
                            dnn = c("Mes", "Municipio"))) # Tabela para discussao
data.frame(tab3)
class(tab3)

## 5.5. Grafico com o numero de internacoes por mes por municipio
grf_sim <- ggplot(tab3, aes(x = Mes, y = Freq, group = Municipio,
                             colour = Municipio)) +
  geom_line() + 
  xlab("Mes de internacao") + 
  ylab("Numero de internacoes")
grf_sim

###########################
# 6. Merging the datasets #
###########################

# Clima
clima <- clima_mean[order(clima_mean$station, clima_mean$month),]

clima <- clima |> 
  mutate(Municipio = station)|>
  mutate(Mes = month)|>
  mutate(precip = precip_Mean_Frequency)|>
  mutate(temp = t_media_Mean_Frequency)|>
  mutate(umid = umid_media_Mean_Frequency)|>
  mutate(pa = pa_media_Mean_Frequency)|>
  mutate(vento = vento_vel_Mean_Frequency)|>
  mutate(radiacao = radiacao_Mean_Frequency)

clima2 <- clima %>% 
  select(c("Municipio", "Mes", "precip", "temp", "umid", "pa", "vento",
           "radiacao"))
clima2 <- as.data.frame(clima2)

## SRAG
srag <- tab2[order(tab2$Municipio, tab2$Mes),]
srag$n_srag <- srag$Freq

## SIH
sih <- tab1[order(tab1$Municipio, tab1$Mes),]
sih$n_interna <- sih$Freq

joined_data <- cbind(clima2, srag[,"n_srag"], sih[, "n_interna"])
colnames(joined_data)                                                  
names(joined_data)[names(joined_data) == "srag[, \"n_srag\"]"] <- "n_srag"
names(joined_data)[names(joined_data) == "sih[, \"n_interna\"]"] <- "n_interna"

## Saving the merged dataset for the correlation and regression analysis ##
write.csv(joined_data, file = "C:/Users/RenataYokota/Documents/Other/Projeto Cnpq/Curso Fiocruz/Base de dados/merged_data.csv", row.names = FALSE)
