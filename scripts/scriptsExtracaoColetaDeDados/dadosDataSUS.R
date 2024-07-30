# remotes::install_github("rfsaldanha/microdatasus")

library(microdatasus)
library(tidyverse)

# Sistema de Informação sobre Mortalidade de Minas Gerais de 2019

data_sim_mg <- fetch_datasus(year_start = 2019,
                             year_end = 2019,
                             uf = "MG",
                             information_system = "SIM-DO")

# Sistema de informação Hospital Descentralizada de Minas Gerais de 2019
data_sih_mg <- fetch_datasus(year_start = 2019,
                             year_end = 2019,
                             month_start = 1,
                             month_end = 12,
                             uf = "MG",
                             information_system = "SIH-RD")

# Sistema de informações sobre Nascidos Vivos de Minas Gerais de 2019

data_sinasc_mg <- fetch_datasus(year_start = 2019,
                                year_end = 2019,
                                uf = "MG",
                                information_system = "SINASC")


# Trata os dados do SIM

sim_mg <- process_sim(data_sim_mg)

# Trata os dados do SIH

sih_mg <- process_sih(data_sih_mg)

# Trata os dados do SIA

sinasc_mg <- process_sinasc(data_sinasc_mg)

# Seleciona os dados de interesse: Estado civil da mãe e Sexo do bebê

sinasc <- sinasc_mg %>%
  select(ESTCIVMAE, SEXO) %>%
  na.omit()

# Visualiza os número de Mães por Estado Civil

sinasc %>%
  count(ESTCIVMAE) %>%
  ggplot(aes(x = ESTCIVMAE, y = n,
             fill = ESTCIVMAE,
             color = ESTCIVMAE,
             label = n))+
  geom_bar(stat = "identity")+
  geom_label(color = "black")+
  labs(title = "Estado Civil das Mães de Nascidos em Minas Gerais",
       subtitle = "ano de 2019",
       x = "",
       y = "",
       caption = "Elaborado por analisemacro.com.br com dados do DATASUS")+
  theme_minimal()+
  theme(legend.position = "none")

# Visualiza o número de bebês por sexo

sinasc %>%
  count(SEXO) %>%
  ggplot(aes(x = SEXO, y = n,
             fill = SEXO,
             color = "SEXO",
             label = n))+
  geom_bar(stat = "identity")+
  geom_label(color = "black")+
  labs(title = "Sexo dos bebês nascidos em Minas Gerais",
       subtitle = "ano de 2019",
       x = "",
       y = "",
       caption = "Elaborado por analisemacro.com.br com dados do DATASUS")+
  theme_minimal()+
  theme(legend.position = "none")

# Referência: https://analisemacro.com.br/data-science/dicas-de-rstats/hackeando-o-r-acessando-os-dados-do-datasus-com-o-r/