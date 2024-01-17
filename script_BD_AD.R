## Autor: Anderson Henrique (UFPE) ###


##Artigo: "O MELHOR REMÉDIO SERIA PREVENIR?
##MAPEANDO AS POLÍTICAS DE PREVENÇÃO DE RISCO E DESASTRES NO BRASIL (2012 – 2019)"

# Definir diretorio
setwd("~/Desktop/base")

# Abrir banco de dados necessarios
library(tidyverse)
library(rio)
library(here)


### Construir banco de dados ####

# Abrir banco sde dados

dados2012 <- import(here("data", "base_selec_2013.xlsx"))
dados2016 <- import(here("data", "base_selec_2017.xlsx"))
dados2019 <- import(here("data", "base_selec_2020.xlsx"))
ibge <- import(here("data", "ibge_cod.xlsx"))

# Adicionar ano as bases

dados2012$ano <- 2012
dados2016$ano <- 2016
dados2019$ano <- 2019

# Retirar variaveis

dados2012$A392 <- NULL
dados2012$A395 <- NULL
dados2016$`COD UF` <- NULL
dados2019$`Cod UF` <- NULL
dados2019$Mgrd179 <- NULL
dados2016$`POP EST` <-NULL
dados2019$Faixa_pop <- NULL


# Modiificar nomes das variaveis

dados2012 <- dados2012 %>%
  rename(id_munic_6 = A1,
         id_estado = A391,
         reg = A393,
         mun = A394,
         plano_diretor_met =  A182,
         lei_prev_met = A184,
         plano_mun_rr = A189,
         cb = A220,
         compdec = A221,
         nudecs = A225 
  )


dados2016 <- dados2016 %>%
  rename(id_munic_7 = CodMun,
         estado = UF,
         reg = REGIAO,
         mun = MUNIC,
         plano_diretor_met =  MGRD171,
         lei_prev_met = MGRD173,
         plano_mun_rr = MGRD177,
         cb = MGRD201,
         compdec = MGRD202,
         nudecs = MGRD203
  )


dados2019 <- dados2019 %>%
  rename(id_munic_7 = CodMun,
         estado = UF,
         reg = Regiao,
         mun = Mun,
         plano_diretor_met =  Mgrd171,
         lei_prev_met = Mgrd173,
         plano_mun_rr = Mgrd177,
         cb = Mgrd211,
         compdec = Mgrd212,
         nudecs = Mgrd213
  )


# Adicionar coluna de "Estado" e "IBGE 7" no banco de 2012


join_dados2012 <- full_join(dados2012, ibge, by = "id_munic_6")


# Selecionar variaveis de interesse 

dados2012_f <- join_dados2012 %>%
  select(id_munic_7, estado_abrev, reg, mun , plano_diretor_met ,lei_prev_met, plano_mun_rr, cb,compdec, nudecs)

#Adicionar ano

dados2012_f$ano<- 2012

#Renomear nome do estado na base de '2012'

dados2012_f <- rename(dados2012_f, estado = estado_abrev)



# Visualizar nomes das colunas

colnames(dados2012_f)
colnames(dados2016)
colnames(dados2019)

# Juntar bancos de dados em apenas um

dados_res <- bind_rows(dados2012_f, dados2016, dados2019)


### Organizar banco de dados ####

# Bancos para gerenciar

if(require(abjutils) == F) install.packages('abjutils'); require(abjutils) #Funcao para remover acentos e colocar em caspslock


# Organizar os nomes da variavel "municipio'

#Tirar acentos
dados_res$mun <- rm_accent(dados_res$mun)

# Colocar todos os nomes em maiusculo

dados_res$mun <- toupper(dados_res$mun)


# Renomear e agrupar variáveis

# Ver as categorias existentes em "regiao'

table(dados_res$reg)

# Renomear
dados_res <- dados_res %>%
  mutate(regiao = case_when( reg == '1 - Norte'~ 'Norte',
                    reg == '2 - Nordeste' ~ 'Nordeste',
                    reg == '3 - Sudeste' ~  'Sudeste',
                    reg == '4 - Sul' ~ 'Sul',
                    reg =='5 - Centro-Oeste' ~ 'Centro-Oeste'
    
  ))


# Ver as categorias existentes em "plano_diretor_met'

table(dados_res$plano_diretor_met)


# Reagrupar
dados_res <- dados_res %>%
  mutate(plano_diretor_met_f = case_when( plano_diretor_met == 'Não'~ 'Não',
                                          plano_diretor_met == 'Sim' ~ 'Sim',
                                          plano_diretor_met == 'Nenhum instrumento' ~  'Não',
                                          plano_diretor_met == 'Não informou' ~ 'Sem Informação',
                                          plano_diretor_met =='Recusa' ~ 'Sem Informação',
                                          plano_diretor_met == '-' ~ 'Sem Informação'
                             
  ))



# Ver as categorias existentes em "plano_diretor_met'

table(dados_res$lei_prev_met)


# Reagrupar
dados_res <- dados_res %>%
  mutate(lei_prev_met_f = case_when( lei_prev_met == 'Não'~ 'Não',
                                     lei_prev_met == 'Sim' ~ 'Sim',
                                     lei_prev_met == 'Não aplicável' ~  'Não',
                                     lei_prev_met == 'Não informou' ~ 'Sem Informação',
                                     lei_prev_met =='Recusa' ~ 'Sem Informação',
                                     lei_prev_met == '-' ~ 'Sem Informação'
                                          
  ))


# Ver as categorias existentes em "plano_mun_rr'

table(dados_res$plano_mun_rr)


# Reagrupar
dados_res <- dados_res %>%
  mutate(plano_mun_rr_f = case_when( plano_mun_rr == 'Não'~ 'Não',
                                     plano_mun_rr == 'Sim' ~ 'Sim',
                                     plano_mun_rr == 'Não aplicável' ~  'Não',
                                     plano_mun_rr == 'Não informou' ~ 'Sem Informação',
                                     plano_mun_rr =='Recusa' ~ 'Sem Informação',
                                     plano_mun_rr == '-' ~ 'Sem Informação'
                                     
  ))


# Ver as categorias existentes em "CB'

table(dados_res$cb)


# Reagrupar
dados_res <- dados_res %>%
  mutate(cb_f = case_when( cb == 'Não'~ 'Não',
                           cb == 'Sim' ~ 'Sim',
                           cb == 'Não aplicável' ~  'Não',
                           cb == 'Não informou' ~ 'Sem Informação',
                           cb =='Recusa' ~ 'Sem Informação',
                           cb == '-' ~ 'Sem Informação'
                                     
  ))

# Ver as categorias existentes em "compdec'

table(dados_res$compdec)


# Reagrupar
dados_res <- dados_res %>%
  mutate(compdec_f = case_when( compdec == 'Não'~ 'Não',
                                compdec == 'Sim' ~ 'Sim',
                                compdec == 'Não aplicável' ~  'Não',
                                compdec == 'Não informou' ~ 'Sem Informação',
                                compdec =='Recusa' ~ 'Sem Informação',
                                compdec == '-' ~ 'Sem Informação'
                           
  ))



# Ver as categorias existentes em "nudecs'

table(dados_res$nudecs)


# Reagrupar
dados_res <- dados_res %>%
  mutate(nudecs_f = case_when( nudecs == 'Não'~ 'Não',
                               nudecs == 'Sim' ~ 'Sim',
                               nudecs == 'Não aplicável' ~  'Não',
                               nudecs == 'Não informou' ~ 'Sem Informação',
                               nudecs =='Recusa' ~ 'Sem Informação',
                               nudecs == '-' ~ 'Sem Informação'
                                
  ))


# Refinir Ano como categoria

dados_res$ano <- as.character(dados_res$ano)

# Remover variaveis antigas

dados_res <- dados_res %>%
  select(-c(3, 5, 6, 7, 8, 9, 10)) #indica a posicao que serao retiradas


#Remover objetos desnecessarios

rm(list = c('dados2012','dados2012_f', 'dados2016', 'dados2019', 'join_dados2012'))

# Salvar bancos de dados
saveRDS(dados_res, "dados_res.rdata")


### Analise de Dados - Descritivo ####

## Analise descritiva das variaveis do banco

# Abrir pacote para agrupar graficos

if(require(ggpubr) == F) install.packages('ggpubr'); require(ggpubr)

# Etapa 1: Variaveis de 'Instrumentos Estruturais de Prevenção'

## Total
## Plano Diretor Municipal de Enchentes e Enxuradas
a1 <- dados_res %>%
  select(plano_diretor_met_f)  %>%
  group_by(plano_diretor_met_f) %>%
  summarise(n=n()) %>%
  mutate(percent = n/sum(n) * 100) %>% 
  arrange(desc(percent))
a1

## Lei de Especifica de Prevencao de Enchentes e Enxuradas
a2 <- dados_res %>%
  select(lei_prev_met_f)  %>%
  group_by(lei_prev_met_f) %>%
  summarise(n=n()) %>%
  mutate(percent = n/sum(n) * 100) %>% 
  arrange(desc(percent))
a2

## Plano Municipal de Reducao de Risco
a3 <-dados_res %>%
  select(plano_mun_rr_f)  %>%
  group_by(plano_mun_rr_f) %>%
  summarise(n=n()) %>%
  mutate(percent = n/sum(n) * 100) %>% 
  arrange(desc(percent))
a3



#Graficos de linha das variaveis de "Estrumentos Estruturais de Prevencao" no tempo

## Plano Diretor Municipal de Enchentes e Enxuradas

g1 <- dados_res %>%
  select(ano,plano_diretor_met_f)  %>%
  group_by(ano,plano_diretor_met_f) %>%
  summarise(n=n()) %>%
  mutate(percent = n/sum(n) * 100) %>% 
  arrange(desc(percent)) %>%
  ggplot(aes(ano, percent, group=plano_diretor_met_f, color=plano_diretor_met_f)) + 
  geom_line() +
  labs(title = "Gráfico 4", color = 'Existência de P.D.M. de \nEnchentes e Enxuradas:', group = 'variavel', 
       x = 'Ano', y = 'Porcentagem (%)') 


g1
## Lei de Especifica de Prevencao de Enchentes e Enxuradas

g2 <- dados_res %>%
  select(ano,lei_prev_met_f)  %>%
  group_by(ano,lei_prev_met_f) %>%
  summarise(n=n()) %>%
  mutate(percent = n/sum(n) * 100) %>% 
  arrange(desc(percent)) %>%
  ggplot(aes(ano, percent, group=lei_prev_met_f, color=lei_prev_met_f)) + 
  geom_line() +
  labs(title = "Gráfico 5",color = 'L.E. de Prevencao de \nEnchentes e Enxuradas:', group = 'variavel', 
       x = 'Ano', y = 'Porcentagem (%)') 


## Plano Municipal de Reducao de Risco

g3 <- dados_res %>%
  select(ano,plano_mun_rr_f)  %>%
  group_by(ano,plano_mun_rr_f) %>%
  summarise(n=n()) %>%
  mutate(percent = n/sum(n) * 100) %>% 
  arrange(desc(percent)) %>%
  ggplot(aes(ano, percent, group=plano_mun_rr_f, color=plano_mun_rr_f)) + 
  geom_line() +
  labs(title = "Gráfico 6",color = 'Plano Municipal de \nReducao de Risco:', group = 'variavel', 
       x = 'Ano', y = 'Porcentagem (%)') 


# Agregar os gráficos
# Dimensões: 950 largura e 650 comprimento

junto <- ggarrange(g1, g2, g3, common.legend = FALSE, align = "v",
                     legend = "top") 

annotate_figure(junto)



## Análise: Desagregado por:  Ano

## Plano Diretor Municipal de Enchentes e Enxuradas
dados_res %>%
  select(ano, plano_diretor_met_f)  %>%
  group_by(ano, plano_diretor_met_f) %>%
  summarise(n=n()) %>%
  mutate(percent = n/sum(n) * 100)

## Lei de Especifica de Prevencao de Enchentes e Enxuradas
dados_res %>%
  select(ano, lei_prev_met_f)  %>%
  group_by(ano, lei_prev_met_f) %>%
  summarise(n=n()) %>%
  mutate(percent = n/sum(n) * 100)


## Plano Municipal de Reducao de Risco
dados_res %>%
  select(ano, plano_mun_rr_f)  %>%
  group_by(ano, plano_mun_rr_f) %>%
  summarise(n=n()) %>%
  mutate(percent = n/sum(n) * 100)




# Etapa 2: Variaveis de 'Instrumentos Burocráticos '


#Bombeiros
a4<- dados_res %>%
  select(ano, cb_f)  %>%
  group_by(cb_f) %>%
  summarise(n=n()) %>%
  mutate(percent = n/sum(n) * 100) %>% 
  arrange(desc(percent))

a4

#COMPDEC

a5 <- dados_res %>%
  select(ano, compdec_f)  %>%
  group_by(compdec_f) %>%
  summarise(n=n()) %>%
  mutate(percent = n/sum(n) * 100) %>% 
  arrange(desc(percent))

a5

#Nudecs

a6 <-dados_res %>%
  select(ano, nudecs_f)  %>%
  group_by(nudecs_f) %>%
  summarise(n=n()) %>%
  mutate(percent = n/sum(n) * 100) %>% 
  arrange(desc(percent))

a6

## Grafico de barra dos "Instrumentos Burocráticos"

#Unidade de Corpo de Bombeiro

g4 <-a4 %>% 
  ggplot(aes(cb_f, percent, fill=cb_f)) + 
  geom_bar(stat="identity", width=.5, position = "dodge") +
  labs(fill = 'Unidade de Corpo de Bombeiro:', 
       x = 'Categorias', y = 'Porcentagem (%)') +
  ggtitle("Gráfico 1") +
  coord_cartesian(ylim=c(0,100)) 

g4

#COMPDEC
g5 <- a5 %>% 
  ggplot(aes(compdec_f, percent, fill=compdec_f)) + 
  geom_bar(stat="identity", width=.5, position = "dodge") +
  labs(fill = 'CMP e Defesa Civil ou órgão similares:', 
       x = 'Categorias', y = 'Porcentagem (%)') +
  ggtitle("Gráfico 2") +
  coord_cartesian(ylim=c(0,100)) 


#Nucleo de Defesa Civil
g6 <- a6  %>% 
  ggplot(aes(nudecs_f, percent, fill=nudecs_f)) + 
  geom_bar(stat="identity", width=.5, position = "dodge") +
  labs(fill = 'Nucleo de Defesa Civil:', 
       x = 'Categorias', y = 'Porcentagem (%)') +
   ggtitle("Gráfico 3") +
  coord_cartesian(ylim=c(0,100)) 


# Agregar os gráficos
# Dimensões: 1000 largura e 600 comprimento

junto2 <- ggarrange(g4, g5, g6, common.legend = FALSE, align = "v",
                   legend = "top") 

annotate_figure(junto2)


### Analise Espacial ####

# Remover objetos desnecessarios

rm(list = c('a1','a2', 'a3', 'a4', 'a5', "a6", 
            "ibge","g1", "g2", "g3", "g4", "g5", "g6", "junto", "junto2"))

# Abrir pacotes necessarios

if(require(tidyverse) == F) install.packages('tidyverse'); require(tidyverse)
if(require(readxl) == F) install.packages('readxl'); require(readxl)
if(require(geobr)== F) install.packages("geobr"); require(geobr)
if(require(here) == F) install.packages('here'); require(here)
if(require(sf) == F) install.packages('sf'); require(sf)
if(require(ggpubr) == F) install.packages('ggpubr'); require(ggpubr)
if(require(rgeos) == F) install.packages("rgeos"); require(rgeos)
if(require(rgdal) == F) install.packages("rgdal"); require(rgdal)
if(require(maptools) == F) install.packages("maptools"); require(maptools)
if(require(ggmap) == F) install.packages("ggmap"); require(ggmap)


#Separar o banco por anos

dados_res_2012 <- dados_res %>%
  filter(ano == "2012")

dados_res_2016 <- dados_res %>%
  filter(ano == "2016")


dados_res_2019<- dados_res %>%
  filter(ano == "2019")


# Criar mapa

## Obs: Pode acontecer do pacote dá "Timeout"  ou demorar alguns minutos. Acontece! 
# Se isso acontecer recomenda-se reiniciar o R e esperar. Repita sempre que isso acontecer, pois e um problema do servidor do IPEA.


# Baixar o arquivo de shapefile de todos os municipios (pode ser estado tbm, tem que ver no pacote, mas a logica e a mesma daqui)
all_muni <- read_municipality(code_muni = "all", year= 2020) # pra estados seria "all_state <- read_state(code_state = "all", year =2010)"

#Aí onde for "all_muni" substitui por "all_state" ou o nome que voce quiser dar.

#visualizar o shapefile
all_muni %>% ggplot() +
  geom_sf(fill='blue', color='black') +
  theme_minimal()

# Ver as variaveis presentes no shapefile
tibble(all_muni)


#Juntar a base de dados "dados_res"  e do shapefile
mapa2012 <- left_join(dados_res_2012, all_muni, by = c("id_munic_7" = "code_muni"))
mapa2016 <- left_join(dados_res_2016, all_muni, by = c("id_munic_7" = "code_muni"))
mapa2019 <- left_join(dados_res_2019, all_muni, by = c("id_munic_7" = "code_muni"))


## Observacao: para a dimensao dos mapas ficar mais visivel nas imagens, recomenda-se estabelecer
## a o comprimento = 2000 e altura = 2500 ou superior a esses valores.

# Produzir o mapa de "Plano Diretor Municipal de Enchentes e Enxuradas"

a4 <- mapa2012 %>%
  ggplot() +
  geom_sf( aes(fill=plano_diretor_met_f, geometry=geom), color= 'black', size=.20) +
  labs(title="Plano Diretor Municipal de Enchentes e Enxuradas em 2012", fill='Existência:',  size=20) +
  theme_minimal() 
# + scale_fill_gradient(low = "yellow", high = "red", na.value = NA)

a4

a5 <- mapa2016 %>%
  ggplot() +
  geom_sf( aes(fill=plano_diretor_met_f, geometry=geom), color= 'black', size=.20) +
  labs(title="Plano Diretor Municipal de Enchentes e Enxuradas em 2016", fill='Existência:',  size=20) +
  theme_minimal() 

a5


a6 <- mapa2019 %>%
  ggplot() +
  geom_sf( aes(fill=plano_diretor_met_f, geometry=geom), color= 'black', size=.20) +
  labs(title="Plano Diretor Municipal de Enchentes e Enxuradas 2019", fill='Existência:',  size=20) +
  theme_minimal() 

a6


#juntar Mapas

juntas1 <- ggarrange(a4, a5, a6, common.legend = TRUE, align = "v",
                    legend = "top") 

annotate_figure(juntas1)



# Produzir o mapa da "Lei de Especifica de Prevencao de Enchentes e Enxuradas"

a7 <- mapa2012 %>%
  ggplot() +
  geom_sf( aes(fill=lei_prev_met_f, geometry=geom), color= 'black', size=.20) +
  labs(title="Lei de Especifica de Prevencao de Enchentes e Enxuradas em 2012", fill='Existência:',  size=20) +
  theme_minimal() 
# + scale_fill_gradient(low = "yellow", high = "red", na.value = NA)

a7

a8 <- mapa2016 %>%
  ggplot() +
  geom_sf( aes(fill=lei_prev_met_f, geometry=geom), color= 'black', size=.20) +
  labs(title="Lei de Especifica de Prevencao de Enchentes e Enxuradas em 2016", fill='Existência:',  size=20) +
  theme_minimal() 

a8


a9 <- mapa2019 %>%
  ggplot() +
  geom_sf( aes(fill=lei_prev_met_f, geometry=geom), color= 'black', size=.20) +
  labs(title="Lei de Especifica de Prevencao de Enchentes e Enxuradas em 2019", fill='Existência:',  size=20) +
  theme_minimal() 


#juntar Mapas

juntas2 <- ggarrange(a7, a8, a9, common.legend = TRUE, align = "v",
                    legend = "top") 

annotate_figure(juntas2)



# Produzir o mapa de "Plano Municipal de Desastre"

a1 <- mapa2012 %>%
  ggplot() +
  geom_sf( aes(fill=plano_mun_rr_f, geometry=geom), color= 'black', size=.20) +
  labs(title="Plano Municipal de Desastre em 2012", fill='Existência:',  size=20) +
  theme_minimal() 
  # + scale_fill_gradient(low = "yellow", high = "red", na.value = NA)

a1

a2 <- mapa2016 %>%
  ggplot() +
  geom_sf( aes(fill=plano_mun_rr_f, geometry=geom), color= 'black', size=.20) +
  labs(title="Plano Municipal de Desastre em 2016", fill='Existência:',  size=20) +
  theme_minimal() 

a2


a3 <- mapa2019 %>%
  ggplot() +
  geom_sf( aes(fill=plano_mun_rr_f, geometry=geom), color= 'black', size=.20) +
  labs(title="Plano Municipal de Desastre em 2019", fill='Existência:',  size=20) +
  theme_minimal() 

a3


#juntar Mapas

juntas3 <- ggarrange(a1, a2, a3, common.legend = TRUE, align = "v",
                    legend = "top") 

annotate_figure(juntas3)



