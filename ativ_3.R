# Atividade 3

# 3.1 Acesso a bancos de dados abertos

# 3.1.1 Exemplo: Finding Dori
# A espécie-alvo será o peixe marinho Paracanthurus hepatus, também conhecido como Blue Tang e, mais recentemente como Dori!

# 3.1.2 GBIF
library(tidyverse)
library(rgbif)

# checar funcoes
?occ_data

# baixar ocorrencias
dori_gbif <- occ_data(scientificName = "Paracanthurus hepatus", 
                      hasCoordinate = TRUE,
                      hasGeospatialIssue=FALSE)

# dimensoes
dim(dori_gbif)

dim(dori_gbif$data)

# checar campos
dori_gbif$data %>% names

# 3.2 Problemas reportados

gbif_issues()

issues_gbif <- dori_gbif$data$issues %>%
  strsplit(., "[,]") %>%
  unlist() %>%
  unique()

dori_gbif1 <- dori_gbif$data %>%
  dplyr::select(scientificName, acceptedScientificName, decimalLatitude, decimalLongitude,
                issues, waterBody, basisOfRecord, occurrenceStatus, rightsHolder, 
                datasetName, recordedBy, depth, locality, habitat) 
dori_gbif1 <- dori_gbif1 %>% 
  distinct() 

# checar niveis dos fatores
lapply(dori_gbif1, unique)

# 3.3 Problemas não reportados

library(bdc) #install.packages("bdc")
library(CoordinateCleaner)

# checar coordenadas válidas
check_pf <- 
  bdc::bdc_coordinates_outOfRange(
    data = dori_gbif1,
    lat = "decimalLatitude",
    lon = "decimalLongitude")

# checar coordenadas válidas e próximas a capitais (muitas vezes as coordenadas são erroneamente associadas a capitais dos países)

cl <- dori_gbif1 %>%
  select(acceptedScientificName, decimalLatitude, decimalLongitude) %>%
  rename(decimallongitude = decimalLongitude,
         decimallatitude = decimalLatitude,
         scientificName = acceptedScientificName) %>% 
  as_tibble() %>% 
  mutate(val = cc_val(., value = "flagged"),
         sea = cc_sea(., value = "flagged"),
         capital = cc_cap(., value = "flagged"))

# verificar coordenadas com flags

# capitais (padrão é um raio de 10km)
cl %>% 
  rename(decimalLongitude = decimallongitude,
         decimalLatitude = decimallatitude) %>% 
  bdc::bdc_quickmap(., col_to_map = "capital")  

cl %>% 
  rename(decimalLongitude = decimallongitude,
         decimalLatitude = decimallatitude) %>% 
  bdc::bdc_quickmap(., col_to_map = "sea")  

# investigar niveis suspeitos
dori_gbif1 %>% 
  distinct(waterBody) %>% 
  pull()

# waterBody
dori_gbif1 %>%
  group_by(waterBody) %>% 
  summarise(occ = length(scientificName)) %>% 
  ggplot(aes(occ, y=waterBody)) +
  geom_bar(stat = 'identity') 

# fonte das regioes erradas
dori_gbif1 %>% 
  filter(waterBody %in% c("Atlantic Ocean", "Carribean", "Royal Caribbean", "Carribean Sea", "Bonaire")) %>% 
  distinct(datasetName)

# 25 ocorrencias
dori_gbif1 %>% 
  filter(datasetName %in% c("Diveboard - Scuba diving citizen science"))

# filtrar todas do dataset suspeito
dori_gbif_ok <- dori_gbif1 %>% 
  filter(!datasetName %in% c("Diveboard - Scuba diving citizen science"))

### 

library(ggmap)
library(maps)
library(mapdata)

world <- map_data('world')

# checar pontos

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = dori_gbif_ok, aes(x = decimalLongitude, y = decimalLatitude), color = "red") +
  labs(x = "longitude", y = "latitude", title = expression(italic("Paracanthurus hepatus")))

# checar profundidade
dori_gbif_ok %>% 
  ggplot(aes(x = depth, fill = waterBody)) +
  geom_histogram() 

# 3.3.1 OBIS

library(robis) #install.packages("robis")

## OBIS
dori_obis <- robis::occurrence("Paracanthurus hepatus")

# checar dados
names(dori_obis)

dori_obis1 <- dori_obis %>% 
  dplyr::select(scientificName, decimalLatitude, decimalLongitude, bathymetry,
                flags, waterBody, basisOfRecord, occurrenceStatus, rightsHolder, 
                datasetName, recordedBy, depth, locality, habitat) %>% 
  distinct()

# check problemas reportados (flags)
dori_obis1 %>% 
  distinct(flags)

# check NA em datasetName
dori_obis1 %>% 
  filter(!flags %in% c("no_depth,on_land", "on_land", "on_land,depth_exceeds_bath", "depth_exceeds_bath,on_land"),
         is.na(datasetName)) %>% 
  distinct(waterBody)

# depth ok
dori_obis1 %>% 
  filter(!flags %in% c("no_depth,on_land", "on_land", "on_land,depth_exceeds_bath", "depth_exceeds_bath,on_land"),
         !is.na(datasetName),
         !waterBody %in% c("North America", "North America Atlantic", "atlantique")) %>% 
  ggplot(aes(x = depth, fill = waterBody)) +
  geom_histogram() 

# checar niveis
dori_obis1 %>% 
  filter(!flags %in% c("no_depth,on_land", "on_land", "on_land,depth_exceeds_bath", "depth_exceeds_bath,on_land"),
         !is.na(datasetName),
         !waterBody %in% c("North America", "North America Atlantic", "atlantique")) %>% 
  lapply(., unique)

# ok
dori_obis_ok <- dori_obis1 %>% 
  filter(!flags %in% c("no_depth,on_land", "on_land", "on_land,depth_exceeds_bath", "depth_exceeds_bath,on_land"),
         !is.na(datasetName),
         !waterBody %in% c("North America", "North America Atlantic", "atlantique", NA)) 

# check
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = dori_obis_ok, aes(x = decimalLongitude, y = decimalLatitude, color = waterBody)) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Paracanthurus hepatus")))

# unir GBIF e OBIS

# ver diferencas
setdiff(names(dori_gbif_ok), names(dori_obis_ok))

setdiff(names(dori_obis_ok), names(dori_gbif_ok))


all_data <- bind_rows(dori_gbif_ok %>% 
                        mutate(repo = paste0("gbif", row.names(.))), 
                      dori_obis_ok %>% 
                        mutate(repo = paste0("obis", row.names(.)))) %>%
  column_to_rownames("repo") %>% 
  dplyr::select(decimalLongitude, decimalLatitude, depth) %>% 
  distinct() %>% 
  rownames_to_column("occ") %>% 
  separate(col = "occ", into = c("datasetName", "rn"), sep = 4) %>%
  mutate(scientificName = "Paracanthurus hepatus") %>% 
  dplyr::select(-rn)


# mapear ocorrencias
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = all_data, aes(x = decimalLongitude, y = decimalLatitude, color = datasetName)) +
  #theme(legend.title = element_blank()) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Paracanthurus hepatus")))

#Salvando os dados já tratados

write.csv(all_data, "data/occ_GBIF-OBIS_par_hepa.csv", row.names = FALSE)

# 3.2.2 EXTRA

# 3.3.2.1 Função ‘caseira’

# funcao para classificar ocorrencias suspeitas
flag_outlier <- function(df, species){
  
  # funcao para classificar ocorrencias suspeitas
  # baseada no calculo do centroide de todas ocorrencias
  # indica como 'check' as ocorrencias que tem distancias até o centroide
  # acima do 90th quantil (default) das distancias calculadas
  
  dados <- df %>% 
    dplyr::filter(scientificName == species); 
  
  dados2 <- geosphere::distVincentyEllipsoid(
    dados %>%
      summarise(centr_lon = median(decimalLongitude),
                centr_lat = median(decimalLatitude)),
    dados %>% 
      dplyr::select(decimalLongitude, decimalLatitude)
  ) %>% 
    bind_cols(dados) %>% 
    rename(dist_centroid = '...1') %>% 
    mutate(flag = ifelse(dist_centroid < quantile(dist_centroid, probs = 0.9), "OK",
                         ifelse(dist_centroid >= quantile(dist_centroid, probs = 0.90) & dist_centroid < quantile(dist_centroid, probs = 0.95), "check > Q90",
                                ifelse(dist_centroid >= quantile(dist_centroid, probs = 0.95), "check > Q95", "OK"))))
  
  # mutate(flag = ifelse(dist_centroid > quantile(dist_centroid, probs = prob), "check", "OK"))
  
  print(dados2)
  
}

# classificar ocorrências
marcados <- dori_gbif$data %>% 
  data.frame() %>% 
  dplyr::select(scientificName, decimalLongitude, decimalLatitude, datasetName) %>% 
  distinct() %>% 
  flag_outlier(., "Paracanthurus hepatus (Linnaeus, 1766)")

# mapa
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = marcados, 
             aes(x = decimalLongitude, y = decimalLatitude, 
                 color = flag)) +
  theme(legend.title = element_blank()) +
  labs(x = "longitude", y = "latitude", 
       title = expression(italic("Paracanthurus hepatus")))

# Podemos notar no mapa acima que as ocorrencias acima do 90ésimo quantil são muito similares às já filtradas acima com base no waterBody, mas se já não tivéssemos a informação da ocorrência restrita da espécie ao Indo-Pacífico, já poderíamos desconfiar destas ocorrências tão longe, os outliers. Investigando o datasetName destas ocorrências com flags também chegaríamos a mesma conclusão de excluir os dados associados ao Diveboard - Scuba diving citizen science e sem valor de datasetName.

# 3.3.2.2 Pacote scrubr
# O pacote scrubr possui algumas funções úteis para indicar problemas com coordenadas com elementos faltantes (coord_incomplete), coordenadas impossíveis (coord_impossible) e coordenadas duplicadas (dedup). Este último é estimado em um raio de tolerância e mais útil para análises em escala mais grosseira ou quando o informações a serem correlacionadas estão em escala maior. Podemos inclusive aplicá-las sequencialmente, pois o padrão das funções é excluir as ocorrências suspeitas, mas não recomendo seguir cegamente métodos automáticos de exclusão de dados!

if (!require("scrubr")) install.packages("scrubr")

library(scrubr)

# usando os dados com flag
data_scrubr <- marcados %>% 
  dframe() %>% 
  coord_impossible() %>% 
  coord_incomplete() %>% 
  coord_unlikely() %>% 
  dedup()


# mapa
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = data_scrubr, 
             aes(x = decimalLongitude, y = decimalLatitude), 
             color = "red") +
  geom_point(data = marcados %>% 
               filter(flag != "OK"), 
             aes(x = decimalLongitude, y = decimalLatitude), 
             color = "blue", shape = 3) +
  theme(legend.title = element_blank()) +
  labs(x = "longitude", y = "latitude", 
       title = expression(italic("Paracanthurus hepatus")))

# Note que as ocorrências indicadas com uma cruz foram mantidas pelo scrubr, e isso reforça o papel de quem analisa em julgar os pontos suspeitos.

# 3.3.2.3 pacote obistools

if (!require("obistools")) install.packages("obistools")

library(obistools)

# dori_obis %>% 
#   dplyr::select(decimalLongitude, decimalLatitude, scientificNameID) %>% 
#   distinct() %>% 
#   check_outliers_species(., report=TRUE)


# usando essa configuração chegamos a valores próximos aos da limpeza manual
dori_obis %>% 
  dplyr::select(decimalLongitude, decimalLatitude, scientificNameID) %>% 
  distinct() %>% 
  check_outliers_dataset(., report = FALSE, iqr_coef = 1, mad_coef = 5) %>% 
  dim()

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = marcados %>% 
               filter(flag != "OK"), 
             aes(x = decimalLongitude, y = decimalLatitude, 
                 color = datasetName)) +
  theme(legend.title = element_blank()) +
  labs(x = "longitude", y = "latitude", 
       title = expression(italic("Paracanthurus hepatus")))

#Por fim, vamos testar o pacote CoordinateCleaner. Nele devemos especificar os campos correspondentes na função clean_coordinates.

flags <-
  clean_coordinates(
    x = marcados,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    species = "scientificName",
    tests = c("equal", "gbif",
              "zeros", "seas")
  )

# Neste caso, nenhuma ocorrência foi marcada como suspeita. Moral da história, sempre temos que conferir todos os dados, mas as ferramentas ajudam muito nesta tarefa!













