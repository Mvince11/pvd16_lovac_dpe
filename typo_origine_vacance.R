library(sf)
library(dplyr)
library(readr)
library(data.table)
library(httr2)
library(tibble)

# Fichiers d'entrée / sortie
LOVAC    <- st_read("Data/lovac_dpe.geojson")
lovac_fil <- read.csv2("Data/lovac_d16_fil.csv", sep=";")
UF <- st_read("Data/uf.geojson")

vers_num <- function(x) as.numeric(gsub(",", ".", as.character(x)))

LOVAC <- LOVAC |>
  mutate(periode_construction = case_when(
    is.na(vers_num(ff_jannath)) | vers_num(ff_jannath) < 1000 |
      vers_num(ff_jannath) > as.integer(format(Sys.Date(), "%Y")) ~ "0_erreur",
    vers_num(ff_jannath) > 2012 ~ "5_Plus2013",
    vers_num(ff_jannath) > 1974 ~ "4_1975_2012",
    vers_num(ff_jannath) > 1960 ~ "3_1961_1974",
    vers_num(ff_jannath) > 1944 ~ "2_1945_1960",
    TRUE                        ~ "1_Avant1945"))

## ================== Compte le nombre de logements par période de construction ===============
lovac_count_construction <- LOVAC |> st_drop_geometry() |> count(periode_construction)   # contrôle

## ================== On récupère le champs idprocpte ===================

#### =========== Correction des géométries =================
LOVAC <- st_make_valid(LOVAC)
UF    <- st_make_valid(UF)


lovac_idpro <- st_join(
    LOVAC,
    UF %>% select(c(idprocpte, locproptxt)),
    join = st_intersects,
    left = TRUE)

#### ========== On compte le nbre de droits par idprocpte ===============
lovac_count_idpro <- lovac_idpro %>%
  group_by(idprocpte)%>%
  summarise(
    nbre_droits =n(),
    .groups = "drop"
  )


lovac_idpro <- lovac_idpro %>%
  left_join(
    st_drop_geometry(lovac_count_idpro),
    by = "idprocpte"
  )

#### ========= Classement cadastral ===========