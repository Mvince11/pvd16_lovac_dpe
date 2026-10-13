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

#### ========== jointure spatial entre lovac et uf afin de récupérer l'idprocpte =============
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


#### ========== On récupère idprodroit, ndroit et ccodro pour le compte idprocpte d'un même local (idlocal) ===============
ff_idprodroits <- paste0("ff_idprodroit_", 1:6)
ff_ccodros <- paste0("ff_ccodro_", 1:6)

lovac_ndroit <- lovac_idpro %>%
  left_join(
    lovac_fil %>%
      mutate(ff_idlocal = as.character(ff_idlocal)) %>%
      select(ff_idlocal, ff_ndroit, all_of(ff_idprodroits), all_of(ff_ccodros)),
    by = "ff_idlocal"
  )

#### ============== Relation entre compte-propriétaire, droit de propriété, et gestionnaire/propriétaire ==================#
############################################################################################################################
# ============ Relation entre compte-propriétaire, droit de propriété, et gestionnaire/propriétaire =======================#
# ccodro 	                    ccodrotxt 	                                                                     typologie   #
# B 	           BAILLEUR A CONSTRUCTION (ASSOCIE AVEC R) 	                                                  Propriétaire #
# C 	           FIDUCIAIRE 	                                                                                Propriétaire #
# F 	           FONCIER (ASSOCIE AVEC D OU T) 	                                                              Propriétaire #
# N 	           NU-PROPRIETAIRE (ASSOCIE AVEC U) 	                                                          Propriétaire #
# P 	           PROPRIETAIRE 	                                                                              Propriétaire #
# V 	           BAILLEUR D'UN BAIL A REHABILITATION (ASSOCIE AVEC W) 	                                      Propriétaire #
# X 	           LA POSTE PROPRIETAIRE ET OCCUPANT 	                                                          Propriétaire #
# A 	           LOCATAIRE-ATTRIBUTAIRE (ASSOCIE AVEC P) 	                                                    Gestionnaire #
# B 	           BAILLEUR REEL SOLIDAIRE (ASSOCIE A Z) 	                                                      Gestionnaire #
# D 	           DOMANIER (ASSOCIE AVEC F) 	                                                                  Gestionnaire #
# E 	           EMPHYTEOTE (ASSOCIE AVEC P) 	                                                                Gestionnaire #
# G 	           GERANT,MANDATAIRE,GESTIONNAIRE 	                                                            Gestionnaire #
# H 	           ASSOCIE DANS UNE SOCIETE EN TRANSPARENCE FISCALE(ASSOCIE AVEC P) 	                          Gestionnaire #
# J 	           JEUNE AGRICULTEUR 	                                                                          Gestionnaire #
# K 	           ANTICHRESISTE (ASSOCIE AVEC P) 	                                                            Gestionnaire #
# L 	           FONCTIONNAIRE LOGE 	                                                                        Gestionnaire #
# M 	           OCCUPANT D UNE PARCELLE APPARTENANT AU DEPARTEMENT DE MAYOTTE OU A L ETAT (ASSOCIE A P) 	    Gestionnaire #
# O 	           AUTORISATION D'OCCUPATION TEMPORAIRE (70 ANS) 	                                              Gestionnaire #
# Q 	           GESTIONNAIRE TAXE SUR LES BUREAUX (ILE DE FRANCE) 	                                          Gestionnaire #
# R 	           PRENEUR A CONSTRUCTION (ASSOCIE AVEC B) 	                                                    Gestionnaire #
# S 	           SYNDIC DE COPROPRIETE 	                                                                      Gestionnaire #
# T 	           TENUYER (ASSOCIE AVEC F) 	                                                                  Gestionnaire #
# U 	           USUFRUITIER (ASSOCIE AVEC N) 	                                                              Gestionnaire #
# W 	           PRENEUR D'UN BAIL A REHABILITATION (ASSOCIE AVEC V) 	                                        Gestionnaire #
# Y 	           LA POSTE OCCUPANT - NON PROPRIETAIRE 	                                                      Gestionnaire #
# Z 	           PRENEUR DE BAIL SOLIDAIRE ASSOCIÉ A B 	                                                      Gestionnaire #
# Z 	           GESTIONNAIRE D UN BIEN DE L ETAT 	                                                          Gestionnaire #
############################################################################################################################

lookup_ccodro <- tribble(
  ~ccodro, ~ccodrotxt, ~typo_prop,
  # --- Propriétaires ---
  "B", "BAILLEUR A CONSTRUCTION (ASSOCIE AVEC PRENEUR A CONSTRUCTION) / BAILLEUR REEL SOLIDAIRE (ASSOCIE A PRENEUR DE BAIL SOLIDAIRE)", "Propriétaire / Gestionnaire",
  "C", "FIDUCIAIRE", "Propriétaire",
  "F", "FONCIER (ASSOCIE AVEC DOMANIER OU TENUYER)", "Propriétaire",
  "N", "NU-PROPRIETAIRE (ASSOCIE AVEC USUFRUITIER)", "Propriétaire",
  "P", "PROPRIETAIRE", "Propriétaire",
  "V", "BAILLEUR D'UN BAIL A REHABILITATION (ASSOCIE AVEC PRENEUR D'UN BAIL A REHABILITATION)", "Propriétaire",
  "X", "LA POSTE PROPRIETAIRE ET OCCUPANT", "Propriétaire",
  # --- Gestionnaires ---
  "A", "LOCATAIRE-ATTRIBUTAIRE (ASSOCIE AVEC PROPRIETAIRE)", "Gestionnaire",
  "D", "DOMANIER (ASSOCIE AVEC FONCIER)", "Gestionnaire",
  "E", "EMPHYTEOTE (ASSOCIE AVEC PROPRIETAIRE)", "Gestionnaire",
  "G", "GERANT, MANDATAIRE, GESTIONNAIRE", "Gestionnaire",
  "H", "ASSOCIE DANS UNE SOCIETE EN TRANSPARENCE FISCALE (ASSOCIE AVEC PROPRIETAIRE)", "Gestionnaire",
  "J", "JEUNE AGRICULTEUR", "Gestionnaire",
  "K", "ANTICHRESISTE (ASSOCIE AVEC PROPRIETAIRE)", "Gestionnaire",
  "L", "FONCTIONNAIRE LOGE", "Gestionnaire",
  "M", "OCCUPANT D'UNE PARCELLE APPARTENANT AU DEPARTEMENT DE MAYOTTE OU A L'ETAT (ASSOCIE A PROPRIETAIRE)", "Gestionnaire",
  "O", "AUTORISATION D'OCCUPATION TEMPORAIRE (70 ANS)", "Gestionnaire",
  "Q", "GESTIONNAIRE TAXE SUR LES BUREAUX (ILE DE FRANCE)", "Gestionnaire",
  "R", "PRENEUR A CONSTRUCTION (ASSOCIE AVEC BAILLEUR A CONSTRUCTION)", "Gestionnaire",
  "S", "SYNDIC DE COPROPRIETE", "Gestionnaire",
  "T", "TENUYER (ASSOCIE AVEC FONCIER)", "Gestionnaire",
  "U", "USUFRUITIER (ASSOCIE AVEC NU-PROPRIETAIRE)", "Gestionnaire",
  "W", "PRENEUR D'UN BAIL A REHABILITATION (ASSOCIE AVEC BAILLEUR D'UN BAIL A REHABILITATION)", "Gestionnaire",
  "Y", "LA POSTE OCCUPANT - NON PROPRIETAIRE", "Gestionnaire",
  "Z", "PRENEUR DE BAIL SOLIDAIRE (ASSOCIE A BAILLEUR REEL SOLIDAIRE) / GESTIONNAIRE D'UN BIEN DE L'ETAT", "Gestionnaire"
)

txt <- setNames(lookup_ccodro$ccodrotxt, lookup_ccodro$ccodro)
typ <- setNames(lookup_ccodro$typo_prop, lookup_ccodro$ccodro)

# Une paire de colonnes par droit : ccodrotxt_1..6 et typo_prop_1..6
for (i in 1:6) {
  lettre <- toupper(trimws(as.character(lovac_ndroit[[paste0("ff_ccodro_", i)]])))
  lovac_ndroit[[paste0("ccodrotxt_", i)]]  <- unname(txt[lettre])
  lovac_ndroit[[paste0("typo_prop_", i)]]  <- unname(typ[lettre])
}


#### ========= Classement cadastral ff_dcapec2 =========== ####

# Les 8 catégories de logements
# 
# • Catégorie 1 : Grand luxe (exceptionnel, rarissime)
# • Catégorie 2 : Luxe (très haut de gamme)
# • Catégorie 3 : Très confortable (construction soignée, prestations élevées)
# • Catégorie 4 : Confortable (bon niveau de construction et d'équipement)
# • Catégorie 5 : Assez confortable (construction correcte, équipements complets)
# • Catégorie 6 : Ordinaire (concerne la majorité du parc de logements)
# • Catégorie 7 : Médiocre (confort réduit, construction vieillissante)
# • Catégorie 8 : Très médiocre (vétusté marquée, équipements 

lookup_cadastre <- c(
  "1" = "Grand luxe",
  "2" = "Luxe",
  "3" = "Très confortable",
  "4" = "Confortable",
  "5" = "Assez confortable",
  "6" = "Ordinaire",
  "7" = "Médiocre",
  "8" = "Très médiocre"
)

lovac_ndroit <- lovac_ndroit %>%
  mutate(
    clst_cadastrale = unname(
      lookup_cadastre[as.character(as.integer(as.character(ff_dcapec2)))]
    )
  )