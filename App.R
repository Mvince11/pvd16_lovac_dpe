# ==============================================================================
# LOVAC -> DPE : récupérer la classe DPE de chaque logement vacant
# ------------------------------------------------------------------------------
# Principe
#   1. Géocoder les adresses LOVAC avec la Base Adresse Nationale (BAN)
#   2. BDNB  : DPE "représentatif" du bâtiment (jointure via l'adresse BAN)
#   3. ADEME : DPE propre au logement (jointure via l'identifiant BAN,
#              départagée par la surface et l'étage)
#   4. Fusion : l'ADEME est prioritaire quand le rapprochement est crédible,
#              sinon on retombe sur la BDNB. Chaque résultat est qualifié.
#
# Limites à garder en tête
#   - L'ADEME ne couvre que les DPE établis depuis juillet 2021.
#   - La BDNB donne une classe par bâtiment, indicative en habitat collectif.
#   - Beaucoup de logements vacants n'ont jamais eu de DPE : un résultat vide
#     n'est pas forcément une erreur de rapprochement.
# ==============================================================================


# 0. PARAMETRES ----------------------------------------------------------------

library(sf)
library(dplyr)
library(readr)
library(data.table)
library(httr2)
library(tibble)

# Fichiers d'entrée / sortie
CHEMIN_LOVAC    <- "Data/2026-10-09_C1775__2026_10_02_lovac_cerema_89.geojson"
CHEMIN_BDNB_REL <- "Data/rel_batiment_groupe_adresse.csv"
CHEMIN_BDNB_DPE <- "Data/batiment_groupe_dpe_representatif_logement.csv"
SORTIE_GEOJSON  <- "lovac_dpe.geojson"
SORTIE_CSV      <- "lovac_dpe.csv"

# Colonnes de la base LOVAC (à adapter si besoin)
COL_ID      <- "ff_idlocal"   # clé : code département (2 car.) + invariant (10 car.)
COL_ADRESSE <- "adresse"      # adresse complète (numéro + voie)
COL_SURFACE <- "ff_slocal"    # surface du local (Fichiers fonciers)

# Jeu de données ADEME "DPE logements existants" (data.ademe.fr)
ID_JEU_ADEME <- "dpe03existant"

# Seuils
SCORE_MIN <- 0.8   # score BAN minimal pour considérer l'adresse fiable
ECART_MAX <- 0.10  # écart relatif max entre surface LOVAC et surface DPE


# 1. LECTURE DE LOVAC ------------------------------------------------------------

lovac <- st_read(CHEMIN_LOVAC, quiet = TRUE)

# Contrôles de base
stopifnot(all(c(COL_ID, COL_ADRESSE, COL_SURFACE) %in% names(lovac)))
stopifnot(!anyDuplicated(lovac[[COL_ID]]))
message(nrow(lovac), " logements vacants lus.")

# Convertit du texte "45,5" ou un nombre en numérique
vers_num <- function(x) as.numeric(gsub(",", ".", as.character(x)))

# Table de travail (sans géométrie)
lovac_attr <- lovac |>
  st_drop_geometry() |>
  transmute(ff_idlocal = .data[[COL_ID]],
            adresse    = .data[[COL_ADRESSE]],
            surf_ff    = vers_num(.data[[COL_SURFACE]]),
            # idlocal = 2 caractères de département + 10 d'invariant :
            # les 5 premiers caractères donnent le code INSEE de la commune
            code_insee = substr(.data[[COL_ID]], 1, 5))

summary(lovac_attr$surf_ff)   # doit afficher des valeurs, pas seulement des NA


# 2. GEOCODAGE DES ADRESSES (BAN, service CSV en lot) -----------------------------

geocoder_lot <- function(df) {
  f <- tempfile(fileext = ".csv")
  write_csv(df, f)
  rep <- request("https://api-adresse.data.gouv.fr/search/csv/") |>
    req_body_multipart(
      data     = curl::form_file(f, type = "text/csv"),
      columns  = "adresse",        # colonne contenant l'adresse
      citycode = "code_insee"      # colonne contenant le code INSEE
    ) |>
    req_error(body = \(r) resp_body_string(r)) |>   # affiche le message d'erreur de l'API
    req_perform()
  read_csv(I(resp_body_string(rep)), col_types = cols(.default = "c"))
}

# Lots de 20 000 lignes max (limite de taille de l'API)
lots <- split(lovac_attr[, c("ff_idlocal", "adresse", "code_insee")],
              ceiling(seq_len(nrow(lovac_attr)) / 20000))

geo <- bind_rows(lapply(lots, geocoder_lot)) |>
  transmute(ff_idlocal,
            ban_id    = tolower(result_id),    # identifiant d'adresse BAN
            ban_label = result_label,
            ban_score = as.numeric(result_score),
            ban_type  = result_type)           # housenumber = au numéro, street = à la rue

# Qualité du géocodage
geo |> count(ban_type, sort = TRUE)

# Adresses utilisables pour les jointures : au numéro et score suffisant
# geo_ok <- geo |> filter(ban_type == "housenumber", ban_score >= SCORE_MIN)
# message(nrow(geo_ok), " adresses géocodées de façon fiable sur ", nrow(geo), ".")


# 3. BDNB : DPE REPRESENTATIF DU BATIMENT -----------------------------------------

# Lien adresse BAN <-> bâtiment
rel_adr <- fread(CHEMIN_BDNB_REL, colClasses = "character") |>
  as_tibble() |>
  transmute(batiment_groupe_id,
            cle_interop_adr = tolower(cle_interop_adr))

# DPE par bâtiment. Les chaînes vides et "N" (non renseigné) deviennent NA.
# classe_bilan_dpe = DPE depuis juillet 2021
# classe_conso_energie_arrete_2012 = ancien DPE (consommation seule)
dpe_bdnb <- fread(CHEMIN_BDNB_DPE, colClasses = "character") |>
  as_tibble() |>
  select(batiment_groupe_id, classe_bilan_dpe, classe_conso_energie_arrete_2012) |>
  mutate(across(-batiment_groupe_id, \(x) na_if(na_if(x, ""), "N")))

res_bdnb <- geo |>
  left_join(rel_adr, by = c("ban_id" = "cle_interop_adr"),
            relationship = "many-to-many") |>
  left_join(dpe_bdnb, by = "batiment_groupe_id") |>
  group_by(ff_idlocal) |>
  summarise(n_batiments = n_distinct(batiment_groupe_id, na.rm = TRUE),
            dpe_actuel  = na.omit(classe_bilan_dpe)[1],
            dpe_ancien  = na.omit(classe_conso_energie_arrete_2012)[1],
            .groups = "drop") |>
  mutate(classe_bdnb = coalesce(dpe_actuel, dpe_ancien),
         source_bdnb = case_when(!is.na(dpe_actuel) ~ "DPE depuis 2021",
                                 !is.na(dpe_ancien) ~ "DPE ancien (avant 07/2021)",
                                 TRUE ~ NA_character_))

res_bdnb |> count(source_bdnb)


# 4. ADEME : TELECHARGER LES DPE DES COMMUNES CONCERNEES --------------------------

base_ademe <- paste0("https://data.ademe.fr/data-fair/api/v1/datasets/", ID_JEU_ADEME)

# Pour consulter les colonnes disponibles :
# schema <- request(base_ademe) |> req_perform() |> resp_body_json()
# sapply(schema$schema, \(x) x$key)

colonnes_ademe <- c("numero_dpe", "date_etablissement_dpe", "etiquette_dpe",
                    "etiquette_ges", "type_batiment", "annee_construction",
                    "surface_habitable_logement", "numero_etage_appartement",
                    "adresse_ban", "identifiant_ban", "score_ban", "code_insee_ban")

codes_insee <- unique(lovac_attr$code_insee)
qs_insee <- paste0("code_insee_ban:(",
                   paste0('"', codes_insee, '"', collapse = " OR "), ")")

req <- request(paste0(base_ademe, "/lines")) |>
  req_url_query(qs = qs_insee, size = 5000,
                select = paste(colonnes_ademe, collapse = ","))

pages <- list()
repeat {                                   # pagination de l'API
  r <- req_perform(req) |> resp_body_json(simplifyVector = TRUE)
  pages[[length(pages) + 1]] <- as_tibble(r$results)
  if (is.null(r$`next`)) break
  req <- request(r$`next`)
}

dpe_ademe <- bind_rows(pages) |>
  mutate(ban_id = tolower(identifiant_ban),
         surface_habitable_logement = vers_num(surface_habitable_logement),
         date_etablissement_dpe     = as.Date(date_etablissement_dpe))

message(nrow(dpe_ademe), " DPE ADEME téléchargés.")

# Contrôle : les identifiants BAN sont-ils comparables ?
mean(startsWith(dpe_ademe$ban_id, paste0(codes_insee[1], "_")))
mean(geo$ban_id %in% dpe_ademe$ban_id)   # part des adresses LOVAC présentes dans l'ADEME


# 5. ADEME : RAPPROCHEMENT PAR ADRESSE, SURFACE ET ETAGE --------------------------

# Toutes les paires (logement LOVAC, DPE ADEME à la même adresse BAN)
cand <- geo |>
  inner_join(dpe_ademe, by = "ban_id", relationship = "many-to-many") |>
  left_join(lovac_attr |> select(ff_idlocal, surf_ff), by = "ff_idlocal") |>
  mutate(ecart = abs(surface_habitable_logement - surf_ff) / surf_ff)

# Aide au réglage du seuil ECART_MAX
cand |> filter(!is.na(ecart)) |> pull(ecart) |> quantile(c(.25, .5, .75, .9))

# Un seul DPE retenu par logement : surface la plus proche, puis le plus récent
choix <- cand |>
  group_by(ff_idlocal) |>
  mutate(n_logements_adresse = n_distinct(round(surface_habitable_logement),
                                          numero_etage_appartement)) |>
  arrange(ecart, desc(date_etablissement_dpe), .by_group = TRUE) |>
  slice(1) |>
  ungroup() |>
  mutate(fiabilite = case_when(
    !is.na(ecart) & ecart <= ECART_MAX ~ "haute",    # surface concordante
    n_logements_adresse == 1           ~ "moyenne",  # seul DPE à cette adresse, surface non confirmée
    TRUE                               ~ "ambigu"))  # plusieurs logements possibles : non utilisé

choix |> count(fiabilite)


# 6. FUSION ET QUALIFICATION DES RESULTATS ----------------------------------------

final <- geo |>
  select(ff_idlocal, ban_label, ban_type, ban_score) |>
  left_join(res_bdnb, by = "ff_idlocal") |>
  left_join(choix |>
              select(ff_idlocal,
                     classe_ademe     = etiquette_dpe,
                     ges_ademe        = etiquette_ges,
                     date_dpe_ademe   = date_etablissement_dpe,
                     numero_dpe_ademe = numero_dpe,
                     fiabilite_ademe  = fiabilite),
            by = "ff_idlocal") |>
  mutate(
    # L'ADEME (propre au logement) est prioritaire quand le rapprochement est crédible
    ademe_utilisable = fiabilite_ademe %in% c("haute", "moyenne"),
    classe_finale = if_else(ademe_utilisable, classe_ademe, classe_bdnb),
    source_finale = case_when(ademe_utilisable      ~ "ADEME (logement)",
                              !is.na(classe_bdnb)   ~ "BDNB (bâtiment)",
                              TRUE                  ~ NA_character_),
    # À contrôler à la main : rapprochement ADEME moyen ou ambigu
    a_verifier = fiabilite_ademe %in% c("moyenne", "ambigu"),
    motif_absence = case_when(
      !is.na(classe_finale)                          ~ NA_character_,
      ban_type != "housenumber" | ban_score < SCORE_MIN ~ "adresse non résolue au numéro",
      TRUE                                           ~ "aucun DPE trouvé pour cette adresse"))

final |> count(source_finale)
final |> count(motif_absence)
final |> count(a_verifier)


# 7. EXPORT ------------------------------------------------------------------------

# On retire d'éventuelles colonnes de même nom déjà présentes dans LOVAC
lovac_dpe <- lovac |>
  select(-any_of(setdiff(names(final), "ff_idlocal"))) |>
  left_join(final, by = c("ff_idlocal" = "ff_idlocal"))

st_write(lovac_dpe, SORTIE_GEOJSON, delete_dsn = TRUE)
write_csv(st_drop_geometry(lovac_dpe), SORTIE_CSV)

# Résumé final
lovac_dpe |> st_drop_geometry() |> count(classe_finale, source_finale, sort = TRUE)
message("Terminé : ", SORTIE_GEOJSON, " et ", SORTIE_CSV)
