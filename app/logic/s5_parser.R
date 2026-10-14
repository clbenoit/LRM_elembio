#app/logic/s5_parser.R
# Validation / construction du manifest RNASeq_S5 (fonctions pures, testables).

#' Valide un tableau S5 (colonnes ANALYSIS_TYPE, BC_NUMBER, SAMPLE_ID, PATIENT_ID, REPASSE).
#' Pur (pas de Shiny) : normalise le type d'analyse et collecte les erreurs lues.
#'
#' Normalisation ANALYSIS_TYPE (case-insensitive) :
#'   - minuscules -> majuscules (`pan` -> `PAN`)
#'   - caracteres en plus -> troncature aux 3 premiers (`PANTAL` -> `PAN`)
#' Erreurs collectees (blockent le download) :
#'   - ANALYSIS_TYPE vide ou ne demarrant ni par PAN ni par CTL (message avec numero de ligne)
#'   - BC_NUMBER vide sur une ligne partiellement remplie (message avec numero de ligne)
#'   - BC_NUMBER duplique entre lignes
#'   - SAMPLE_ID duplique entre lignes
#' Les lignes entierement vides sont ignorees (NA = cellule vide handsontable).
#' La colonne REPASSE (checkbox) est normalisee : TRUE (cochee) -> "REPASSE",
#' FALSE/NA -> "".
#'
#' @param df data.frame aux colonnes S5 (NA = cellule vide, REPASSE logique)
#' @return list(data = data.frame normalise (lignes non vides, REPASSE = "" ou "REPASSE"),
#'              errors = character())
parse_s5_table <- function(df) {
  # Robustesse : df sans colonnes S5 (payload de schema heterogene) -> erreur
  # explicite au lieu d'un crash (le branchement Shiny filtre deja, c'est
  # une derniere garde).
  missing <- setdiff(
    c("ANALYSIS_TYPE", "BC_NUMBER", "SAMPLE_ID", "PATIENT_ID"),
    colnames(df)
  )
  if (length(missing) > 0) {
    return(list(data = df[0, , drop = FALSE],
                errors = paste0(
                  "Schema inattendu : colonnes manquantes (",
                  paste(missing, collapse = ", "), ")")))
  }
  df <- as.data.frame(lapply(df, function(x) {
    v <- as.character(x)
    v[is.na(v)] <- ""
    trimws(v)
  }))
  empty <- rowSums(df != "") == 0

  at <- toupper(df$ANALYSIS_TYPE)
  known <- startsWith(at, "PAN") | startsWith(at, "CTL")
  at[known] <- substr(at[known], 1, 3)

  errors <- character(0)
  bad_type <- which(!empty & !known)
  for (i in bad_type) {
    if (df$ANALYSIS_TYPE[i] == "") {
      errors <- c(errors, paste0("Ligne ", i, " : ANALYSIS_TYPE vide"))
    } else {
      errors <- c(errors, paste0(
        "Ligne ", i, " : type d'analyse inconnu ('", df$ANALYSIS_TYPE[i],
        "') - doit demarrer par PAN ou CTL"))
    }
  }
  bad_bc <- which(!empty & df$BC_NUMBER == "")
  for (i in bad_bc) {
    errors <- c(errors, paste0("Ligne ", i, " : BC_NUMBER vide"))
  }

  data <- df[!empty, , drop = FALSE]
  if (is.null(data$REPASSE)) data$REPASSE <- rep("", nrow(data))
  data$ANALYSIS_TYPE <- at[!empty]
  # checkbox REPASSE : TRUE (coche) -> "REPASSE" ; FALSE/NA -> "" (robuste aussi
  # a l'ancien format texte "REPASSE")
  data$REPASSE <- ifelse(data$REPASSE %in% c("TRUE", "REPASSE"), "REPASSE", "")
  rownames(data) <- NULL

  if (nrow(data) > 0) {
    bc_ne <- data$BC_NUMBER[data$BC_NUMBER != ""]
    dup_bc <- unique(bc_ne[duplicated(bc_ne)])
    if (length(dup_bc)) {
      errors <- c(errors,
                  paste0("BC_NUMBER duplique : ", paste(dup_bc, collapse = ", ")))
    }
    sid_ne <- data$SAMPLE_ID[data$SAMPLE_ID != ""]
    dup_id <- unique(sid_ne[duplicated(sid_ne)])
    if (length(dup_id)) {
      errors <- c(errors,
                  paste0("SAMPLE_ID duplique : ", paste(dup_id, collapse = ", ")))
    }
  }

  list(data = data, errors = errors)
}

#' Construit les lignes de donnees du manifest Ion v2.3 RNA.
#' Sample Name = `<SAMPLE_ID> <PATIENT_ID[1:3] en majuscules> <ANALYSIS_TYPE>` (+ " REPASSE").
#'
#' @param data resultat de parse_s5_table()$data
#' @return data.frame 9 colonnes Ion (sans les 2 lignes de header)
build_s5_manifest <- function(data) {
  n <- nrow(data)
  sample_names <- character(n)
  for (i in seq_len(n)) {
    pat_abbr <- toupper(substr(data$PATIENT_ID[i], 1, min(3, nchar(data$PATIENT_ID[i]))))
    sample_names[i] <- gsub("\\s+", " ",
                            paste(data$SAMPLE_ID[i], pat_abbr, data$ANALYSIS_TYPE[i]))
    if (data$REPASSE[i] == "REPASSE") {
      sample_names[i] <- paste(sample_names[i], "REPASSE")
    }
  }
  data.frame(
    Barcode = paste0("Ion_V2_RBC1_BC", data$BC_NUMBER),
    `Control Type` = "",
    `Sample Name (required)` = sample_names,
    `Sample ID` = "",
    `Sample Description` = "",
    `DNA/RNA/Fusions` = "RNA",
    `Reference library` = "",
    `Target regions BED file` = "",
    `Hotspot regions BED file` = "",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

#' Data.frame complet du CSV Ion (2 lignes d'entete + lignes de donnees).
#'
#' @param data resultat de parse_s5_table()$data
#' @return data.frame 9 colonnes Ion (2 lignes d'entete incluses, NA = cellule vide)
build_s5_manifest_df <- function(data) {
  manifest <- build_s5_manifest(data)
  hdr2 <- c("Barcode", "Control Type", "Sample Name (required)", "Sample ID",
            "Sample Description", "DNA/RNA/Fusions", "Reference library",
            "Target regions BED file", "Hotspot regions BED file")
  hdr1 <- c("CSV Version (required)", "2.3", rep(NA_character_, 7))
  hdr_df <- data.frame(
    matrix(c(hdr1, hdr2), nrow = 2, byrow = TRUE),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  colnames(hdr_df) <- colnames(manifest)
  rbind(hdr_df, manifest)
}

#' Lignes completes du CSV a telecharger (2 lignes de header + donnees).
#' @param manifest resultat de build_s5_manifest()
#' @return character() : une ligne par element
s5_manifest_csv_lines <- function(manifest) {
  c(
    "CSV Version (required),2.3",
    "Barcode,Control Type,Sample Name (required),Sample ID,Sample Description,DNA/RNA/Fusions,Reference library,Target regions BED file,Hotspot regions BED file",
    apply(as.matrix(manifest), 1, paste, collapse = ",")
  )
}
