#app/logic/s5_parser.R
# Parsing / validation / construction du manifest S5 (fonctions pures, testables).

#' Parse un input S5 collé (N lignes `<CTRL> <BC> <SAMPLE_ID> <NOM>`, espaces).
#' Pur (pas de Shiny) : renvoie les lignes valides + un vecteur d'erreurs lues.
#'
#' Normalisation CTRL (case-insensitive) :
#'   - minuscules -> majuscules (`pan` -> `PAN`)
#'   - caracteres en plus -> troncature aux 3 premiers (`PANTAL` -> `PAN`)
#' Erreurs collectees (blockent le download) :
#'   - ligne non vide != 4 champs (message avec numero de ligne)
#'   - CTRL ne demarrant ni par PAN ni par CTL
#'   - BC duplique entre lignes
#'   - SAMPLE_ID duplique entre lignes
#'
#' @param text character (1) brut du textarea
#' @return list(data = data.frame(CTRL, BC, SAMPLE_ID, NOM, row_number),
#'              errors = character())
parse_s5_input <- function(text) {
  lines <- trimws(gsub("\r$", "", strsplit(text, "\n")[[1]]))
  rows <- vector("list")
  errors <- character(0)

  for (i in seq_along(lines)) {
    if (!nzchar(lines[i])) next
    fields <- strsplit(trimws(lines[i]), "\\s+")[[1]]
    fields <- fields[nzchar(fields)]
    if (length(fields) != 4) {
      errors <- c(errors, paste0(
        "Ligne ", i, " : 4 champs attendus (CTRL BC SAMPLE_ID NOM), ",
        length(fields), " trouves"))
      next
    }
    ctrl <- toupper(fields[1])
    if (!startsWith(ctrl, "PAN") && !startsWith(ctrl, "CTL")) {
      errors <- c(errors, paste0(
        "Ligne ", i, " : type de controle inconnu ('", fields[1],
        "') - doit demarrer par PAN ou CTL"))
      next
    }
    rows[[length(rows) + 1]] <- data.frame(
      CTRL = substr(ctrl, 1, 3),
      BC = fields[2],
      SAMPLE_ID = fields[3],
      NOM = fields[4],
      row_number = i,
      stringsAsFactors = FALSE
    )
  }

  data <- if (length(rows)) {
    do.call(rbind, rows)
  } else {
    data.frame(
      CTRL = character(0), BC = character(0), SAMPLE_ID = character(0),
      NOM = character(0), row_number = numeric(0), stringsAsFactors = FALSE
    )
  }

  if (nrow(data) > 0) {
    dup_bc <- unique(data$BC[duplicated(data$BC)])
    if (length(dup_bc)) {
      errors <- c(errors, paste0("BC duplique : ", paste(dup_bc, collapse = ", ")))
    }
    dup_id <- unique(data$SAMPLE_ID[duplicated(data$SAMPLE_ID)])
    if (length(dup_id)) {
      errors <- c(errors,
                  paste0("SAMPLE_ID duplique : ", paste(dup_id, collapse = ", ")))
    }
  }

  list(data = data, errors = errors)
}

#' Construit les lignes de donnees du manifest Ion v2.3 RNA.
#' Sample Name = `<SAMPLE_ID> <NOM[1:3] en majuscules> <CTRL>` (+ " REPASSE").
#'
#' @param data resultat de parse_s5_input()$data
#' @param repasse logical(nrow(data)) : flag REPASSE par ligne (NA/TRUE/FALSE)
#' @return data.frame 9 colonnes Ion (sans les 2 lignes de header)
build_s5_manifest <- function(data, repasse) {
  n <- nrow(data)
  sample_names <- character(n)
  for (i in seq_len(n)) {
    nom_abbr <- toupper(substr(data$NOM[i], 1, min(3, nchar(data$NOM[i]))))
    sample_names[i] <- paste(data$SAMPLE_ID[i], nom_abbr, data$CTRL[i])
    if (isTRUE(repasse[i])) {
      sample_names[i] <- paste(sample_names[i], "REPASSE")
    }
  }
  data.frame(
    Barcode = paste0("Ion_V2_RBC1_BC", data$BC),
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
