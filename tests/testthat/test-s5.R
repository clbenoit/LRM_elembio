box::use(
  testthat[expect_true, expect_equal, expect_null, test_that],
  shiny[testServer],
)
box::use(
  app/logic/appDataManager[appDataManager],
  app/logic/s5_parser[build_s5_manifest, s5_manifest_csv_lines],
  app/main[server],
  app/view/import_indexes,
)

# appDataManager pre-configure (l'analyse est choisie avant le chargement des templates)
am_for <- function(analysis = "RNASeq_S5", num_rows = 8L) {
  am <- appDataManager$new()
  am$selectors$analysis_name <- analysis
  am$selectors$num_rows <- num_rows
  am
}

# Valeur serialisee handsontable S5 (format getData() : lignes x colonnes).
# REPASSE : checkbox -> valeurs logiques (TRUE = cochee, NA = vide).
ht_s5_value <- function(types, bcs, sids, pats, repasses) {
  list(
    data = lapply(
      seq_along(types),
      function(i) c(types[i], bcs[i], sids[i], pats[i], repasses[i])
    ),
    changes = list(event = "afterChange"),
    params = list(
      rClass = "data.frame",
      rColHeaders = list("ANALYSIS_TYPE", "BC_NUMBER", "SAMPLE_ID",
                         "PATIENT_ID", "REPASSE"),
      rRowHeaders = list(seq_along(types)),
      rColClasses = list(
        ANALYSIS_TYPE = "character", BC_NUMBER = "character",
        SAMPLE_ID = "character", PATIENT_ID = "character",
        REPASSE = "logical"
      )
    )
  )
}

test_that("main server : RNASeq_S5 -> loadTemplates sans erreur (pas de kit)", {
  testServer(server, {
    session$setInputs(analysis_name = "RNASeq_S5")
    expect_equal(appDataManager$selectors$analysis_name, "RNASeq_S5")
    # pas de fichier Template_Settings_ : le template reste NULL
    expect_null(appDataManager$data$template_settings)
  })
})

test_that("import_indexes : RNASeq_S5 -> schema S5 (dropdown REPASSE) + round-trip colonnes", {
  am <- am_for()
  testServer(import_indexes$server, {
    # pas de template charge (pas de kit RNASeq_S5)
    expect_null(am$data$template_settings)
    # schema S5 : colonne REPASSE en checkbox dans la config du tableau
    tbl <- as.character(output$table_input)
    expect_true(any(grepl("checkbox", tbl)))
    # round-trip handsontable : les colonnes S5 reviennent intactes
    session$setInputs(
      table_input =
        ht_s5_value(c("CTL"), c("22"), c("26P1470"), c("BENVENUTI"), c(NA))
    )
    expect_equal(
      colnames(am$data$positions),
      c("ANALYSIS_TYPE", "BC_NUMBER", "SAMPLE_ID", "PATIENT_ID", "REPASSE")
    )
  }, args = list(appData = am, main_session = NULL))
})

test_that("import_indexes : bascule TS65 -> reset + schema standard (dropdown LANE)", {
  am <- am_for()
  testServer(import_indexes$server, {
    session$setInputs(
      table_input =
        ht_s5_value(c("CTL"), c("22"), c("26P1470"), c("BENVENUTI"), c(NA))
    )
    expect_equal(nrow(am$data$positions), 1)
    # bascule d'analyse -> reset du tableau + positions NULL
    am$selectors$analysis_name <- "TS65/GHEM-FFPE"
    session$setInputs(flush = 1) # force la reevaluation du graphe reactif
    expect_null(am$data$positions)
    tbl <- as.character(output$table_input)
    expect_true(any(grepl("1+2", tbl, fixed = TRUE)))
  }, args = list(appData = am, main_session = NULL))
})

test_that("import_indexes : tableau S5 valide -> manifest Ion exact + preview CSV", {
  am <- am_for()
  testServer(import_indexes$server, {
    session$setInputs(
      table_input =
        ht_s5_value(c("CTL", "PAN"), c("22", "23"),
                    c("26P1470", "26P1475"), c("BENVENUTI", "GEVAUDAN"),
                    c(NA, TRUE))
    )
    expect_equal(nrow(am$data$positions), 2)
    # manifest_samples_info = CSV Ion complet (2 lignes d'entete + 2 lignes)
    expect_equal(nrow(am$data$manifest_samples_info), 4)
    lines <- s5_manifest_csv_lines(build_s5_manifest(am$data$positions))
    expect_equal(lines[3], "Ion_V2_RBC1_BC22,,26P1470 BEN CTL,,,RNA,,,")
    expect_equal(lines[4], "Ion_V2_RBC1_BC23,,26P1475 GEV PAN REPASSE,,,RNA,,,")
    # manifest = les lignes du CSV Ion exactes (caractere) : la ligne 1 n'a que 2 champs
    expect_equal(am$data$manifest[1], "CSV Version (required),2.3")
    expect_equal(am$data$manifest[3], "Ion_V2_RBC1_BC22,,26P1470 BEN CTL,,,RNA,,,")
    expect_equal(length(am$data$manifest), 4)
  }, args = list(appData = am, main_session = NULL))
})

test_that("import_indexes : reset en S5 -> tableau S5 + manifest vide", {
  am <- am_for()
  testServer(import_indexes$server, {
    session$setInputs(
      table_input =
        ht_s5_value(c("CTL"), c("22"), c("26P1470"), c("BENVENUTI"), c(NA))
    )
    expect_equal(nrow(am$data$positions), 1)
    expect_equal(length(am$data$manifest), 3) # 2 entetes + 1 ligne
    session$setInputs(reset = 1)
    expect_null(am$data$positions)
    expect_null(am$data$manifest_samples_info)
    expect_null(am$data$manifest)
    # le tableau reste au schema S5 (checkbox REPASSE)
    tbl <- as.character(output$table_input)
    expect_true(any(grepl("checkbox", tbl)))
  }, args = list(appData = am, main_session = NULL))
})

test_that("import_indexes : Apply (generate_rows) en S5 -> lignes vides S5, schema conserve", {
  am <- am_for()
  testServer(import_indexes$server, {
    am$selectors$num_rows <- 10L
    am$selectors$generate_rows <- 1
    session$setInputs(flush = 1) # force la reevaluation du graphe reactif
    # pas d'erreur : le rbind utilise le schema S5 (5 colonnes) ; lignes vides -> positions NULL
    expect_null(am$data$positions)
    tbl <- as.character(output$table_input)
    expect_true(any(grepl("REPASSE", tbl)))
  }, args = list(appData = am, main_session = NULL))
})

# Valeur serialisee handsontable du schema STANDARD (LANE pre-rempli "1+2") :
# represente l'echo du widget de l'ancien schema apres une bascule d'analyse.
ht_standard_value <- function(sids) {
  n <- length(sids)
  list(
    data = lapply(seq_len(n), function(i) c(sids[i], "", "", "1+2")),
    changes = list(event = "afterChange"),
    params = list(
      rClass = "data.frame",
      rColHeaders = list("SAMPLE_ID", "SAMPLE_DESCRIPTION", "WELL_ID", "LANE"),
      rRowHeaders = list(seq_len(n)),
      rColClasses = list(
        SAMPLE_ID = "character", SAMPLE_DESCRIPTION = "character",
        WELL_ID = "character", LANE = "character"
      )
    )
  )
}

test_that("import_indexes : echo standard en retard en S5 -> ignore (pas de crash)", {
  am <- am_for()
  testServer(import_indexes$server, {
    session$setInputs(
      table_input =
        ht_s5_value(c("CTL"), c("22"), c("26P1470"), c("BENVENUTI"), c(NA))
    )
    expect_equal(nrow(am$data$positions), 1)
    # l'echo du schema standard arrive apres la bascule vers S5 : il est
    # ignore (avant le fix, le parser crashait sur la colonne REPASSE absente)
    session$setInputs(table_input = ht_standard_value(rep("", 8)))
    expect_equal(nrow(am$data$positions), 1)
    expect_equal(am$data$positions$SAMPLE_ID, "26P1470")
    # le tableau reste au schema S5 (pas de bascule vers le schema standard)
    tbl <- as.character(output$table_input)
    expect_true(any(grepl("checkbox", tbl)))
  }, args = list(appData = am, main_session = NULL))
})

test_that("import_indexes : type inconnu -> positions NULL (download bloque)", {
  am <- am_for()
  testServer(import_indexes$server, {
    session$setInputs(
      table_input =
        ht_s5_value(c("CTL", "XYZ"), c("22", "23"),
                    c("26P1470", "26P1475"), c("BENVENUTI", "GEVAUDAN"),
                    c(NA, NA))
    )
    expect_null(am$data$positions)
    expect_null(am$data$manifest_samples_info)
  }, args = list(appData = am, main_session = NULL))
})
