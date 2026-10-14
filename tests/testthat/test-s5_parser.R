box::use(
  testthat[expect_true, expect_equal, test_that],
)
box::use(
  app/logic/s5_parser[parse_s5_table, build_s5_manifest, build_s5_manifest_df,
                      s5_manifest_csv_lines],
)

# REPASSE : checkbox handsontable -> logique (TRUE = cochee, NA = vide)
make_s5_df <- function(types, bcs, sids, pats, repasses = NULL) {
  n <- length(types)
  data.frame(
    ANALYSIS_TYPE = types,
    BC_NUMBER = bcs,
    SAMPLE_ID = sids,
    PATIENT_ID = pats,
    REPASSE = if (is.null(repasses)) rep(NA, n) else repasses,
    stringsAsFactors = FALSE
  )
}

test_that("parse: tableau valide 2 lignes", {
  res <- parse_s5_table(make_s5_df(c("CTL", "PAN"), c("22", "23"),
                                   c("26P1470", "26P1475"),
                                   c("BENVENUTI", "GEVAUDAN")))
  expect_equal(nrow(res$data), 2)
  expect_equal(res$errors, character(0))
  expect_equal(res$data$ANALYSIS_TYPE, c("CTL", "PAN"))
  expect_equal(res$data$BC_NUMBER, c("22", "23"))
  expect_equal(res$data$SAMPLE_ID, c("26P1470", "26P1475"))
  expect_equal(res$data$PATIENT_ID, c("BENVENUTI", "GEVAUDAN"))
})

test_that("parse: lignes entierement vides ignorees", {
  res <- parse_s5_table(make_s5_df(c("", "CTL", "", "PAN"),
                                   c("", "22", "", "23"),
                                   c("", "26P1470", "", "26P1475"),
                                   c("", "BENVENUTI", "", "GEVAUDAN")))
  expect_equal(nrow(res$data), 2)
  expect_equal(res$errors, character(0))
})

test_that("parse: minuscules normalisees + caracteres en plus tronques", {
  res <- parse_s5_table(make_s5_df(c("pan", "PANTAL", "ctlef"),
                                   c("23", "24", "25"),
                                   c("26P1475", "26P1476", "26P1477"),
                                   c("GEVAUDAN", "BENVENUTI", "GIRAUD")))
  expect_equal(res$data$ANALYSIS_TYPE, c("PAN", "PAN", "CTL"))
  expect_equal(res$errors, character(0))
})

test_that("parse: type inconnu -> erreur avec numero de ligne", {
  res <- parse_s5_table(make_s5_df(c("CTL", "XYZ"), c("22", "23"),
                                   c("26P1470", "26P1475"),
                                   c("BENVENUTI", "GEVAUDAN")))
  expect_true(any(grepl("type d'analyse inconnu", res$errors)))
  expect_true(any(grepl("Ligne 2", res$errors)))
})

test_that("parse: ANALYSIS_TYPE vide sur ligne partiellement remplie -> erreur", {
  res <- parse_s5_table(make_s5_df(c("CTL", ""), c("22", "23"),
                                   c("26P1470", "26P1475"),
                                   c("BENVENUTI", "GEVAUDAN")))
  expect_true(any(grepl("ANALYSIS_TYPE vide", res$errors)))
  expect_true(any(grepl("Ligne 2", res$errors)))
})

test_that("parse: BC_NUMBER vide sur ligne partiellement remplie -> erreur", {
  res <- parse_s5_table(make_s5_df(c("CTL", "PAN"), c("22", ""),
                                   c("26P1470", "26P1475"),
                                   c("BENVENUTI", "GEVAUDAN")))
  expect_true(any(grepl("BC_NUMBER vide", res$errors)))
  expect_true(any(grepl("Ligne 2", res$errors)))
})

test_that("parse: BC_NUMBER duplique -> erreur", {
  res <- parse_s5_table(make_s5_df(c("CTL", "PAN"), c("22", "22"),
                                   c("26P1470", "26P1471"),
                                   c("BENVENUTI", "GEVAUDAN")))
  expect_true(any(grepl("BC_NUMBER duplique", res$errors)))
  expect_true(any(grepl("22", res$errors)))
})

test_that("parse: SAMPLE_ID duplique -> erreur", {
  res <- parse_s5_table(make_s5_df(c("CTL", "PAN"), c("22", "23"),
                                   c("26P1470", "26P1470"),
                                   c("BENVENUTI", "GEVAUDAN")))
  expect_true(any(grepl("SAMPLE_ID duplique", res$errors)))
  expect_true(any(grepl("26P1470", res$errors)))
})

test_that("parse: NA (cellules vides handsontable) -> \"\"", {
  df <- make_s5_df(c("CTL", "PAN"), c("22", "23"),
                   c("26P1470", "26P1475"), c("BENVENUTI", NA))
  res <- parse_s5_table(df)
  expect_equal(nrow(res$data), 2)
  expect_equal(res$errors, character(0))
  expect_equal(res$data$PATIENT_ID[2], "")
})

test_that("manifest: lignes exactes du spec", {
  res <- parse_s5_table(make_s5_df(c("CTL", "PAN"), c("22", "23"),
                                   c("26P1470", "26P1475"),
                                   c("BENVENUTI", "GEVAUDAN")))
  lines <- s5_manifest_csv_lines(build_s5_manifest(res$data))
  expect_equal(lines[1], "CSV Version (required),2.3")
  expect_equal(
    lines[2],
    "Barcode,Control Type,Sample Name (required),Sample ID,Sample Description,DNA/RNA/Fusions,Reference library,Target regions BED file,Hotspot regions BED file"
  )
  expect_equal(lines[3], "Ion_V2_RBC1_BC22,,26P1470 BEN CTL,,,RNA,,,")
  expect_equal(lines[4], "Ion_V2_RBC1_BC23,,26P1475 GEV PAN,,,RNA,,,")
})

test_that("manifest: REPASSE cochee (TRUE) -> appendue au Sample Name", {
  res <- parse_s5_table(make_s5_df(c("CTL", "PAN"), c("22", "23"),
                                   c("26P1470", "26P1475"),
                                   c("BENVENUTI", "GEVAUDAN"),
                                   repasses = c(NA, TRUE)))
  lines <- s5_manifest_csv_lines(build_s5_manifest(res$data))
  expect_equal(lines[3], "Ion_V2_RBC1_BC22,,26P1470 BEN CTL,,,RNA,,,")
  expect_equal(lines[4], "Ion_V2_RBC1_BC23,,26P1475 GEV PAN REPASSE,,,RNA,,,")
})

test_that("manifest: REPASSE FALSE/NA -> pas de suffixe", {
  res <- parse_s5_table(make_s5_df(c("CTL", "PAN"), c("22", "23"),
                                   c("26P1470", "26P1475"),
                                   c("BENVENUTI", "GEVAUDAN"),
                                   repasses = c(FALSE, NA)))
  lines <- s5_manifest_csv_lines(build_s5_manifest(res$data))
  expect_equal(lines[3], "Ion_V2_RBC1_BC22,,26P1470 BEN CTL,,,RNA,,,")
  expect_equal(lines[4], "Ion_V2_RBC1_BC23,,26P1475 GEV PAN,,,RNA,,,")
})

test_that("parse: REPASSE au format texte historique (\"REPASSE\") reste geree", {
  res <- parse_s5_table(make_s5_df(c("CTL"), c("22"),
                                   c("26P1470"), c("BENVENUTI"),
                                   repasses = c("REPASSE")))
  expect_equal(res$data$REPASSE, "REPASSE")
  expect_equal(res$errors, character(0))
})

test_that("manifest: PATIENT_ID court (< 3 caracteres) garde le nom entier", {
  res <- parse_s5_table(make_s5_df("CTL", "22", "26P1470", "LI"))
  manifest <- build_s5_manifest(res$data)
  expect_equal(manifest[["Sample Name (required)"]], "26P1470 LI CTL")
})

test_that("manifest: PATIENT_ID vide -> pas d'espace double", {
  res <- parse_s5_table(make_s5_df("CTL", "22", "26P1470", ""))
  manifest <- build_s5_manifest(res$data)
  expect_equal(manifest[["Sample Name (required)"]], "26P1470 CTL")
})

test_that("parse: df au schema standard (pas de colonnes S5) -> erreur explicite, pas de crash", {
  std <- data.frame(
    SAMPLE_ID = rep("", 8), SAMPLE_DESCRIPTION = rep("", 8),
    WELL_ID = rep("", 8), LANE = rep("1+2", 8),
    stringsAsFactors = FALSE
  )
  res <- parse_s5_table(std)
  expect_equal(nrow(res$data), 0)
  expect_true(any(grepl("colonnes manquantes", res$errors, fixed = TRUE)))
})

test_that("parse: colonnes S5 presentes mais REPASSE manquante -> gere sans crash", {
  s4 <- data.frame(
    ANALYSIS_TYPE = "CTL", BC_NUMBER = "22",
    SAMPLE_ID = "26P1470", PATIENT_ID = "BENVENUTI",
    stringsAsFactors = FALSE
  )
  res <- parse_s5_table(s4)
  expect_equal(nrow(res$data), 1)
  expect_equal(res$errors, character(0))
  expect_equal(res$data$REPASSE, "")
})

test_that("manifest_df: 2 lignes d'entete + donnees, meme longueur que le CSV", {
  res <- parse_s5_table(make_s5_df("CTL", "22", "26P1470", "BENVENUTI"))
  df <- build_s5_manifest_df(res$data)
  expect_equal(nrow(df), 3)
  expect_equal(df$Barcode, c("CSV Version (required)", "Barcode", "Ion_V2_RBC1_BC22"))
  lines <- s5_manifest_csv_lines(build_s5_manifest(res$data))
  expect_equal(nrow(build_s5_manifest_df(res$data)), length(lines))
})
