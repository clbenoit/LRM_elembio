box::use(
  testthat[expect_true, expect_false, expect_equal, expect_error, test_that],
)
box::use(
  app/logic/s5_parser[parse_s5_input, build_s5_manifest, s5_manifest_csv_lines],
)

test_that("parse: input valide 2 lignes", {
  res <- parse_s5_input("CTL 22 26P1470 BENVENUTI\nPAN 23 26P1475 GEVAUDAN")
  expect_equal(nrow(res$data), 2)
  expect_equal(res$errors, character(0))
  expect_equal(res$data$CTRL, c("CTL", "PAN"))
  expect_equal(res$data$BC, c("22", "23"))
  expect_equal(res$data$SAMPLE_ID, c("26P1470", "26P1475"))
  expect_equal(res$data$NOM, c("BENVENUTI", "GEVAUDAN"))
  expect_equal(res$data$row_number, c(1L, 2L))
})

test_that("parse: lignes vides ignorees", {
  res <- parse_s5_input("\nCTL 22 26P1470 BENVENUTI\n\nPAN 23 26P1475 GEVAUDAN\n")
  expect_equal(nrow(res$data), 2)
  expect_equal(res$errors, character(0))
  expect_equal(res$data$row_number, c(2L, 4L))
})

test_that("parse: minuscules normalisees en majuscules", {
  res <- parse_s5_input("pan 23 26P1475 GEVAUDAN")
  expect_equal(res$data$CTRL, "PAN")
  expect_equal(res$errors, character(0))
})

test_that("parse: caracteres en plus tronques aux 3 premiers", {
  res <- parse_s5_input("PANTAL 23 26P1475 GEVAUDAN")
  expect_equal(res$data$CTRL, "PAN")
  expect_equal(res$errors, character(0))
  res2 <- parse_s5_input("ctlef 22 26P1470 BENVENUTI")
  expect_equal(res2$data$CTRL, "CTL")
})

test_that("parse: CTRL inconnu -> erreur, download bloque", {
  res <- parse_s5_input("XYZ 22 26P1470 BENVENUTI")
  expect_true(any(grepl("type de controle inconnu", res$errors, ignore.case = TRUE)))
  expect_true(any(grepl("Ligne 1", res$errors)))
  expect_equal(nrow(res$data), 0)
})

test_that("parse: ligne mal formee -> erreur avec numero de ligne", {
  res <- parse_s5_input("CTL 22 26P1470")
  expect_true(any(grepl("Ligne 1", res$errors)))
  expect_true(any(grepl("4 champs", res$errors)))
  res2 <- parse_s5_input("CTL 22 26P1470 BENVENUTI\nPAN 23")
  expect_true(any(grepl("Ligne 2", res2$errors)))
  expect_equal(nrow(res2$data), 1)
})

test_that("parse: BC duplique -> erreur", {
  res <- parse_s5_input("CTL 22 26P1470 BENVENUTI\nPAN 22 26P1471 GEVAUDAN")
  expect_true(any(grepl("BC duplique", res$errors)))
  expect_true(any(grepl("22", res$errors)))
})

test_that("parse: SAMPLE_ID duplique -> erreur", {
  res <- parse_s5_input("CTL 22 26P1470 BENVENUTI\nPAN 23 26P1470 GEVAUDAN")
  expect_true(any(grepl("SAMPLE_ID duplique", res$errors)))
  expect_true(any(grepl("26P1470", res$errors)))
})

test_that("parse: input vide", {
  res <- parse_s5_input("")
  expect_equal(nrow(res$data), 0)
  expect_equal(res$errors, character(0))
})

test_that("manifest: lignes exactes du spec", {
  res <- parse_s5_input("CTL 22 26P1470 BENVENUTI\nPAN 23 26P1475 GEVAUDAN")
  manifest <- build_s5_manifest(res$data, repasse = c(FALSE, FALSE))
  lines <- s5_manifest_csv_lines(manifest)
  expect_equal(lines[1], "CSV Version (required),2.3")
  expect_equal(
    lines[2],
    "Barcode,Control Type,Sample Name (required),Sample ID,Sample Description,DNA/RNA/Fusions,Reference library,Target regions BED file,Hotspot regions BED file"
  )
  expect_equal(lines[3], "Ion_V2_RBC1_BC22,,26P1470 BEN CTL,,,RNA,,,")
  expect_equal(lines[4], "Ion_V2_RBC1_BC23,,26P1475 GEV PAN,,,RNA,,,")
})

test_that("manifest: REPASSE appende quand coché", {
  res <- parse_s5_input("CTL 22 26P1470 BENVENUTI\nPAN 23 26P1475 GEVAUDAN")
  manifest <- build_s5_manifest(res$data, repasse = c(FALSE, TRUE))
  lines <- s5_manifest_csv_lines(manifest)
  expect_equal(lines[3], "Ion_V2_RBC1_BC22,,26P1470 BEN CTL,,,RNA,,,")
  expect_equal(lines[4], "Ion_V2_RBC1_BC23,,26P1475 GEV PAN REPASSE,,,RNA,,,")
})

test_that("manifest: NA (case decochée) ne ajoute pas REPASSE", {
  res <- parse_s5_input("PAN 23 26P1475 GEVAUDAN")
  manifest <- build_s5_manifest(res$data, repasse = c(NA))
  expect_equal(manifest[["Sample Name (required)"]], "26P1475 GEV PAN")
})

test_that("manifest: NOM court (< 3 caracteres) garde le nom entier", {
  res <- parse_s5_input("CTL 22 26P1470 LI")
  manifest <- build_s5_manifest(res$data, repasse = c(FALSE))
  expect_equal(manifest[["Sample Name (required)"]], "26P1470 LI CTL")
})
