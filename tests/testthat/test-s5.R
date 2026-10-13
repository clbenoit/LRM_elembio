box::use(
  testthat[expect_true, expect_false, expect_equal, expect_error, expect_match,
           test_that],
  shiny[testServer],
)
box::use(
  app/view/s5[ui, server],
)

# Valeur serialisee handsontable (format getData() : lignes x colonnes)
ht_value <- function(repasse) {
  list(
    data = list(
      list("CTL", "22", "26P1470", "BENVENUTI", repasse[[1]]),
      list("PAN", "23", "26P1475", "GEVAUDAN", repasse[[2]])
    ),
    changes = list(event = "afterChange"),
    params = list(
      rClass = "data.frame",
      rColHeaders = list("CTRL", "BC", "SAMPLE_ID", "NOM", "REPASSE"),
      rRowHeaders = list(1L, 2L),
      rColClasses = list(
        CTRL = "character", BC = "character", SAMPLE_ID = "character",
        NOM = "character", REPASSE = "logical"
      )
    )
  )
}

test_that("s5 server : input valide -> preview + download exact", {
  testServer(server, {
    session$setInputs(
      s5_input = "CTL 22 26P1470 BENVENUTI\nPAN 23 26P1475 GEVAUDAN",
      load = 1
    )

    expect_equal(nrow(s5_data()), 2)
    expect_equal(s5_errors(), character(0))

    dl <- output$dlS5
    expect_match(basename(dl), "^S5-\\d{4}-\\d{2}-\\d{2}\\.csv$")
    lines <- readLines(dl)
    expect_equal(lines[1], "CSV Version (required),2.3")
    expect_equal(
      lines[2],
      "Barcode,Control Type,Sample Name (required),Sample ID,Sample Description,DNA/RNA/Fusions,Reference library,Target regions BED file,Hotspot regions BED file"
    )
    expect_equal(lines[3], "Ion_V2_RBC1_BC22,,26P1470 BEN CTL,,,RNA,,,")
    expect_equal(lines[4], "Ion_V2_RBC1_BC23,,26P1475 GEV PAN,,,RNA,,,")
    expect_equal(length(lines), 4)
  })
})

test_that("s5 server : REPASSE coche -> appende au Sample Name", {
  testServer(server, {
    session$setInputs(
      s5_input = "CTL 22 26P1470 BENVENUTI\nPAN 23 26P1475 GEVAUDAN",
      load = 1
    )
    session$setInputs(s5_table = ht_value(list(FALSE, TRUE)))

    dl <- output$dlS5
    lines <- readLines(dl)
    expect_equal(lines[3], "Ion_V2_RBC1_BC22,,26P1470 BEN CTL,,,RNA,,,")
    expect_equal(lines[4], "Ion_V2_RBC1_BC23,,26P1475 GEV PAN REPASSE,,,RNA,,,")
  })
})

test_that("s5 server : CTRL inconnu -> erreur, download bloque", {
  testServer(server, {
    session$setInputs(
      s5_input = "XYZ 22 26P1470 BENVENUTI",
      load = 1
    )

    expect_true(length(s5_errors()) > 0)
    expect_true(any(grepl("type de controle inconnu", s5_errors())))

    expect_error(output$dlS5, "s5_errors")
  })
})

test_that("s5 server : ligne mal formee -> erreur avec numero", {
  testServer(server, {
    session$setInputs(
      s5_input = "CTL 22 26P1470",
      load = 1
    )

    expect_true(any(grepl("Ligne 1", s5_errors())))
  })
})

test_that("s5 server : BC duplique -> erreur", {
  testServer(server, {
    session$setInputs(
      s5_input = "CTL 22 26P1470 BENVENUTI\nPAN 22 26P1471 GEVAUDAN",
      load = 1
    )

    expect_true(any(grepl("BC duplique", s5_errors())))
  })
})

test_that("s5 server : preview rend le handsontable", {
  testServer(server, {
    session$setInputs(
      s5_input = "CTL 22 26P1470 BENVENUTI",
      load = 1
    )
    ui_out <- as.character(output$preview_ui)
    expect_true(any(grepl("s5_table", ui_out)))
    tbl <- as.character(output$s5_table)
    expect_true(any(grepl("BENVENUTI", tbl)))
    expect_true(any(grepl("REPASSE", tbl)))
  })
})

test_that("s5 server : reset -> preview vide", {
  testServer(server, {
    session$setInputs(
      s5_input = "CTL 22 26P1470 BENVENUTI",
      load = 1
    )
    expect_equal(nrow(s5_data()), 1)
    session$setInputs(reset = 1)
    expect_null(s5_data())
    expect_equal(s5_errors(), character(0))
  })
})
