# tests/testthat/test-season_theme.R
box::use(
  testthat[expect_equal, expect_null, expect_error, expect_true, test_that],
  app/logic/season_theme[easter_sunday, nth_sunday, season_defs, active_season],
)

test_that("easter_sunday : annees connues", {
  expect_equal(easter_sunday(2018), as.Date("2018-04-01"))
  expect_equal(easter_sunday(2024), as.Date("2024-03-31"))
  expect_equal(easter_sunday(2026), as.Date("2026-04-05"))
  expect_equal(easter_sunday(2027), as.Date("2027-03-28"))
  expect_equal(easter_sunday(2038), as.Date("2038-04-25"))
})

test_that("nth_sunday : 2e dimanche mai / 3e dimanche juin", {
  expect_equal(nth_sunday(2026, 5, 2), as.Date("2026-05-10"))
  expect_equal(nth_sunday(2026, 6, 3), as.Date("2026-06-21"))
  expect_equal(nth_sunday(2027, 5, 2), as.Date("2027-05-09"))
})

test_that("season_defs : 11 saisons completes", {
  defs <- season_defs()
  expect_length(names(defs), 11L)
  for (id in names(defs)) {
    expect_true(!is.null(defs[[id]]$priority), label = id)
    expect_true(nchar(defs[[id]]$emoji) > 0, label = paste0(id, " emoji"))
    expect_true(nchar(defs[[id]]$label) > 0, label = paste0(id, " label"))
  }
})

test_that("halloween : bornes", {
  expect_null(active_season(as.Date("2026-10-22")))
  expect_true(active_season(as.Date("2026-10-23"))$id == "halloween")
  expect_true(active_season(as.Date("2026-10-31"))$id == "halloween")
  expect_null(active_season(as.Date("2026-11-01")))
})

test_that("noel : bornes", {
  expect_null(active_season(as.Date("2026-12-14")))
  expect_true(active_season(as.Date("2026-12-15"))$id == "noel")
  expect_true(active_season(as.Date("2026-12-26"))$id == "noel")
  expect_null(active_season(as.Date("2026-12-27")))
})

test_that("nouvel_an : bornes + annee cible", {
  expect_null(active_season(as.Date("2026-12-28")))
  s <- active_season(as.Date("2026-12-29"))
  expect_true(s$id == "nouvel_an")
  expect_true(s$banner == "Bonne & heureuse année 2027 !")
  expect_true(active_season(as.Date("2026-12-31"))$banner == "Bonne & heureuse année 2027 !")
  expect_true(active_season(as.Date("2027-01-03"))$banner == "Bonne & heureuse année 2027 !")
  expect_null(active_season(as.Date("2027-01-04")))
})

test_that("valentin : bornes (2027, sans chevauchement carnaval)", {
  expect_null(active_season(as.Date("2027-02-11")))
  expect_true(active_season(as.Date("2027-02-12"))$id == "valentin")
  expect_true(active_season(as.Date("2027-02-15"))$id == "valentin")
  expect_null(active_season(as.Date("2027-02-16")))
})

test_that("poisson : bornes (2038, Paques tardive = 25 avril)", {
  expect_null(active_season(as.Date("2038-03-30")))
  expect_true(active_season(as.Date("2038-03-31"))$id == "poisson")
  expect_true(active_season(as.Date("2038-04-02"))$id == "poisson")
  expect_null(active_season(as.Date("2038-04-03")))
})

test_that("paques 2026 : fenetre 2 -> 6 avril", {
  expect_true(active_season(as.Date("2026-04-01"))$id == "poisson")  # poisson seul le 1er
  expect_true(active_season(as.Date("2026-04-02"))$id == "poisson")  # chevauchement : poisson > paques
  expect_true(active_season(as.Date("2026-04-03"))$id == "paques")
  expect_true(active_season(as.Date("2026-04-06"))$id == "paques")
  expect_null(active_season(as.Date("2026-04-07")))
})

test_that("mardi_gras 2026 : fenetre 14 -> 18 fevrier", {
  expect_true(active_season(as.Date("2026-02-13"))$id == "valentin")
  expect_true(active_season(as.Date("2026-02-14"))$id == "mardi_gras")
  expect_true(active_season(as.Date("2026-02-17"))$id == "mardi_gras")
  expect_true(active_season(as.Date("2026-02-18"))$id == "mardi_gras")
  expect_null(active_season(as.Date("2026-02-19")))
})

test_that("meres / peres 2026 : fenetrees calculees", {
  expect_null(active_season(as.Date("2026-05-08")))
  expect_true(active_season(as.Date("2026-05-09"))$id == "meres")
  expect_true(active_season(as.Date("2026-05-11"))$id == "meres")
  expect_null(active_season(as.Date("2026-05-12")))
  expect_null(active_season(as.Date("2026-06-19")))
  expect_true(active_season(as.Date("2026-06-20"))$id == "peres")
  expect_true(active_season(as.Date("2026-06-22"))$id == "peres")
  expect_null(active_season(as.Date("2026-06-23")))
})

test_that("vacances / rentree : bornes + bascule 1er sept", {
  expect_null(active_season(as.Date("2026-06-30")))
  expect_true(active_season(as.Date("2026-07-01"))$id == "vacances")
  expect_true(active_season(as.Date("2026-08-31"))$id == "vacances")
  expect_true(active_season(as.Date("2026-09-01"))$id == "rentree")
  expect_true(active_season(as.Date("2026-09-12"))$id == "rentree")
  expect_null(active_season(as.Date("2026-09-13")))
})

test_that("priorites : poisson > paques (2018)", {
  expect_true(active_season(as.Date("2018-03-30"))$id == "paques")
  expect_true(active_season(as.Date("2018-04-01"))$id == "poisson")
  expect_true(active_season(as.Date("2018-04-02"))$id == "poisson")  # paques et poisson actifs, poisson gagne
  expect_null(active_season(as.Date("2018-04-03")))
})

test_that("priorites : mardi_gras > valentin (2026)", {
  expect_true(active_season(as.Date("2026-02-13"))$id == "valentin")
  expect_true(active_season(as.Date("2026-02-14"))$id == "mardi_gras")
  expect_true(active_season(as.Date("2026-02-15"))$id == "mardi_gras")
})

test_that("override : id explicite / none / valeur inconnue", {
  expect_true(active_season(as.Date("2026-07-01"), override = "halloween")$id == "halloween")
  expect_null(active_season(as.Date("2026-12-20"), override = "none"))
  expect_error(active_season(as.Date("2026-07-01"), override = "boulgour"), "LRM_SEASON")
})

test_that("env var LRM_SEASON", {
  local({
    Sys.setenv(LRM_SEASON = "noel")
    on.exit(Sys.unsetenv("LRM_SEASON"))
    expect_true(active_season(as.Date("2026-07-01"))$id == "noel")
  })
  local({
    Sys.setenv(LRM_SEASON = "none")
    on.exit(Sys.unsetenv("LRM_SEASON"))
    expect_null(active_season(as.Date("2026-12-20")))
  })
})

test_that("hors periode : NULL", {
  expect_null(active_season(as.Date("2026-01-15")))
  expect_null(active_season(as.Date("2026-06-15")))
  expect_null(active_season(as.Date("2026-10-13")))
})

test_that("par saison : emoji/banner/css/fx", {
  defs <- season_defs()
  animated <- c("noel", "halloween", "nouvel_an", "paques", "valentin", "mardi_gras", "poisson")
  for (id in names(defs)) {
    s <- active_season(as.Date("2026-01-01"), override = id)
    expect_true(!is.null(s), label = id)
    expect_true(nchar(s$emoji) > 0, label = paste0(id, " emoji"))
    expect_true(nchar(s$banner) > 0, label = paste0(id, " banner"))
    expect_true(grepl("--lrm-banner", s$css, fixed = TRUE), label = paste0(id, " css bandeau"))
    expect_true(
      grepl("lrm-fx", s$css, fixed = TRUE) == (id %in% animated),
      label = paste0(id, " css fx")
    )
  }
  for (id in animated) {
    fx <- active_season(as.Date("2026-01-01"), override = id)$fx
    expect_true(!is.null(fx) && length(fx$emojis) >= 2 && fx$n >= 5,
                label = paste0(id, " spec fx"))
  }
})
