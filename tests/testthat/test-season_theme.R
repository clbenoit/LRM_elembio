# tests/testthat/test-season_theme.R
box::use(
  testthat[expect_equal, expect_false, expect_null, expect_error, expect_true, test_that],
  app/logic/season_theme[easter_sunday, last_sunday, nth_sunday, season_defs, active_season],
  app/main[ui],
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

test_that("last_sunday : dernier dimanche du mois (plusieurs annees)", {
  # fev 2026 : 28 jours (samedi), dernier dimanche = 22
  expect_equal(last_sunday(2026, 2), as.Date("2026-02-22"))
  # mai 2026 : 31 jours (dimanche), dernier dimanche = 31
  expect_equal(last_sunday(2026, 5), as.Date("2026-05-31"))
  # mai 2024 (vendredi 31) = 26 ; mai 2025 (samedi 31) = 25
  expect_equal(last_sunday(2024, 5), as.Date("2024-05-26"))
  expect_equal(last_sunday(2025, 5), as.Date("2025-05-25"))
  # mai 2027 (lundi 31) = 30 ; mai 2030 (jeudi 31) = 26
  expect_equal(last_sunday(2027, 5), as.Date("2027-05-30"))
  expect_equal(last_sunday(2030, 5), as.Date("2030-05-26"))
  # decembre (mois court de fin d'annee)
  expect_equal(last_sunday(2026, 12), as.Date("2026-12-27"))
})

test_that("season_defs : 14 saisons completes, toutes animees (petits emojis)", {
  defs <- season_defs()
  expect_length(names(defs), 14L)
  expect_false("ete" %in% names(defs))
  for (id in names(defs)) {
    expect_true(!is.null(defs[[id]]$priority), label = id)
    expect_true(nchar(defs[[id]]$emoji) > 0, label = paste0(id, " emoji"))
    expect_true(nchar(defs[[id]]$label) > 0, label = paste0(id, " label"))
    # toutes les saisons ont une couche d'emojis animee (petits emojis qui tombent)
    expect_true(!is.null(defs[[id]]$anim), label = paste0(id, " anim"))
    expect_true(length(defs[[id]]$emojis_fx) >= 1, label = paste0(id, " fx"))
  }
  # les 4 saisons ex-sans-animation sont en "fall" ; valentin (rise) / poisson (swim)
  # conservent leur animation historique
  for (id in c("vacances", "rentree", "meres", "peres")) {
    expect_equal(defs[[id]]$anim, "fall", label = id)
  }
  expect_equal(defs[["valentin"]]$anim, "rise")
  expect_equal(defs[["poisson"]]$anim, "swim")
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

test_that("meres / peres 2026 : fenetres [dimanche ref - 4 j, dimanche ref]", {
  # dernier dimanche de mai 2026 = 31 mai -> fenetre 27 -> 31 mai
  expect_null(active_season(as.Date("2026-05-26")))
  expect_true(active_season(as.Date("2026-05-27"))$id == "meres")
  expect_true(active_season(as.Date("2026-05-31"))$id == "meres")
  expect_null(active_season(as.Date("2026-06-01")))
  # 3e dimanche de juin 2026 = 21 juin -> fenetre 17 -> 21 juin
  expect_null(active_season(as.Date("2026-06-16")))
  expect_true(active_season(as.Date("2026-06-17"))$id == "peres")
  expect_true(active_season(as.Date("2026-06-21"))$id == "peres")
  expect_null(active_season(as.Date("2026-06-22")))
})

test_that("vacances / rentree : bornes + automne > rentree du 1er au 7 sept", {
  expect_null(active_season(as.Date("2026-06-30")))
  expect_true(active_season(as.Date("2026-07-01"))$id == "vacances")
  expect_true(active_season(as.Date("2026-08-31"))$id == "vacances")
  expect_true(active_season(as.Date("2026-09-01"))$id == "automne")
  expect_true(active_season(as.Date("2026-09-07"))$id == "automne")
  expect_true(active_season(as.Date("2026-09-08"))$id == "rentree")
  expect_true(active_season(as.Date("2026-09-12"))$id == "rentree")
  expect_null(active_season(as.Date("2026-09-13")))
})

test_that("saisons calendaires : plages de 7 j au debut de saison", {
  # hiver 1 -> 7 dec (pas de chevauchement avec noel 15 -> 26 dec)
  expect_null(active_season(as.Date("2026-11-30")))
  expect_true(active_season(as.Date("2026-12-01"))$id == "hiver")
  expect_true(active_season(as.Date("2026-12-07"))$id == "hiver")
  expect_null(active_season(as.Date("2026-12-08")))
  expect_true(active_season(as.Date("2026-12-15"))$id == "noel")
  # printemps 1 -> 7 mars
  expect_null(active_season(as.Date("2026-02-28")))
  expect_true(active_season(as.Date("2026-03-01"))$id == "printemps")
  expect_true(active_season(as.Date("2026-03-07"))$id == "printemps")
  expect_null(active_season(as.Date("2026-03-08")))
  # automne 1 -> 7 sept (voir test rentree ci-dessus)
  expect_true(active_season(as.Date("2026-09-03"))$id == "automne")
  # pas de saison ete : mi-juillet / mi-aout = vacances
  expect_true(active_season(as.Date("2026-07-15"))$id == "vacances")
  expect_true(active_season(as.Date("2026-08-20"))$id == "vacances")
})

test_that("saisons calendaires : emoji / banner / fx", {
  s <- active_season(as.Date("2026-01-01"), override = "hiver")
  expect_equal(s$emoji, "❄️")
  expect_equal(s$banner, "C'est l'hiver !")
  expect_equal(s$fx$emojis, c("❄️", "⛄"))
  s <- active_season(as.Date("2026-01-01"), override = "printemps")
  expect_equal(s$emoji, "🌷")
  expect_equal(s$banner, "Bienvenue au printemps !")
  expect_equal(s$fx$emojis, c("🌷", "🌸"))
  s <- active_season(as.Date("2026-01-01"), override = "automne")
  expect_equal(s$emoji, "🍂")
  expect_equal(s$banner, "C'est l'automne !")
  expect_equal(s$fx$emojis, c("🍂", "🍁"))
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

test_that("par saison : emoji/banner/css/fx (les 14 saisons animees)", {
  defs <- season_defs()
  for (id in names(defs)) {
    s <- active_season(as.Date("2026-01-01"), override = id)
    expect_true(!is.null(s), label = id)
    expect_true(nchar(s$emoji) > 0, label = paste0(id, " emoji"))
    expect_true(nchar(s$banner) > 0, label = paste0(id, " banner"))
    expect_true(grepl("--lrm-banner", s$css, fixed = TRUE), label = paste0(id, " css bandeau"))
    expect_true(grepl("lrm-fx", s$css, fixed = TRUE), label = paste0(id, " css fx"))
    expect_true(!is.null(s$fx) && length(s$fx$emojis) >= 1 && s$fx$n >= 5,
                label = paste0(id, " spec fx"))
  }
  # emojis specifiques (retours user 2026-10-14)
  expect_equal(active_season(as.Date("2026-01-01"), override = "noel")$fx$emojis,
               c("❄️", "🎁", "🎅"))
  expect_equal(active_season(as.Date("2026-01-01"), override = "vacances")$fx$emojis,
               c("⛱️", "🕶️", "🍹", "🍦"))
  expect_equal(active_season(as.Date("2026-01-01"), override = "rentree")$fx$emojis,
               c("🎒", "✏️", "📚"))
  expect_equal(active_season(as.Date("2026-01-01"), override = "meres")$fx$emojis, "💐")
  expect_equal(active_season(as.Date("2026-01-01"), override = "peres")$fx$emojis, "🎩")
  # banniere valentin "tout court"
  expect_equal(active_season(as.Date("2026-01-01"), override = "valentin")$banner,
               "C'est la Saint-Valentin")
})

test_that("ui : timer 10 s (masquage .lrm-fx) injecte uniquement si saison active", {
  Sys.unsetenv("LRM_SEASON")
  ui_neutre <- as.character(ui("app"))
  expect_false(any(grepl("10000", ui_neutre, fixed = TRUE)))
  local({
    Sys.setenv(LRM_SEASON = "noel")
    on.exit(Sys.unsetenv("LRM_SEASON"))
    ui_saison <- as.character(ui("app"))
    expect_true(any(grepl("10000", ui_saison, fixed = TRUE)))
    expect_true(any(grepl("lrm-fx", ui_saison, fixed = TRUE)))
    # le titre garde l'emoji de saison + LRM_CHUGA
    expect_true(any(grepl("🎄 LRM_CHUGA", ui_saison, fixed = TRUE)))
  })
})
