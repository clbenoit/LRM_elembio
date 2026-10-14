# app/logic/season_theme.R
# Moteur de saisons de l'UI LRM_CHUGA : selection de la periode de l'annee
# active (11 evenements, dates fixes ou calculees) + payload (emoji, bandeau,
# CSS, spec d'animation). Fonctions pures (pas de Shiny) => testables.
# L'application cote (titre navbar, style, couche d'animation) est dans app/main.R.

#' Date du dimanche de Paques (algorithme gregorien, Meeus/Jones/Butcher).
#'
#' @param year annee entiere (ex. 2026)
#' @return Date du dimanche de Paques
easter_sunday <- function(year) {
  a  <- year %% 19
  b  <- year %/% 100
  cc <- year %% 100
  d  <- b %/% 4
  e  <- b %% 4
  f  <- (b + 8) %/% 25
  g  <- (b - f + 1) %/% 3
  h  <- (19 * a + b - d - g + 15) %% 30
  i  <- cc %/% 4
  k  <- cc %% 4
  l  <- (32 + 2 * e + 2 * i - h - k) %% 7
  m  <- (a + 11 * h + 22 * l) %/% 451
  month <- (h + l - 7 * m + 114) %/% 31
  day   <- ((h + l - 7 * m + 114) %% 31) + 1
  as.Date(paste(year, month, day, sep = "-"))
}

#' n-ieme dimanche d'un mois (independant du locale, via %u : 1=lundi .. 7=dimanche).
#'
#' @param year annee entiere
#' @param month mois entier (1-12)
#' @param n n-ieme dimanche (1-based)
#' @return Date du n-ieme dimanche du mois
nth_sunday <- function(year, month, n) {
  first <- as.Date(paste(year, month, 1, sep = "-"))
  first + (7 - as.integer(format(first, "%u"))) %% 7 + 7 * (n - 1)
}

#' Nombre de jours d'un mois.
#'
#' @param year annee entiere
#' @param month mois entier (1-12)
#' @return nombre de jours du mois
days_in_month <- function(year, month) {
  prev_first <- as.Date(paste0(year, "-", sprintf("%02d", month), "-01"))
  next_first <- as.Date(paste0(
    year + as.integer(month == 12), "-", sprintf("%02d", (month %% 12) + 1), "-01"
  ))
  as.integer(next_first - prev_first)
}

#' Dernier dimanche d'un mois (independant du locale, via %u : 1=lundi .. 7=dimanche).
#'
#' @param year annee entiere
#' @param month mois entier (1-12)
#' @return Date du dernier dimanche du mois
last_sunday <- function(year, month) {
  last_day <- as.Date(paste0(year, "-", sprintf("%02d", month), "-01")) +
    days_in_month(year, month) - 1
  last_day - (as.integer(format(last_day, "%u")) %% 7)
}

#' Table declarative des 11 saisons.
#'
#' Chaque definition : priority (la plus haute gagne en cas de chevauchement),
#' emoji, label, banner (string ou fonction de date), start/end (fonctions de
#' la date renvoyant des Date), et optionnellement anim/emojis_fx (animation).
season_defs <- function() {
  y <- function(date) as.integer(format(date, "%Y"))
  fixed <- function(mmdd) function(date) as.Date(paste0(y(date), "-", mmdd))
  list(
    valentin = list(
      priority = 70, emoji = "❤️", label = "Saint-Valentin",
      banner = "C'est la Saint-Valentin !",
      start = fixed("02-12"), end = fixed("02-15"),
      anim = "rise", emojis_fx = c("❤️", "💖")
    ),
    mardi_gras = list(
      priority = 75, emoji = "🎭", label = "Mardi Gras",
      banner = "Joyeux Carnaval !",
      start = function(date) easter_sunday(y(date)) - 50,
      end   = function(date) easter_sunday(y(date)) - 46,
      anim = "fall", emojis_fx = c("🎊", "🎉")
    ),
    paques = list(
      priority = 65, emoji = "🐣", label = "Pâques",
      banner = "Bonne Pâques !",
      start = function(date) easter_sunday(y(date)) - 3,
      end   = function(date) easter_sunday(y(date)) + 1,
      anim = "fall", emojis_fx = c("🥚", "🐣", "🌷")
    ),
    poisson = list(
      priority = 100, emoji = "🐟", label = "Poisson d'avril",
      banner = "C'est le poisson d'avril !",
      start = fixed("03-31"), end = fixed("04-02"),
      anim = "swim", emojis_fx = c("🐟", "🐠")
    ),
    meres = list(
      priority = 50, emoji = "💐", label = "Fête des mères",
      banner = "Joyeuse fête des mères !",
      start = function(date) last_sunday(y(date), 5) - 4,
      end   = function(date) last_sunday(y(date), 5),
      anim = "fall", emojis_fx = c("💐")
    ),
    peres = list(
      priority = 50, emoji = "🎩", label = "Fête des pères",
      banner = "Joyeuse fête des pères !",
      start = function(date) nth_sunday(y(date), 6, 3) - 4,
      end   = function(date) nth_sunday(y(date), 6, 3),
      anim = "fall", emojis_fx = c("🎩")
    ),
    vacances = list(
      priority = 60, emoji = "☀️", label = "Vacances d'été",
      banner = "Bonnes vacances !",
      start = fixed("07-01"), end = fixed("08-31"),
      anim = "fall", emojis_fx = c("⛱️", "🕶️", "🍹", "🍦")
    ),
    rentree = list(
      priority = 60, emoji = "📚", label = "Rentrée",
      banner = "Bonne rentrée !",
      start = fixed("09-01"), end = fixed("09-12"),
      anim = "fall", emojis_fx = c("🎒", "✏️", "📚")
    ),
    halloween = list(
      priority = 70, emoji = "🎃", label = "Halloween",
      banner = "Happy Halloween !",
      start = fixed("10-23"), end = fixed("10-31"),
      anim = "fall", emojis_fx = c("🎃", "🦇")
    ),
    noel = list(
      priority = 80, emoji = "🎄", label = "Noël",
      banner = "Joyeuses fêtes de fin d'année !",
      start = fixed("12-15"), end = fixed("12-26"),
      anim = "fall", emojis_fx = c("❄️", "🎁", "🎅")
    ),
    nouvel_an = list(
      priority = 90, emoji = "🎆", label = "Nouvel An",
      banner = function(date) {
        target_year <- if (as.integer(format(date, "%m")) >= 12) y(date) + 1 else y(date)
        paste0("Bonne & heureuse année ", target_year, " !")
      },
      start = function(date) {
        target_year <- if (as.integer(format(date, "%m")) >= 12) y(date) + 1 else y(date)
        as.Date(paste0(target_year - 1, "-12-29"))
      },
      end = function(date) {
        target_year <- if (as.integer(format(date, "%m")) >= 12) y(date) + 1 else y(date)
        as.Date(paste0(target_year, "-01-03"))
      },
      anim = "fall", emojis_fx = c("🎊", "✨", "🎉")
    ),
    # ---- Saisons calendaires : plage de 7 j au debut meteorologique de saison
    # (pas de saison ete : la plage "vacances" 1 juil -> 31 aout couvre la periode)
    printemps = list(
      priority = 65, emoji = "🌷", label = "Printemps",
      banner = "Bienvenue au printemps !",
      start = fixed("03-01"), end = fixed("03-07"),
      anim = "fall", emojis_fx = c("🌷", "🌸")
    ),
    automne = list(
      priority = 65, emoji = "🍂", label = "Automne",
      banner = "C'est l'automne !",
      start = fixed("09-01"), end = fixed("09-07"),
      anim = "fall", emojis_fx = c("🍂", "🍁")
    ),
    hiver = list(
      priority = 65, emoji = "❄️", label = "Hiver",
      banner = "C'est l'hiver !",
      start = fixed("12-01"), end = fixed("12-07"),
      anim = "fall", emojis_fx = c("❄️", "⛄")
    )
  )
}

#' CSS de la saison (injecte dans l'ui).
#'
#' @param id id d'une saison (cles de season_defs)
#' @param banner message du bandeau (resolu)
#' @return character(1) : CSS
season_css <- function(id, banner) {
  def <- season_defs()[[id]]
  anim <- if (is.null(def$anim)) "" else def$anim

  palette <- list(
    valentin   = c(nav = "#9d174d", bbg = "#f9a8d4", bfg = "#831843"),
    mardi_gras = c(nav = "#4c1d95", bbg = "#f59e0b", bfg = "#451a03"),
    paques     = c(nav = "#6b9080", bbg = "#e9d5ff", bfg = "#4c1d95"),
    poisson    = c(nav = "#0e7490", bbg = "#a5f3fc", bfg = "#155e75"),
    meres      = c(nav = "#a78bfa", bbg = "#fbcfe8", bfg = "#831843"),
    peres      = c(nav = "#334155", bbg = "#cbd5e1", bfg = "#1e293b"),
    vacances   = c(nav = "#0369a1", bbg = "#fde047", bfg = "#713f12"),
    rentree    = c(nav = "#1d4ed8", bbg = "#bfdbfe", bfg = "#1e3a8a"),
    printemps  = c(nav = "#15803d", bbg = "#bbf7d0", bfg = "#14532d"),
    automne    = c(nav = "#9a3412", bbg = "#fed7aa", bfg = "#7c2d12"),
    hiver      = c(nav = "#0c4a6e", bbg = "#bae6fd", bfg = "#082f49"),
    halloween  = c(nav = "#2d1b4e", bbg = "#f97316", bfg = "#431407"),
    noel       = c(nav = "#14532d", bbg = "#7f1d1d", bfg = "#fef2f2"),
    nouvel_an  = c(nav = "#111827", bbg = "#fbbf24", bfg = "#451a03")
  )[[id]]

  keyframes <- switch(anim,
    fall = "@keyframes lrm-fall { from { transform: translateY(0) rotate(0deg); } to { transform: translateY(130vh) rotate(360deg); } }",
    rise = "@keyframes lrm-rise { from { transform: translateY(0) rotate(0deg); } to { transform: translateY(-130vh) rotate(24deg); } }",
    swim = "@keyframes lrm-swim { from { transform: translateX(-15vw) rotate(-8deg); } to { transform: translateX(115vw) rotate(8deg); } }",
    ""
  )

  span_pos <- switch(anim,
    fall = "top: -14vh;",
    rise = "bottom: -14vh; top: auto;",
    swim = "left: 0;",
    ""
  )

  fx_css <- if (nzchar(anim)) paste0(
    ".lrm-fx { position: fixed; top: 0; left: 0; width: 100%; height: 100%;\n",
    "  overflow: hidden; pointer-events: none; z-index: 1038; }\n",
    ".lrm-fx > span { position: absolute; ", span_pos, "\n",
    "  animation-name: lrm-", anim, "; animation-timing-function: linear;\n",
    "  animation-iteration-count: infinite; opacity: 0.8; }\n",
    keyframes, "\n",
    "@media (prefers-reduced-motion: reduce) { .lrm-fx { display: none !important; } }\n"
  ) else ""

  paste0(
    "/* LRM seasonal UI — ", id, " */\n",
    ":root { --lrm-banner: \"", banner, "\"; }\n",
    "body::after { content: var(--lrm-banner); position: fixed; bottom: 0;\n",
    "  left: 0; right: 0; text-align: center; padding: 6px 0; font-size: 0.85rem;\n",
    "  background: ", palette[["bbg"]], "; color: ", palette[["bfg"]], ";\n",
    "  z-index: 1039; pointer-events: none; }\n",
    "body { padding-bottom: 44px; }\n",
    ".navbar { background: ", palette[["nav"]], " !important; }\n",
    ".navbar .navbar-brand, .navbar .nav-link, .navbar .nav-item { color: #ffffff !important; }\n",
    ".navbar .nav-link:hover, .navbar .nav-link:focus { color: #ffffff !important; }\n",
    fx_css
  )
}

#' Payload complet d'une saison (id, label, emoji, banner, css, fx).
season_payload <- function(id, def, date) {
  banner <- if (is.function(def$banner)) def$banner(date) else def$banner
  fx <- if (is.null(def$anim)) NULL else
    list(anim = def$anim, emojis = def$emojis_fx, n = 18L)
  list(
    id = id, label = def$label, emoji = def$emoji, banner = banner,
    css = season_css(id, banner), fx = fx
  )
}

#' Saison active pour une date donnee.
#'
#' @param date Date (defaut : date locale du serveur)
#' @param override NULL (=> lit l'env LRM_SEASON), "auto", un id de saison, ou "none"
#' @return list(id, label, emoji, banner, css, fx) ou NULL (UI neutre)
active_season <- function(date = Sys.Date(), override = NULL) {
  if (is.null(override)) override <- Sys.getenv("LRM_SEASON", "auto")
  override <- trimws(override)
  defs <- season_defs()

  if (!identical(override, "auto")) {
    if (identical(override, "none")) return(NULL)
    if (is.null(defs[[override]])) {
      stop("LRM_SEASON inconnu : '", override,
           "' — ids valides : ", paste(names(defs), collapse = ", "),
           " (ou 'none' / 'auto')", call. = FALSE)
    }
    return(season_payload(override, defs[[override]], date))
  }

  in_window <- vapply(
    defs, function(def) date >= def$start(date) && date <= def$end(date),
    logical(1), USE.NAMES = TRUE
  )
  if (!any(in_window)) return(NULL)
  prio <- vapply(defs, function(d) d$priority, numeric(1))
  id <- names(which.max(prio[in_window]))
  season_payload(id, defs[[id]], date)
}
