# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-engine.R · v0.2.3 · 2026-10-16 16:08 · Publication du 16 octobre
# fpa-engine.R — Point d'entree du moteur FPA local
#
# Ce fichier est du texte lisible. Il ne contient aucun binaire.
#
# Utilisation, au choix:
#   1) Dans RStudio: ouvrez ce fichier, puis Source (Ctrl+Shift+S).
#   2) Dans un terminal:
#        Rscript fpa-engine.R "C:/FPA-Local/fpa-config.json"
#   3) Sans argument: le script cherche fpa-config.json a cote de lui,
#      puis dans config/, puis dans data/fixtures/config/.

# --- Localiser ce script, quel que soit le mode de lancement ----------------
fpa_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  hit <- grep("^--file=", args, value = TRUE)
  if (length(hit)) {
    return(normalizePath(dirname(sub("^--file=", "", hit[[1]])), winslash = "/"))
  }
  for (i in seq_len(sys.nframe())) {
    of <- sys.frame(i)$ofile
    if (!is.null(of) && nzchar(as.character(of))) {
      return(normalizePath(dirname(of), winslash = "/"))
    }
  }
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    ctx <- tryCatch(rstudioapi::getSourceEditorContext(), error = function(e) NULL)
    p <- if (!is.null(ctx)) ctx$path else ""
    if (is.character(p) && nzchar(p) && file.exists(p)) {
      return(normalizePath(dirname(p), winslash = "/"))
    }
  }
  normalizePath(getwd(), winslash = "/")
}

FPA_HOME <- fpa_script_dir()
Sys.setenv(FPA_HOME = FPA_HOME)

# --- Trouver le fichier de reglages -----------------------------------------
fpa_find_config <- function(home = FPA_HOME) {
  cli <- commandArgs(trailingOnly = TRUE)
  cli <- cli[nzchar(cli)]
  if (length(cli) && file.exists(cli[[1]])) {
    return(normalizePath(cli[[1]], winslash = "/"))
  }
  extra <- character(0)
  env_home <- Sys.getenv("FPA_HOME", unset = "")
  if (nzchar(env_home)) extra <- c(extra, file.path(env_home, "fpa-config.json"))
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    ctx <- tryCatch(rstudioapi::getSourceEditorContext(), error = function(e) NULL)
    p <- if (!is.null(ctx)) ctx$path else ""
    if (is.character(p) && nzchar(p) && file.exists(p)) {
      extra <- c(extra, file.path(dirname(p), "fpa-config.json"))
    }
  }
  candidates <- c(
    file.path(home, "fpa-config.json"),
    extra,
    file.path(home, "config", "fpa-config.json"),
    file.path(home, "data", "fixtures", "config", "fpa-config.json"),
    file.path(getwd(), "fpa-config.json")
  )
  for (p in candidates) {
    if (file.exists(p)) return(normalizePath(p, winslash = "/"))
  }
  stop(
    "Fichier de reglages introuvable.\n",
    "Dossier du script : ", normalizePath(home, winslash = "\\", mustWork = FALSE), "\n",
    "Dossier de travail R : ", normalizePath(getwd(), winslash = "\\", mustWork = FALSE), "\n",
    "Placez fpa-config.json a cote de fpa-engine.R, ou dans RStudio: ",
    "Session > Set Working Directory > To Source File Location, puis Source.",
    call. = FALSE
  )
}

# --- Verifier la seule dependance -------------------------------------------
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop(
    "Le paquet R 'jsonlite' est requis pour lire fpa-config.json.\n",
    "Installez-le une fois: install.packages(\"jsonlite\")",
    call. = FALSE
  )
}

# --- Charger le moteur -------------------------------------------------------
source(file.path(FPA_HOME, "R", "fpa-paths.R"), encoding = "UTF-8")
source(file.path(FPA_HOME, "R", "fpa-config.R"), encoding = "UTF-8")
fpa_source_all(FPA_HOME)

# Chemins affiches a l'utilisateur: backslash Windows, pas de melange.
fpa_show_path <- function(...) {
  p <- file.path(...)
  normalizePath(p, winslash = "\\", mustWork = FALSE)
}

fpa_eur <- function(x) {
  if (is.null(x) || !length(x) || is.na(x)) return("n/d")
  formatC(round(as.numeric(x)), format = "f", digits = 0, big.mark = " ")
}

# Core files the user must supply before anything can be computed.
FPA_CORE_ROLES <- c(
  pnl_actuals = "compte de resultat",
  opening_bs = "bilan d'ouverture",
  budget = "budget"
)

#' Declared core files that are not on disk yet, as role -> declared path.
fpa_missing_core <- function(config_path) {
  config <- fpa_load_config(config_path)
  missing <- character(0)
  for (role in names(FPA_CORE_ROLES)) {
    declared <- config$paths[[role]]
    if (is.null(declared) || !nzchar(as.character(declared))) next
    if (!file.exists(as.character(declared))) {
      missing[[role]] <- as.character(declared)
    }
  }
  missing
}

fpa_main <- function() {
  FPA_HOME <<- fpa_script_dir()
  Sys.setenv(FPA_HOME = FPA_HOME)
  if (exists("fpa_sync_pull", mode = "function")) {
    pulled <- fpa_sync_pull(home = FPA_HOME)
    if (!is.null(pulled$message) && nzchar(pulled$message)) cat(pulled$message, "\n", sep = "")
    if (isTRUE(pulled$stop)) return(invisible(pulled))
  }
  config_path <- fpa_find_config(FPA_HOME)

  # 1. Creer le dossier de travail et les tables modeles (jamais d'ecrasement)
  init <- suppressMessages(fpa_init(config_path))
  if (length(init$created) || length(init$written)) {
    cat("\nPrepare  : ", length(init$created), " dossier(s), ",
        length(init$written), " fichier(s) modele.\n", sep = "")
  }

  # 2. Premier controle: chaque fichier declare vs le JSON.
  #    En cas de point bloquant, on s'arrete ici (pas de traceback R).
  config <- fpa_load_config(config_path)
  paths <- fpa_paths(config)
  diag <- fpa_diagnose(config)
  fpa_diagnose_print(diag, config_path, init$working)
  diag_file <- tryCatch(
    fpa_diagnose_write(diag, paths, config_path),
    error = function(e) NULL
  )
  if (!is.null(diag_file) && file.exists(diag_file)) {
    cat("  Copie du diagnostic : ", fpa_show_path(diag_file), "\n\n", sep = "")
  }
  if (isTRUE(diag$blocking)) {
    if (!interactive()) {
      quit(save = "no", status = 1L, runLast = FALSE)
    }
    return(invisible(diag))
  }

  # 3. Calculer les trois etats
  res <- fpa_run(config_path, write = TRUE, stop_on_error = TRUE)
  if (exists("fpa_sync_report_run", mode = "function")) {
    tryCatch(fpa_sync_report_run(FPA_HOME, config_path, res), error = function(e) NULL)
  }

  cat("\n--- Resultat ---\n")
  if (is.null(res$pnl) || !nrow(res$pnl)) {
    cat("Aucune periode calculee. Verifiez les chemins de vos fichiers.\n")
    return(invisible(res))
  }
  periodes <- res$pnl$periode
  unit <- if (identical(res$grain, "year")) "exercice(s)" else "mois"
  cat("Periodes : ", periodes[[1]], " a ", periodes[[length(periodes)]],
      " (", length(periodes), " ", unit, ")\n", sep = "")
  cat("CA total : ", fpa_eur(sum(res$pnl$REV, na.rm = TRUE)), "\n", sep = "")
  cat("Resultat net total : ", fpa_eur(sum(res$pnl$NI, na.rm = TRUE)), "\n", sep = "")
  if (!is.null(res$cash) && nrow(res$cash)) {
    cat("Tresorerie finale  : ",
        fpa_eur(res$cash$cash_close[[nrow(res$cash)]]), "\n", sep = "")
  }
  if (!is.null(res$identity)) {
    cat("Bilan equilibre    : ",
        if (isTRUE(res$identity$ok)) "oui, tous les mois" else "NON",
        "\n", sep = "")
  }

  if (!is.null(res$findings) && nrow(res$findings)) {
    cat("\n--- Points a verifier ---\n")
    cat(fpa_format_findings(res$findings), "\n", sep = "")
  }

  drv <- tryCatch(
    fpa_driver_pass(config_path, res = res, write = TRUE),
    error = function(e) {
      cat("\n--- Drivers ---\n")
      cat("Calcul des drivers interrompu: ", conditionMessage(e), "\n", sep = "")
      NULL
    }
  )
  if (!is.null(drv)) {
    cat("\n--- Drivers ---\n")
    cat(drv$summary_text, "\n", sep = "")
    if (!is.null(drv$findings) && nrow(drv$findings)) {
      cat(fpa_format_findings(drv$findings), "\n", sep = "")
    }
  }

  cat("\n--- Fichiers ecrits ---\n")
  for (f in c(
    "pnl-monthly.csv",
    "bs-monthly.csv",
    "cash-bridge-monthly.csv",
    "budget-monthly.csv"
  )) {
    p <- file.path(res$paths$work, f)
    if (file.exists(p)) cat(" ", fpa_show_path(p), "\n", sep = "")
  }
  cat("\nAucune donnee n'a quitte ce poste.\n\n")
  invisible(res)
}

if (!isTRUE(getOption("fpa.skip_main"))) {
  fpa_result <- fpa_main()
}
