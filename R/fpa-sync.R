# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Pull the published fpa-config.json. Offline, or a refused key, keeps the local file.
# The key is never printed.

fpa_sync_msg <- function(key, ...) {
  if (exists("fpa_txt", mode = "function")) return(fpa_txt(key, ...))
  key
}

fpa_sync_guess <- function(home, config_path) {
  if (nzchar(config_path) && file.exists(config_path)) {
    return(normalizePath(config_path, winslash = "/", mustWork = FALSE))
  }
  env <- Sys.getenv("FPA_CONFIG", unset = "")
  if (nzchar(env) && file.exists(env)) {
    return(normalizePath(env, winslash = "/", mustWork = FALSE))
  }
  beside <- file.path(home, "fpa-config.json")
  if (file.exists(beside)) return(normalizePath(beside, winslash = "/", mustWork = FALSE))
  ""
}

fpa_sync_read <- function(path) {
  if (!nzchar(path) || !file.exists(path)) return(NULL)
  tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) NULL)
}

fpa_sync_link <- function(cfg) {
  p <- if (is.null(cfg)) NULL else cfg$project
  from_file <- function(block) {
    if (is.null(block) || !is.list(block)) return(NULL)
    id <- as.character(block$id %||% "")
    key <- as.character(block$key %||% "")
    site <- sub("/+$", "", as.character(block$site %||% ""))
    if (!nzchar(id) || !nzchar(key) || !grepl("^https?://", site)) return(NULL)
    rev <- suppressWarnings(as.integer(block$sync_revision %||% 0))
    if (is.na(rev)) rev <- 0L
    list(id = id, key = key, site = site, rev = rev)
  }
  hit <- from_file(p)
  if (!is.null(hit)) return(hit)
  if (exists("FPA_LINK", envir = .GlobalEnv, inherits = FALSE)) {
    return(from_file(get("FPA_LINK", envir = .GlobalEnv)))
  }
  NULL
}

fpa_sync_script_version <- function(home) {
  for (name in c("fpa-engine.R", "fpa-moteur.R")) {
    path <- file.path(home, name)
    if (!file.exists(path)) next
    line <- readLines(path, n = 1L, warn = FALSE, encoding = "UTF-8")
    hit <- regmatches(line, regexec("v[0-9]+\\.[0-9]+\\.[0-9]+", line))[[1]]
    if (length(hit)) return(hit[[1]])
  }
  ""
}

fpa_sync_names <- function(items, id_field, label_field) {
  if (!is.list(items) || !length(items)) return(character(0))
  keys <- vapply(items, function(x) as.character(x[[id_field]] %||% x[[label_field]] %||% ""), "")
  labels <- vapply(items, function(x) as.character(x[[label_field]] %||% x[[id_field]] %||% ""), "")
  names(labels) <- keys
  labels[nzchar(keys)]
}

#' What a pull brought in: files by source_id, drivers by column.
fpa_sync_delta <- function(old, new) {
  pick <- function(cfg, field, id_field, label_field) {
    if (is.null(cfg)) return(character(0))
    fpa_sync_names(cfg[[field]], id_field, label_field)
  }
  so <- pick(old, "sources", "source_id", "source_name")
  sn <- pick(new, "sources", "source_id", "source_name")
  do <- pick(old, "driver_bindings", "column", "label")
  dn <- pick(new, "driver_bindings", "column", "label")
  list(
    files_added = unname(sn[setdiff(names(sn), names(so))]),
    files_removed = unname(so[setdiff(names(so), names(sn))]),
    drivers_added = unname(dn[setdiff(names(dn), names(do))]),
    drivers_removed = unname(do[setdiff(names(do), names(dn))])
  )
}

fpa_sync_delta_path <- function(config_path) {
  file.path(dirname(config_path), "work", "last-sync.json")
}

fpa_sync_read_delta <- function(config_path) {
  path <- fpa_sync_delta_path(config_path)
  if (!file.exists(path)) return(NULL)
  tryCatch(jsonlite::fromJSON(path, simplifyVector = TRUE), error = function(e) NULL)
}

#' Local and sovereign mode: no call to the server from Shiny or the engine.
fpa_sync_mode_path <- function(config_path) {
  file.path(dirname(config_path), "work", "sync-mode.json")
}

fpa_sync_mode <- function(config_path) {
  if (!nzchar(config_path %||% "")) return("connected")
  got <- tryCatch(jsonlite::fromJSON(fpa_sync_mode_path(config_path)), error = function(e) NULL)
  if (identical(as.character(got$mode %||% ""), "local")) "local" else "connected"
}

fpa_sync_set_mode <- function(config_path, mode) {
  mode <- if (identical(mode, "local")) "local" else "connected"
  path <- fpa_sync_mode_path(config_path)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(list(mode = mode, since = format(Sys.time(), "%Y-%m-%d %H:%M")), path, auto_unbox = TRUE, pretty = TRUE)
  invisible(mode)
}

#' Last contact with the server, per kind ("config" or "scripts").
fpa_sync_contact_path <- function(config_path) {
  file.path(dirname(config_path), "work", "sync-contact.json")
}

fpa_sync_contact <- function(config_path) {
  if (!nzchar(config_path %||% "")) return(list())
  got <- tryCatch(jsonlite::fromJSON(fpa_sync_contact_path(config_path), simplifyVector = FALSE), error = function(e) NULL)
  if (is.list(got)) got else list()
}

fpa_sync_note_contact <- function(config_path, kind, fields) {
  if (!nzchar(config_path %||% "")) return(invisible(NULL))
  all <- fpa_sync_contact(config_path)
  prev <- all[[kind]] %||% list()
  for (k in names(fields)) prev[[k]] <- fields[[k]]
  prev$at <- format(Sys.time(), "%Y-%m-%d %H:%M")
  all[[kind]] <- prev
  path <- fpa_sync_contact_path(config_path)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tryCatch(jsonlite::write_json(all, path, auto_unbox = TRUE, pretty = TRUE, null = "null"), error = function(e) NULL)
  invisible(all)
}

fpa_sync_file_version <- function(path) {
  if (!nzchar(path) || !file.exists(path)) return("")
  line <- tryCatch(readLines(path, n = 1L, warn = FALSE, encoding = "UTF-8"), error = function(e) "")
  hit <- regmatches(line, regexec("v[0-9]+\\.[0-9]+\\.[0-9]+", line))[[1]]
  if (length(hit)) hit[[1]] else ""
}

#' Arguments for one curl call. Each piece is quoted so Windows cmd does not
#' split on & or on a space. seconds is the hard stop.
fpa_sync_curl_args <- function(args, seconds = 8L) {
  seconds <- suppressWarnings(as.integer(seconds[[1]]))
  if (length(seconds) != 1L || is.na(seconds) || seconds < 2L) seconds <- 8L
  args <- as.character(unlist(args, use.names = FALSE))
  args <- args[!is.na(args) & nzchar(args)]
  connect <- min(4L, seconds)
  args <- c("-sS", "--connect-timeout", as.character(connect), "-m", as.character(seconds), args)
  vapply(args, function(a) shQuote(a, type = "cmd"), character(1))
}

fpa_sync_curl <- function(args, seconds = 8L) {
  seconds <- suppressWarnings(as.integer(seconds[[1]]))
  if (length(seconds) != 1L || is.na(seconds) || seconds < 2L) seconds <- 8L
  curl <- Sys.which("curl")
  if (!nzchar(curl)) curl <- "curl.exe"
  tryCatch(
    suppressWarnings(system2(
      curl, fpa_sync_curl_args(args, seconds),
      stdout = TRUE, stderr = FALSE, timeout = seconds + 1
    )),
    error = function(e) "000"
  )
}

#' Tell the site which R versions this folder is running. No print of the key.
#' Skipped in local mode. A failure leaves the pull alone.
fpa_sync_report_scripts <- function(home, config_path = "") {
  if (isTRUE(getOption("fpa.offline"))) return(invisible(FALSE))
  guess <- fpa_sync_guess(home, config_path)
  if (!nzchar(guess) || identical(fpa_sync_mode(guess), "local")) return(invisible(FALSE))
  link <- fpa_sync_link(fpa_sync_read(guess))
  if (is.null(link)) return(invisible(FALSE))
  engine <- fpa_sync_file_version(file.path(home, "fpa-engine.R"))
  screen <- fpa_sync_file_version(file.path(home, "app.R"))
  if (!nzchar(engine) && !nzchar(screen)) return(invisible(FALSE))
  tmp <- normalizePath(tempfile(fileext = ".json"), winslash = "/", mustWork = FALSE)
  out <- normalizePath(tempfile(fileext = ".txt"), winslash = "/", mustWork = FALSE)
  errf <- normalizePath(tempfile(fileext = ".txt"), winslash = "/", mustWork = FALSE)
  file.create(c(out, errf))
  on.exit(unlink(c(tmp, out, errf)), add = TRUE)
  jsonlite::write_json(
    list(project = link$id, key = link$key, engine = engine, screen = screen),
    tmp, auto_unbox = TRUE
  )
  code <- fpa_sync_curl(
    c("-o", out, "-w", "%{http_code}",
      "-H", "Content-Type: application/json", "--data-binary", paste0("@", tmp),
      paste0(link$site, "/api/script-report")),
    seconds = 4L
  )
  status <- as.character(code[[1]] %||% "")
  hit <- regmatches(status, regexec("([0-9]{3})$", status))[[1]]
  invisible(length(hit) >= 2L && identical(hit[[2]], "200"))
}

#' A run is worth reporting only when the statements came out clean.
#' Empty, unbalanced, or still carrying an error: do not post.
fpa_sync_run_ok <- function(res) {
  if (is.null(res) || !is.list(res)) return(FALSE)
  pnl <- res$pnl
  if (is.null(pnl) || !is.data.frame(pnl) || !nrow(pnl)) return(FALSE)
  if (!isTRUE(res$identity$ok)) return(FALSE)
  findings <- res$findings
  if (is.data.frame(findings) && nrow(findings) && any(findings$severity == "error")) return(FALSE)
  TRUE
}

#' Tell the site that one real run finished. No amounts, no paths, no print of the key.
#' Posted once from fpa_main, fpa_shiny_sync and the close re-run. Not from fpa_run.
#' Skipped offline, in local mode, with no project link, or when the run is not clean.
fpa_sync_report_run <- function(home, config_path = "", res = NULL) {
  if (!fpa_sync_run_ok(res)) return(invisible(FALSE))
  if (isTRUE(getOption("fpa.offline"))) return(invisible(FALSE))
  guess <- fpa_sync_guess(home, config_path)
  if (!nzchar(guess) || identical(fpa_sync_mode(guess), "local")) return(invisible(FALSE))
  link <- fpa_sync_link(fpa_sync_read(guess))
  if (is.null(link)) return(invisible(FALSE))
  engine <- fpa_sync_file_version(file.path(home, "fpa-engine.R"))
  if (!nzchar(engine)) return(invisible(FALSE))
  screen <- fpa_sync_file_version(file.path(home, "app.R"))
  tmp <- normalizePath(tempfile(fileext = ".json"), winslash = "/", mustWork = FALSE)
  out <- normalizePath(tempfile(fileext = ".txt"), winslash = "/", mustWork = FALSE)
  errf <- normalizePath(tempfile(fileext = ".txt"), winslash = "/", mustWork = FALSE)
  file.create(c(out, errf))
  on.exit(unlink(c(tmp, out, errf)), add = TRUE)
  body <- list(
    project = link$id, key = link$key, revision = link$rev,
    engine = engine, ok = TRUE
  )
  if (nzchar(screen)) body$screen <- screen
  jsonlite::write_json(body, tmp, auto_unbox = TRUE)
  n <- getOption("fpa.run_report_n", NULL)
  if (!is.null(n)) options(fpa.run_report_n = as.integer(n) + 1L)
  code <- fpa_sync_curl(
    c("-o", out, "-w", "%{http_code}",
      "-H", "Content-Type: application/json", "--data-binary", paste0("@", tmp),
      paste0(link$site, "/api/run-report")),
    seconds = 4L
  )
  status <- as.character(code[[1]] %||% "")
  hit <- regmatches(status, regexec("([0-9]{3})$", status))[[1]]
  invisible(length(hit) >= 2L && identical(hit[[2]], "200"))
}

#' Current script versions published by the site. No key is sent.
fpa_sync_server_scripts <- function(site) {
  site <- sub("/+$", "", as.character(site %||% ""))
  if (!grepl("^https?://", site)) return(NULL)
  tmp <- tempfile(fileext = ".json")
  errf <- tempfile()
  on.exit(unlink(c(tmp, errf)), add = TRUE)
  code <- fpa_sync_curl(
    c("-o", tmp, "-w", "%{http_code}", paste0(site, "/api/script-versions")),
    seconds = 6L
  )
  if (!identical(as.character(code[[1]] %||% ""), "200")) return(NULL)
  got <- tryCatch(jsonlite::fromJSON(tmp, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(got)) return(NULL)
  list(
    engine = as.character(got$engine %||% ""),
    screen = as.character(got$screen %||% ""),
    engine_file = as.character(got$engine_file %||% "fpa-engine.R"),
    screen_file = as.character(got$screen_file %||% "app.R")
  )
}

#' Returns list(ok, status, revision, changed, message, stop, delta).
#' status: local | offline | nolink | unchanged | pulled | refused | missing | noconfig
fpa_sync_pull <- function(home = getwd(), config_path = "") {
  quiet <- function(status, message = "", stop = FALSE) {
    list(ok = status %in% c("local", "offline", "nolink", "unchanged", "pulled"), status = status,
         revision = 0L, changed = FALSE, message = message, stop = stop)
  }
  if (isTRUE(getOption("fpa.offline"))) return(quiet("offline"))
  guess <- fpa_sync_guess(home, config_path)
  if (nzchar(guess) && identical(fpa_sync_mode(guess), "local")) return(quiet("local"))
  plain <- quiet
  quiet <- function(status, message = "", stop = FALSE) {
    if (nzchar(guess) && status %in% c("offline", "refused", "missing")) {
      fpa_sync_note_contact(guess, "config", list(status = status))
    }
    plain(status, message, stop)
  }
  cfg <- fpa_sync_read(guess)
  link <- fpa_sync_link(cfg)
  if (is.null(link)) {
    if (!nzchar(guess)) {
      return(quiet("noconfig", fpa_sync_msg("sync_pull_noconfig"), stop = TRUE))
    }
    return(quiet("nolink"))
  }
  script <- fpa_sync_script_version(home)
  addr <- paste0(
    link$site, "/api/sync/pull?project=", utils::URLencode(link$id, reserved = TRUE),
    "&key=", utils::URLencode(link$key, reserved = TRUE),
    "&rev=", link$rev,
    if (nzchar(script)) paste0("&script=", utils::URLencode(script, reserved = TRUE)) else ""
  )
  tmp <- tempfile(fileext = ".json")
  errf <- tempfile()
  on.exit(unlink(c(tmp, errf)), add = TRUE)
  code <- fpa_sync_curl(c("-o", tmp, "-w", "%{http_code}", addr), seconds = 8L)
  status <- suppressWarnings(as.integer(code[[1]] %||% "0"))
  if (is.na(status) || status == 0L) {
    if (!nzchar(guess)) return(quiet("noconfig", fpa_sync_msg("sync_pull_noconfig"), stop = TRUE))
    return(quiet("offline", fpa_sync_msg("sync_pull_offline")))
  }
  if (status == 401L) {
    return(quiet("refused", fpa_sync_msg("sync_pull_refused"), stop = !nzchar(guess)))
  }
  if (status == 404L) {
    if (!nzchar(guess)) return(quiet("missing", fpa_sync_msg("sync_pull_missing"), stop = TRUE))
    return(quiet("missing", fpa_sync_msg("sync_pull_missing")))
  }
  if (status == 204L) {
    fpa_sync_report_scripts(home, guess)
    fpa_sync_note_contact(guess, "config", list(status = "unchanged", revision = link$rev))
    return(list(
      ok = TRUE, status = "unchanged", revision = link$rev, changed = FALSE,
      message = fpa_sync_msg("sync_pull_same", link$rev), stop = FALSE
    ))
  }
  if (status != 200L) {
    if (!nzchar(guess)) return(quiet("offline", fpa_sync_msg("sync_pull_offline"), stop = TRUE))
    return(quiet("offline", fpa_sync_msg("sync_pull_offline")))
  }
  parsed <- tryCatch(jsonlite::fromJSON(tmp, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(parsed) || !identical(as.character(parsed$project$id %||% ""), link$id)) {
    return(quiet("refused", fpa_sync_msg("sync_pull_refused"), stop = !nzchar(guess)))
  }
  hint <- as.character((parsed$paths %||% list())$working_root_hint %||% "")
  targets <- character(0)
  if (nzchar(hint)) targets <- c(targets, file.path(hint, "fpa-config.json"))
  if (nzchar(guess)) targets <- c(targets, guess)
  if (!length(targets)) targets <- file.path(home, "fpa-config.json")
  targets <- unique(normalizePath(targets, winslash = "/", mustWork = FALSE))
  body <- readBin(tmp, "raw", n = file.info(tmp)$size)
  for (dest in targets) {
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    if (file.exists(dest)) {
      hist <- file.path(dirname(dest), "work", "config-history")
      dir.create(hist, recursive = TRUE, showWarnings = FALSE)
      bak <- file.path(hist, paste0("fpa-config-", format(Sys.time(), "%Y%m%d-%H%M"), ".json"))
      file.copy(dest, bak, overwrite = TRUE)
      file.remove(dest)
    }
    out <- tempfile(tmpdir = dirname(dest), fileext = ".json")
    con <- file(out, "wb")
    writeBin(body, con)
    close(con)
    file.rename(out, dest)
  }
  rev <- suppressWarnings(as.integer(parsed$project$sync_revision %||% 0))
  if (is.na(rev)) rev <- 0L
  delta <- fpa_sync_delta(cfg, parsed)
  record <- c(list(revision = rev, from_revision = link$rev, at = format(Sys.time(), "%Y-%m-%d %H:%M")), delta)
  fpa_sync_report_scripts(home, guess)
  for (dest in targets) {
    fpa_sync_note_contact(dest, "config", list(status = "pulled", revision = rev))
    rec <- fpa_sync_delta_path(dest)
    dir.create(dirname(rec), recursive = TRUE, showWarnings = FALSE)
    tryCatch(
      jsonlite::write_json(record, rec, auto_unbox = TRUE, pretty = TRUE),
      error = function(e) NULL
    )
  }
  list(
    ok = TRUE, status = "pulled", revision = rev, changed = TRUE,
    message = fpa_sync_msg("sync_pull_got", rev), stop = FALSE, delta = record
  )
}

#' Read a budget-year response. 200 keeps the revision. 409 means the account copy has no columns.
fpa_budget_post_read <- function(status, parsed = NULL) {
  status <- suppressWarnings(as.integer(status %||% 0L))
  if (length(status) != 1L || is.na(status)) status <- 0L
  if (status == 409L) {
    return(list(ok = FALSE, status = "empty", revision = NA_integer_, unchanged = FALSE))
  }
  if (status == 200L && is.list(parsed)) {
    rev <- suppressWarnings(as.integer(parsed$sync_revision %||% NA_integer_))
    if (length(rev) == 1L && !is.na(rev)) {
      return(list(ok = TRUE, status = "ok", revision = rev, unchanged = isTRUE(parsed$unchanged)))
    }
  }
  list(ok = FALSE, status = "fail", revision = NA_integer_, unchanged = FALSE)
}

#' POST /api/sync/budget-year with from "shiny". The key stays in the body file and is not printed.
#' Does not pull. A failed post leaves the local year where the caller wrote it.
fpa_sync_post_budget_year <- function(home = getwd(), config_path = "", year, source_id = "") {
  fail <- function(status) list(ok = FALSE, status = status, revision = NA_integer_, unchanged = FALSE)
  if (isTRUE(getOption("fpa.offline"))) return(fail("offline"))
  guess <- fpa_sync_guess(home, config_path)
  if (!nzchar(guess) || identical(fpa_sync_mode(guess), "local")) return(fail("local"))
  link <- fpa_sync_link(fpa_sync_read(guess))
  if (is.null(link)) return(fail("nolink"))
  year <- suppressWarnings(as.integer(year))
  if (length(year) != 1L || is.na(year) || year < 2000L || year > 2100L) return(fail("bad_year"))
  body <- list(project = link$id, key = link$key, year = year, from = "shiny")
  sid <- as.character(source_id %||% "")
  if (length(sid) == 1L && nzchar(sid)) body$source_id <- sid
  tmp <- normalizePath(tempfile(fileext = ".json"), winslash = "/", mustWork = FALSE)
  out <- normalizePath(tempfile(fileext = ".txt"), winslash = "/", mustWork = FALSE)
  errf <- normalizePath(tempfile(fileext = ".txt"), winslash = "/", mustWork = FALSE)
  file.create(c(out, errf))
  on.exit(unlink(c(tmp, out, errf)), add = TRUE)
  jsonlite::write_json(body, tmp, auto_unbox = TRUE)
  code <- fpa_sync_curl(
    c("-o", out, "-w", "%{http_code}",
      "-H", "Content-Type: application/json",
      "--data-binary", paste0("@", tmp),
      paste0(sub("/+$", "", link$site), "/api/sync/budget-year")),
    seconds = 8L
  )
  status <- as.character(code[[1]] %||% "")
  hit <- regmatches(status, regexec("([0-9]{3})$", status))[[1]]
  http <- if (length(hit) >= 2L) as.integer(hit[[2]]) else 0L
  parsed <- tryCatch(jsonlite::fromJSON(out, simplifyVector = FALSE), error = function(e) NULL)
  fpa_budget_post_read(http, parsed)
}
