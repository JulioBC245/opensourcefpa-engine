# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Paramètres card: what a sync brought in, and script versions against the server.
# Run from fpa-local: Rscript tests/check-params-card.R

`%||%` <- function(a, b) if (is.null(a)) b else a
root <- if (file.exists("app.R")) "." else ".."
source(file.path(root, "R", "fpa-copy.R"), encoding = "UTF-8")
source(file.path(root, "R", "fpa-sync.R"), encoding = "UTF-8")

wanted <- c("fpa_shiny_script_pick", "fpa_shiny_ver_newer", "fpa_shiny_script_rows", "fpa_shiny_delta_line",
            "fpa_shiny_delta_body", "fpa_shiny_ago", "fpa_shiny_project_title")
for (e in parse(file.path(root, "app.R"), encoding = "UTF-8")) {
  if (is.call(e) && identical(as.character(e[[1]]), "<-") && as.character(e[[2]]) %in% wanted) eval(e, globalenv())
}

pass <- 0L
fail <- 0L
check <- function(label, ok) {
  if (isTRUE(ok)) { pass <<- pass + 1L; cat("PASS", label, "\n") } else { fail <<- fail + 1L; cat("FAIL", label, "\n") }
}

src <- function(id, name) list(source_id = id, source_name = name)
drv <- function(col, label) list(column = col, label = label)
old <- list(sources = list(src("a", "pnl.xlsx"), src("b", "budget.xlsx")),
            driver_bindings = list(drv("tarif", "tarif"), drv("effectif", "effectif")))
new <- list(sources = list(src("a", "pnl.xlsx"), src("b", "budget.xlsx"), src("c", "meteo.csv")),
            driver_bindings = list(drv("tarif", "tarif"), drv("effectif", "effectif"), drv("pluie", "pluie mm")))

d <- fpa_sync_delta(old, new)
check("one file added, named", identical(d$files_added, "meteo.csv") && !length(d$files_removed))
check("one driver added, named", identical(d$drivers_added, "pluie mm") && !length(d$drivers_removed))

back <- fpa_sync_delta(new, old)
check("reverse pull lists removals", identical(back$files_removed, "meteo.csv") && identical(back$drivers_removed, "pluie mm"))

same <- fpa_sync_delta(new, new)
check("same config, nothing listed", !length(unlist(same)))
line_same <- fpa_shiny_delta_line(c(list(revision = 3L, at = "2026-09-29 15:00"), same))
check("same config line says settings only", grepl("sans nouveau fichier", line_same))

line_new <- fpa_shiny_delta_line(c(list(revision = 3L, at = "2026-09-29 15:00"), d))
check("delta line counts and names", grepl("1 fichier\\(s\\) ajouté\\(s\\) : meteo.csv", line_new) &&
  grepl("1 driver\\(s\\) ajouté\\(s\\) : pluie mm", line_new) && grepl("révision 3", line_new))

rt <- tempfile("rec"); dir.create(file.path(rt, "work"), recursive = TRUE)
jsonlite::write_json(c(list(revision = 3L, at = "x"), d), file.path(rt, "work", "last-sync.json"), auto_unbox = TRUE)
check("record survives a write and read", identical(fpa_shiny_delta_line(fpa_sync_read_delta(file.path(rt, "fpa-config.json"))),
  fpa_shiny_delta_line(c(list(revision = 3L, at = "x"), d))))

home <- tempfile("home"); dir.create(home)
put <- function(name, ver) writeLines(paste0("# ", name, " · ", ver, " · 2026-09-29 14:15 · note"), file.path(home, name))
put("fpa-engine.R", "v0.2.0"); put("app.R", "v0.2.0")
srv <- function(e, s) list(engine = e, screen = s, engine_file = "fpa-engine.R", screen_file = "app.R")

r <- fpa_shiny_script_rows(home, srv("v0.2.0", "v0.2.0"))
check("same versions: no gap, no advice", !any(vapply(r$lines, `[[`, TRUE, "gap")) && !length(r$stale))

r <- fpa_shiny_script_rows(home, srv("v0.3.0", "v0.2.0"))
check("newer engine on server: gap names both versions", r$lines[[1]]$gap &&
  grepl("vous avez v0.2.0, le serveur a v0.3.0", r$lines[[1]]$text) && identical(r$stale, "fpa-engine.R"))
advice <- fpa_txt("params_script_advice", paste(r$stale, collapse = " et "))
check("advice points to the configuration and names the file", grepl("Ouvrez la configuration", advice) && grepl("fpa-engine.R", advice))

r <- fpa_shiny_script_rows(home, srv("v0.2.0", "v0.10.0"))
check("v0.10.0 counts as newer than v0.2.0", identical(r$stale, "app.R"))

put("app.R", "v0.4.0")
r <- fpa_shiny_script_rows(home, srv("v0.2.0", "v0.3.0"))
check("local newer than server: gap shown, no replace advice", r$lines[[2]]$gap && !length(r$stale))

unlink(file.path(home, "app.R"))
r <- fpa_shiny_script_rows(home, srv("v0.2.0", "v0.2.0"))
check("app.R missing: says absent, advises download", grepl("absent", r$lines[[2]]$text) && identical(r$stale, "app.R"))

unlink(file.path(home, "fpa-engine.R")); put("fpa-moteur.R", "v0.2.0"); put("app.R", "v0.2.0")
r <- fpa_shiny_script_rows(home, srv("v0.2.0", "v0.2.0"))
check("old fpa-moteur.R: advises fpa-engine.R", grepl("fpa-moteur.R", r$lines[[1]]$text) && identical(r$stale, "fpa-engine.R"))

r <- fpa_shiny_script_rows(home, NULL)
check("server not reached: says so, no advice", grepl("serveur non joint", r$lines[[1]]$text) && !length(r$stale))

states <- function(r) vapply(r$rows, `[[`, "", "state")
unlink(file.path(home, "fpa-moteur.R")); put("fpa-engine.R", "v0.2.0"); put("app.R", "v0.2.0")
check("states: both up to date -> ok, ok", identical(states(fpa_shiny_script_rows(home, srv("v0.2.0", "v0.2.0"))), c("ok", "ok")))
check("states: engine behind -> old, ok", identical(states(fpa_shiny_script_rows(home, srv("v0.3.0", "v0.2.0"))), c("old", "ok")))
check("states: screen ahead -> ok, ahead", identical(states(fpa_shiny_script_rows(home, srv("v0.2.0", "v0.1.0"))), c("ok", "ahead")))
check("states: never checked -> unchecked, unchecked", identical(states(fpa_shiny_script_rows(home, NULL)), c("unchecked", "unchecked")))

now <- as.POSIXct("2026-09-29 17:30", tz = Sys.timezone())
check("ago: same day says today", identical(fpa_shiny_ago("2026-09-29 15:02", now), "aujourd'hui à 15:02"))
check("ago: day before says yesterday", identical(fpa_shiny_ago("2026-09-28 09:10", now), "hier à 09:10"))
check("ago: counts days", identical(fpa_shiny_ago("2026-09-25 11:00", now), "il y a 4 jours (2026-09-25)"))
check("ago: never is empty", identical(fpa_shiny_ago("", now), ""))

box <- tempfile("mode"); dir.create(box)
cfg_path <- file.path(box, "fpa-config.json")
jsonlite::write_json(list(
  project = list(id = "p1", key = "k-secret-123", site = "http://127.0.0.1:9", sync_revision = 3L),
  paths = list(working_root_hint = box)
), cfg_path, auto_unbox = TRUE)
before <- readLines(cfg_path, warn = FALSE)
check("mode: a new folder is connected", identical(fpa_sync_mode(cfg_path), "connected"))
fpa_sync_set_mode(cfg_path, "local")
check("mode: switch to local is saved", identical(fpa_sync_mode(cfg_path), "local"))
t0 <- Sys.time()
out <- capture.output(res <- fpa_sync_pull(home = box, config_path = cfg_path))
check("local: pull stops before any call, file untouched", identical(res$status, "local") && isTRUE(res$ok) &&
  as.numeric(difftime(Sys.time(), t0, units = "secs")) < 1 && identical(readLines(cfg_path, warn = FALSE), before))
check("local: no contact is logged", is.null(fpa_sync_contact(cfg_path)$config))
t1 <- Sys.time()
reported <- fpa_sync_report_scripts(box, cfg_path)
check("local: versions are not reported", !isTRUE(reported) && as.numeric(difftime(Sys.time(), t1, units = "secs")) < 1)
fpa_sync_set_mode(cfg_path, "connected")
warned <- character(0)
out <- capture.output(res <- withCallingHandlers(
  fpa_sync_pull(home = box, config_path = cfg_path),
  warning = function(w) { warned <<- c(warned, conditionMessage(w)); invokeRestart("muffleWarning") },
  message = function(m) { warned <<- c(warned, conditionMessage(m)); invokeRestart("muffleMessage") }
), type = "output")
out <- c(out, warned)
contact <- fpa_sync_contact(cfg_path)$config
check("connected, server down: offline, logged with a time", identical(res$status, "offline") &&
  identical(contact$status, "offline") && nzchar(contact$at %||% ""))
check("connected, server down: key never printed, not even in a warning", !any(grepl("k-secret-123", out, fixed = TRUE)) &&
  !grepl("k-secret-123", paste(readLines(fpa_sync_contact_path(cfg_path), warn = FALSE), collapse = ""), fixed = TRUE))
fpa_sync_note_contact(cfg_path, "scripts", list(engine = "v0.2.0", screen = "v0.2.0", status = "ok"))
both <- fpa_sync_contact(cfg_path)
check("contact keeps settings and scripts apart", identical(both$config$status, "offline") && identical(both$scripts$engine, "v0.2.0"))
check("delta body names what came in", grepl("meteo.csv", fpa_shiny_delta_body(d)) && !grepl("révision", fpa_shiny_delta_body(d)))

titled <- fpa_shiny_project_title(list(
  project = list(name = "hotel provence · sept. 2026"),
  company = list(name = "hotel provence", industry_tag = "hotellerie")
))
check("project name is the site name, not the company", identical(titled$name, "hotel provence · sept. 2026"))
check("company stays on its own field", identical(titled$company, "hotel provence"))
check("company line does not repeat the project name",
  identical(fpa_txt("pm_company_line", titled$company), "Société : hotel provence") &&
  !grepl("sept", fpa_txt("pm_company_line", titled$company), fixed = TRUE))
blank <- fpa_shiny_project_title(list(company = list(name = "hotel provence")))
check("missing project name falls back to the company", identical(blank$name, "hotel provence"))

cat(sprintf("\n%d passed, %d failed\n", pass, fail))
if (fail > 0L) quit(status = 1L)
