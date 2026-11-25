# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Pull checks. Uses the hotel project already published on localhost:3001.
# A temp copy is used whenever a write could leave the working folder.
root <- "."
source(file.path(root, "tests", "helpers.R"), encoding = "UTF-8")
source(file.path(root, "R/fpa-paths.R"), encoding = "UTF-8")
source(file.path(root, "R/fpa-config.R"), encoding = "UTF-8")
source(file.path(root, "R/fpa-sync.R"), encoding = "UTF-8")
source(file.path(root, "R/fpa-copy.R"), encoding = "UTF-8")

pass <- 0L
fail <- 0L
check <- function(ok, label) {
  if (isTRUE(ok)) {
    pass <<- pass + 1L
    cat("ok  ", label, "\n", sep = "")
  } else {
    fail <<- fail + 1L
    cat("FAIL ", label, "\n", sep = "")
  }
}

live <- fpa_optional_data_config()
if (is.null(live)) fpa_skip_if_no_optional_data("sync pull checks")
cfg <- jsonlite::fromJSON(live, simplifyVector = FALSE)
key <- as.character(cfg$project$key %||% "")
check(nzchar(key), "local file has a project key")

sandbox <- function(rev) {
  dir <- tempfile("fpa-sync-")
  dir.create(dir)
  dir.create(file.path(dir, "work"))
  body <- jsonlite::fromJSON(live, simplifyVector = FALSE)
  body$paths$working_root_hint <- normalizePath(dir, winslash = "/")
  body$project$sync_revision <- rev
  dest <- file.path(dir, "fpa-config.json")
  jsonlite::write_json(body, dest, auto_unbox = TRUE, pretty = TRUE)
  dest
}

# Offline keeps the file and does not need the server.
off <- sandbox(1L)
before <- readBin(off, "raw", file.info(off)$size)
options(fpa.offline = TRUE)
got <- fpa_sync_pull(home = dirname(off), config_path = off)
options(fpa.offline = FALSE)
after <- readBin(off, "raw", file.info(off)$size)
check(identical(got$status, "offline") && identical(before, after), "offline keeps the file")

# A wrong key must not rewrite the file. The published hint is replaced above, so a
# mistaken 200 would still land in the sandbox.
bad <- sandbox(0L)
raw <- paste(readLines(bad, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
raw <- sub(key, paste0(key, "xxxx"), raw, fixed = TRUE)
writeLines(raw, bad, useBytes = TRUE)
snap <- readBin(bad, "raw", file.info(bad)$size)
out <- capture.output(got <- fpa_sync_pull(home = dirname(bad), config_path = bad))
check(identical(got$status, "refused"), "bad key is refused")
check(!any(grepl(key, out, fixed = TRUE)), "the key is not printed")
check(identical(snap, readBin(bad, "raw", file.info(bad)$size)), "bad key leaves the sandbox file")

# Real pull into a sandbox whose hint is the sandbox. The server answer still names
# the real working folder, so this call also refreshes the live file (that is the product).
box <- sandbox(0L)
got <- fpa_sync_pull(home = dirname(box), config_path = box)
check(identical(got$status, "pulled") && isTRUE(got$changed) && got$revision >= 1L, "pull writes a new revision")
live_after <- jsonlite::fromJSON(live, simplifyVector = FALSE)
check(identical(as.character(live_after$project$key %||% ""), key), "live file still has the same key")
check(as.integer(live_after$project$sync_revision %||% 0) >= 1L, "live file records the revision")

again <- capture.output(got2 <- fpa_sync_pull(home = dirname(live), config_path = live))
check(identical(got2$status, "unchanged"), "second pull is already current")
check(!any(grepl(key, again, fixed = TRUE)), "second pull does not print the key")

cat("sync pull: ", pass, "/", pass + fail, " passed\n", sep = "")
if (fail) quit(status = 1)
