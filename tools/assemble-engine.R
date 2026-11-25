# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Assemble the subscriber fpa-engine.R (one readable file) from R/ modules.
# LAST CHANGE: 2026-09-30 — the module list is engine-modules.txt only.
#
# Usage, from the opensourcefpa-engine repository root:
#   Rscript tools/assemble-engine.R path/to/fpa-engine.R

root <- if (dir.exists("R")) {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
} else {
  stop("Run this from the repository root (the folder that contains R/).")
}
setwd(root)

manifest <- file.path(root, "engine-modules.txt")
if (!file.exists(manifest)) stop("Liste des modules introuvable: ", manifest)
man_lines <- readLines(manifest, warn = FALSE, encoding = "UTF-8")
mods <- trimws(sub("#.*$", "", man_lines))
mods <- mods[nzchar(mods)]
if (!length(mods)) stop("engine-modules.txt est vide.")

banner <- c(
  "# fpa-engine.R — Moteur FPA local (fichier unique, texte lisible)",
  "# Ouvrez ce fichier dans le Bloc-notes: ce n'est pas un binaire.",
  "#",
  "# 1. Installez R depuis https://cran.r-project.org/bin/windows/base/",
  "# 2. Une fois: install.packages(\"jsonlite\")",
  "# 3. Placez ce fichier a cote de fpa-config.json",
  "# 4. Dans RStudio: Source. Ou: Rscript fpa-engine.R",
  "#",
  "# Les blocs ci-dessous sont le moteur, dans l'ordre de chargement.",
  ""
)

read_utf8 <- function(path) {
  raw <- readBin(path, "raw", n = file.info(path)$size)
  if (length(raw) >= 3 && identical(as.integer(raw[1:3]), c(239L, 187L, 191L))) {
    raw <- raw[-seq_len(3)]
  }
  txt <- rawToChar(raw)
  Encoding(txt) <- "UTF-8"
  txt
}

blocks <- vapply(mods, function(name) {
  path <- file.path(root, "R", name)
  if (!file.exists(path)) stop("Module manquant: ", path)
  body <- gsub("\\s+$", "", read_utf8(path))
  message("  ", name, " ", nchar(body), " c")
  paste0("# === ", name, " ===\n", body, "\n")
}, character(1))

entry <- read_utf8(file.path(root, "fpa-engine.R"))
entry_lines <- strsplit(entry, "\n", fixed = TRUE)[[1]]
keep <- vapply(entry_lines, function(line) {
  t <- trimws(line)
  if (startsWith(t, "source(file.path(FPA_HOME")) return(FALSE)
  if (startsWith(t, "fpa_source_all(")) return(FALSE)
  TRUE
}, logical(1))
entry_stripped <- paste(entry_lines[keep], collapse = "\n")

header <- strsplit(entry, "\n", fixed = TRUE)[[1]][[1]]
out <- paste(
  c(
    header,
    paste(banner, collapse = "\n"),
    blocks,
    paste0("# === fpa-engine.R ===\n", entry_stripped)
  ),
  collapse = "\n"
)
if (!grepl("\n$", out)) out <- paste0(out, "\n")

dest <- commandArgs(trailingOnly = TRUE)
if (!length(dest) || !nzchar(dest[[1]])) {
  stop("Pass the output path: Rscript tools/assemble-engine.R path/to/fpa-engine.R")
} else {
  dest <- dest[[1]]
}
dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
# writeLines corrupts UTF-8 on some Windows locales; writeBytes instead
con <- file(dest, "wb")
writeBin(charToRaw(enc2utf8(out)), con)
close(con)
written <- rawToChar(readBin(dest, "raw", n = file.info(dest)$size))
Encoding(written) <- "UTF-8"
missing <- mods[!vapply(mods, function(name) {
  grepl(paste0("# === ", name, " ==="), written, fixed = TRUE)
}, logical(1))]
if (length(missing)) {
  stop("Assemble a omis: ", paste(missing, collapse = ", "))
}
message("Wrote ", dest, " (", length(mods), " modules)")
