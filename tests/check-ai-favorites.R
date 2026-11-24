# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Favorites pool, named sets, and batch queue order.
`%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a
root <- Sys.getenv("FPA_LOCAL_DIR", ".")
source(file.path(root, "R", "fpa-ai.R"), encoding = "UTF-8")
source(file.path(root, "R", "fpa-ai-prompts.R"), encoding = "UTF-8")

fail <- 0L
say <- function(ok, msg) {
  if (!ok) fail <<- fail + 1L
  cat(if (ok) "ok   " else "FAIL ", msg, "\n", sep = "")
}

box <- tempfile("fpa-ai-fav-")
dir.create(box)
cfg <- file.path(box, "fpa-config.json")
writeLines("{}", cfg)
dir.create(file.path(box, "work"), showWarnings = FALSE)

say(identical(fpa_ai_fav_read(cfg)$favorites, character()), "empty favorites")
say(!length(fpa_ai_fav_read(cfg)$sets), "empty sets")

say(isTRUE(fpa_ai_fav_toggle(cfg, "var_why")), "toggle on")
say(isTRUE(fpa_ai_fav_is(cfg, "var_why")), "is favorite")
say(!isTRUE(fpa_ai_fav_toggle(cfg, "var_why")), "toggle off")
say(!isTRUE(fpa_ai_fav_is(cfg, "var_why")), "not favorite")
fpa_ai_fav_toggle(cfg, "var_why")
fpa_ai_fav_toggle(cfg, "cash_end")
fpa_ai_fav_toggle(cfg, "dir_three")
say(length(fpa_ai_fav_read(cfg)$favorites) == 3L, "three favorites")

set <- fpa_ai_set_add(cfg, "Revue mensuelle")
say(!is.null(set) && identical(set$name, "Revue mensuelle"), "create set")
say(isTRUE(fpa_ai_set_item_add(cfg, set$id, "dir_three")), "add first")
say(isTRUE(fpa_ai_set_item_add(cfg, set$id, "var_why")), "add second")
say(isTRUE(fpa_ai_set_item_add(cfg, set$id, "cash_end")), "add third")
say(!isTRUE(fpa_ai_set_item_add(cfg, set$id, "var_why")), "refuse duplicate")
got <- fpa_ai_set_get(cfg, set$id)
say(identical(as.character(unlist(got$items)), c("dir_three", "var_why", "cash_end")), "order after add")

say(!isTRUE(fpa_ai_set_move(cfg, set$id, 1L, "up")), "first up is no-op")
say(!isTRUE(fpa_ai_set_move(cfg, set$id, 3L, "down")), "last down is no-op")
say(isTRUE(fpa_ai_set_move(cfg, set$id, 2L, "up")), "middle up")
got <- fpa_ai_set_get(cfg, set$id)
say(identical(as.character(unlist(got$items)), c("var_why", "dir_three", "cash_end")), "order after move")
say(isTRUE(fpa_ai_set_move(cfg, set$id, 1L, "down")), "first down")
got <- fpa_ai_set_get(cfg, set$id)
say(identical(as.character(unlist(got$items)), c("dir_three", "var_why", "cash_end")), "order restored")
say(!isTRUE(fpa_ai_set_move(cfg, set$id, 1L, "top")), "first top is no-op")
say(!isTRUE(fpa_ai_set_move(cfg, set$id, 3L, "bottom")), "last bottom is no-op")
say(isTRUE(fpa_ai_set_move(cfg, set$id, 3L, "top")), "send last to top")
got <- fpa_ai_set_get(cfg, set$id)
say(identical(as.character(unlist(got$items)), c("cash_end", "dir_three", "var_why")), "order after top")
say(isTRUE(fpa_ai_set_move(cfg, set$id, 1L, "bottom")), "send first to bottom")
got <- fpa_ai_set_get(cfg, set$id)
say(identical(as.character(unlist(got$items)), c("dir_three", "var_why", "cash_end")), "order after bottom")
say(isTRUE(fpa_ai_set_move(cfg, set$id, 1L, "to", 3L)), "drag first onto third")
got <- fpa_ai_set_get(cfg, set$id)
say(identical(as.character(unlist(got$items)), c("var_why", "cash_end", "dir_three")), "order after drag")
fpa_ai_set_move(cfg, set$id, 3L, "top")
got <- fpa_ai_set_get(cfg, set$id)
say(identical(as.character(unlist(got$items)), c("dir_three", "var_why", "cash_end")), "order restored after drag")

all_set <- fpa_ai_set_add(cfg, "Tous les favoris")
n_add <- fpa_ai_set_items_add_favs(cfg, all_set$id)
say(identical(n_add, 3L), paste("add all favorites", n_add))
say(identical(fpa_ai_set_items_add_favs(cfg, all_set$id), 0L), "add all is idempotent")
fav_order <- as.character(fpa_ai_fav_read(cfg)$favorites)
say(identical(as.character(unlist(fpa_ai_set_get(cfg, all_set$id)$items)), fav_order), "favorite order kept")

say(isTRUE(fpa_ai_set_rename(cfg, set$id, "Revue du mois")), "rename")
say(identical(fpa_ai_set_get(cfg, set$id)$name, "Revue du mois"), "renamed name")

user <- fpa_ai_user_prompt_add(cfg, "Controle", "Dis le chiffre a surveiller.")
fpa_ai_fav_toggle(cfg, user$id)
fpa_ai_set_item_add(cfg, set$id, user$id)
say(user$id %in% fpa_ai_fav_read(cfg)$favorites, "user prompt is favorite")
say(user$id %in% as.character(unlist(fpa_ai_set_get(cfg, set$id)$items)), "user prompt in set")
fpa_ai_user_prompt_drop(cfg, user$id)
say(!(user$id %in% fpa_ai_fav_read(cfg)$favorites), "drop cleans favorites")
say(!(user$id %in% as.character(unlist(fpa_ai_set_get(cfg, set$id)$items))), "drop cleans sets")

cat_q <- fpa_ai_batch_queue(fpa_ai_set_get(cfg, set$id), fpa_ai_catalog(), list(), "fr")
say(length(cat_q) == 3L, paste("queue length", length(cat_q)))
say(identical(vapply(cat_q, function(x) x$id, ""), c("dir_three", "var_why", "cash_end")), "queue order")
say(nzchar(cat_q[[1]]$recipe) && nzchar(cat_q[[1]]$title), "queue has recipe and title")
say(!is.null(cat_q[[1]]$slice), "catalog slice kept")

empty_set <- fpa_ai_set_add(cfg, "Vide")
say(!length(fpa_ai_batch_queue(empty_set, fpa_ai_catalog(), list(), "fr")), "empty set queue")
bad <- list(id = "sx", name = "Bad", items = list("dir_three", "no_such_prompt", "cash_end"))
q2 <- fpa_ai_batch_queue(bad, fpa_ai_catalog(), list(), "fr")
say(length(q2) == 2L && identical(vapply(q2, function(x) x$id, ""), c("dir_three", "cash_end")), "unknown id skipped")

say(isTRUE(fpa_ai_set_item_remove(cfg, set$id, 2L)), "remove middle")
got <- fpa_ai_set_get(cfg, set$id)
say(identical(as.character(unlist(got$items)), c("dir_three", "cash_end")), "after remove")
say(isTRUE(fpa_ai_set_drop(cfg, set$id)), "drop set")
say(is.null(fpa_ai_set_get(cfg, set$id)), "set gone")

unlink(box, recursive = TRUE)
cat("FAILS ", fail, "\n", sep = "")
quit(status = if (fail) 1 else 0)
