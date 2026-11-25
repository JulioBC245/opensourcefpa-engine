# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Optional analyst. Off unless the user turns it on.
# The key stays in the Windows user profile, never in fpa-config.json.

fpa_ai_providers <- function() {
  list(
    openai = list(
      id = "openai", label = "OpenAI", style = "openai",
      base = "https://api.openai.com/v1/chat/completions",
      model = "gpt-4o-mini",
      keys = "https://platform.openai.com/api-keys",
      credits = "https://platform.openai.com/settings/organization/billing/overview"
    ),
    anthropic = list(
      id = "anthropic", label = "Anthropic", style = "anthropic",
      base = "https://api.anthropic.com/v1/messages",
      model = "claude-haiku-4-5",
      keys = "https://platform.claude.com/settings/keys",
      credits = "https://platform.claude.com/settings/billing"
    ),
    gemini = list(
      id = "gemini", label = "Gemini", style = "openai",
      base = "https://generativelanguage.googleapis.com/v1beta/openai/chat/completions",
      model = "gemini-2.5-flash",
      keys = "https://aistudio.google.com/apikey",
      credits = "https://aistudio.google.com/apikey"
    ),
    mistral = list(
      id = "mistral", label = "Mistral", style = "openai",
      base = "https://api.mistral.ai/v1/chat/completions",
      model = "mistral-small-latest",
      keys = "https://admin.mistral.ai",
      credits = "https://admin.mistral.ai"
    ),
    deepseek = list(
      id = "deepseek", label = "DeepSeek", style = "openai",
      base = "https://api.deepseek.com/chat/completions",
      model = "deepseek-chat",
      keys = "https://platform.deepseek.com/api_keys",
      credits = "https://platform.deepseek.com/top_up"
    ),
    other = list(
      id = "other", label = "Other", style = "openai",
      base = "https://openrouter.ai/api/v1/chat/completions",
      model = "openai/gpt-4o-mini",
      keys = "https://openrouter.ai/settings/keys",
      credits = "https://openrouter.ai/settings/credits"
    )
  )
}

fpa_ai_provider <- function(id) {
  all <- fpa_ai_providers()
  hit <- all[[as.character(id %||% "")]]
  if (is.null(hit)) all$openai else hit
}

fpa_ai_key_path <- function() {
  override <- getOption("fpa.ai.key_path", "")
  if (nzchar(override %||% "")) return(as.character(override))
  new_path <- file.path(tools::R_user_dir("opensourcefpa-engine", "config"), "ai-key.json")
  if (file.exists(new_path)) return(new_path)
  old_path <- file.path(tools::R_user_dir("fpa-local", "config"), "ai-key.json")
  if (file.exists(old_path)) return(old_path)
  new_path
}

fpa_ai_settings_path <- function(config_path) {
  if (!nzchar(config_path %||% "")) return("")
  file.path(dirname(config_path), "work", "ai-settings.json")
}

fpa_ai_settings_default <- function() {
  p <- fpa_ai_provider("openai")
  list(
    enabled = FALSE, provider = "openai", base_url = "", model = p$model,
    mode = "sans", open_report = "", preview = TRUE
  )
}

fpa_ai_mode_norm <- function(x) {
  x <- tolower(trimws(as.character(if (!length(x)) "" else x[[1]])))
  if (identical(x, "avec")) "avec" else "sans"
}

fpa_ai_settings_read <- function(config_path) {
  out <- fpa_ai_settings_default()
  path <- fpa_ai_settings_path(config_path)
  if (!nzchar(path) || !file.exists(path)) return(out)
  got <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.list(got)) return(out)
  out$enabled <- isTRUE(got$enabled)
  if (nzchar(got$provider %||% "")) out$provider <- as.character(got$provider)
  out$base_url <- as.character(got$base_url %||% "")
  if (nzchar(got$model %||% "")) out$model <- as.character(got$model)
  out$mode <- fpa_ai_mode_norm(got$mode)
  out$open_report <- as.character(got$open_report %||% "")
  out$preview <- if (is.null(got$preview)) TRUE else isTRUE(got$preview)
  out
}

fpa_ai_settings_write <- function(config_path, settings) {
  path <- fpa_ai_settings_path(config_path)
  if (!nzchar(path)) return(FALSE)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  body <- list(
    enabled = isTRUE(settings$enabled),
    provider = as.character(settings$provider %||% "openai"),
    base_url = as.character(settings$base_url %||% ""),
    model = as.character(settings$model %||% ""),
    mode = fpa_ai_mode_norm(settings$mode),
    open_report = as.character(settings$open_report %||% ""),
    preview = if (is.null(settings$preview)) TRUE else isTRUE(settings$preview)
  )
  ok <- tryCatch({
    jsonlite::write_json(body, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
    TRUE
  }, error = function(e) FALSE)
  invisible(isTRUE(ok))
}

fpa_ai_key_read <- function() {
  path <- fpa_ai_key_path()
  if (!file.exists(path)) return("")
  got <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.list(got)) return("")
  if (identical(as.character(got$store %||% ""), "dpapi") && nzchar(got$blob %||% "")) {
    return(fpa_ai_dpapi_open(got$blob))
  }
  key <- as.character(got$key %||% "")
  if (!nzchar(key) || is.na(key)) "" else key
}

fpa_ai_dpapi_seal <- function(text) {
  text <- as.character(text %||% "")
  if (!nzchar(text)) return("")
  b64 <- jsonlite::base64_enc(charToRaw(enc2utf8(text)))
  cmd <- sprintf(
    "$b=[Convert]::FromBase64String('%s'); $s=[Text.Encoding]::UTF8.GetString($b); $sec=ConvertTo-SecureString -String $s -AsPlainText -Force; ConvertFrom-SecureString $sec",
    b64
  )
  out <- tryCatch(system2("powershell", c("-NoProfile", "-Command", cmd), stdout = TRUE, stderr = FALSE), error = function(e) "")
  blob <- trimws(paste(out, collapse = ""))
  if (!nzchar(blob) || !grepl("^[0-9a-fA-F-]+$", blob)) "" else blob
}

fpa_ai_dpapi_open <- function(blob) {
  blob <- trimws(as.character(blob %||% ""))
  if (!nzchar(blob)) return("")
  cmd <- sprintf(
    "$sec=ConvertTo-SecureString -String '%s'; $p=[Runtime.InteropServices.Marshal]::SecureStringToBSTR($sec); try { [Runtime.InteropServices.Marshal]::PtrToStringBSTR($p) } finally { [Runtime.InteropServices.Marshal]::ZeroFreeBSTR($p) }",
    gsub("'", "''", blob)
  )
  out <- tryCatch(system2("powershell", c("-NoProfile", "-Command", cmd), stdout = TRUE, stderr = FALSE), error = function(e) "")
  key <- paste(out, collapse = "")
  if (!nzchar(key)) "" else key
}

fpa_ai_key_write <- function(key) {
  key <- trimws(as.character(key %||% ""))
  path <- fpa_ai_key_path()
  if (!nzchar(key)) {
    if (file.exists(path)) unlink(path)
    return(invisible(TRUE))
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  blob <- fpa_ai_dpapi_seal(key)
  body <- if (nzchar(blob)) list(store = "dpapi", blob = blob) else list(key = key)
  ok <- tryCatch({
    jsonlite::write_json(body, path, auto_unbox = TRUE)
    TRUE
  }, error = function(e) FALSE)
  invisible(isTRUE(ok))
}

fpa_ai_key_mask <- function(key = NULL) {
  if (is.null(key)) key <- fpa_ai_key_read()
  key <- as.character(key %||% "")
  if (!nzchar(key)) return("")
  paste0("···· ", substr(key, max(1L, nchar(key) - 3L), nchar(key)))
}

fpa_ai_endpoint <- function(settings) {
  p <- fpa_ai_provider(settings$provider)
  custom <- trimws(as.character(settings$base_url %||% ""))
  if (identical(p$id, "other") && nzchar(custom)) custom else p$base
}

fpa_ai_messages_body <- function(messages, model, style, max_tokens = 1200L) {
  max_tokens <- as.integer(max_tokens)
  if (identical(style, "anthropic")) {
    sys <- vapply(messages, function(m) if (identical(m$role, "system")) m$content else "", "")
    sys <- paste(sys[nzchar(sys)], collapse = "\n\n")
    user <- lapply(messages, function(m) {
      if (identical(m$role, "system")) NULL else list(role = m$role, content = m$content)
    })
    user <- user[!vapply(user, is.null, logical(1))]
    list(
      model = model, max_tokens = max_tokens, temperature = 0.2,
      system = sys, messages = user
    )
  } else {
    list(
      model = model, temperature = 0.2, max_tokens = max_tokens,
      messages = messages
    )
  }
}

fpa_ai_header_lines <- function(key, style) {
  lines <- "Content-Type: application/json"
  if (nzchar(key)) {
    if (identical(style, "anthropic")) {
      lines <- c(lines, paste0("x-api-key: ", key), "anthropic-version: 2023-06-01")
    } else {
      lines <- c(lines, paste0("Authorization: Bearer ", key))
    }
  }
  lines
}

#' Start a call. The key is only in a temp header file, not on the command line.
#' Returns a job list, or a finished error list when curl cannot start.
fpa_ai_request_start <- function(url, messages, model, style = "openai", key = "", max_tokens = 1200L) {
  dir <- tempfile("fpa-ai-")
  dir.create(dir, showWarnings = FALSE)
  body <- fpa_ai_messages_body(messages, model, style, max_tokens)
  header <- file.path(dir, "headers.txt")
  payload <- file.path(dir, "body.json")
  out <- file.path(dir, "out.txt")
  code <- file.path(dir, "code.txt")
  err <- file.path(dir, "err.txt")
  writeLines(fpa_ai_header_lines(key, style), header, useBytes = TRUE)
  jsonlite::write_json(body, payload, auto_unbox = TRUE, null = "null")
  args <- c(
    "-sS", "--connect-timeout", "8", "-m", "120",
    "-D", file.path(dir, "resp.hdr"),
    "-o", out,
    "-w", "%{http_code}",
    "-H", paste0("@", header),
    "--data-binary", paste0("@", payload),
    url
  )
  status <- tryCatch(
    system2("curl", args, stdout = code, stderr = err, wait = FALSE),
    error = function(e) e
  )
  job <- list(
    dir = dir, header = header, payload = payload, out = out, code = code, err = err,
    started = Sys.time(), style = style
  )
  if (inherits(status, "error")) {
    job$done <- list(ok = FALSE, kind = "offline", status = 0L, text = "")
  }
  job
}

fpa_ai_interpret <- function(status, body, style = "openai") {
  status <- suppressWarnings(as.integer(status[[1]] %||% 0))
  if (is.na(status)) status <- 0L
  body <- paste(as.character(body %||% ""), collapse = "\n")
  if (!status) return(list(ok = FALSE, kind = "offline", status = 0L, text = ""))
  if (identical(status, 401L) || identical(status, 403L)) {
    return(list(ok = FALSE, kind = "auth", status = status, text = ""))
  }
  if (identical(status, 429L)) return(list(ok = FALSE, kind = "rate", status = status, text = ""))
  if (status < 200L || status >= 300L) {
    return(list(ok = FALSE, kind = "http", status = status, text = substr(body, 1L, 240L)))
  }
  parsed <- tryCatch(jsonlite::fromJSON(body, simplifyVector = FALSE), error = function(e) NULL)
  text <- ""
  if (identical(style, "anthropic") && is.list(parsed)) {
    block <- parsed$content
    if (is.list(block) && length(block)) text <- as.character(block[[1]]$text %||% "")
  } else if (is.list(parsed)) {
    choice <- parsed$choices
    if (is.list(choice) && length(choice)) {
      text <- as.character(choice[[1]]$message$content %||% "")
    }
  }
  if (!nzchar(text)) return(list(ok = FALSE, kind = "empty", status = status, text = ""))
  list(ok = TRUE, kind = "ok", status = status, text = text)
}

#' NULL while the call is still running.
fpa_ai_request_poll <- function(job, timeout_sec = 130) {
  if (is.null(job)) return(NULL)
  if (!is.null(job$done)) {
    unlink(job$dir, recursive = TRUE)
    return(job$done)
  }
  code <- ""
  if (file.exists(job$code)) {
    code <- tryCatch(paste(suppressWarnings(readLines(job$code, warn = FALSE)), collapse = ""), error = function(e) "")
    code <- trimws(code)
  }
  if (!grepl("^[0-9]{3}$", code)) {
    if (as.numeric(difftime(Sys.time(), job$started, units = "secs")) > timeout_sec) {
      unlink(job$dir, recursive = TRUE)
      return(list(ok = FALSE, kind = "timeout", status = 0L, text = ""))
    }
    return(NULL)
  }
  body <- ""
  if (file.exists(job$out)) {
    body <- paste(readLines(job$out, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  }
  hit <- fpa_ai_interpret(code, body, job$style %||% "openai")
  unlink(job$dir, recursive = TRUE)
  hit
}

fpa_ai_user_prompts_path <- function(config_path) {
  if (!nzchar(config_path %||% "")) return("")
  file.path(dirname(config_path), "work", "ai-user-prompts.json")
}

fpa_ai_user_prompts_read <- function(config_path) {
  path <- fpa_ai_user_prompts_path(config_path)
  if (!nzchar(path) || !file.exists(path)) return(list())
  got <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.list(got)) return(list())
  Filter(function(x) is.list(x) && nzchar(x$text %||% "") && nzchar(x$name %||% ""), got)
}

fpa_ai_user_prompts_write <- function(config_path, items) {
  path <- fpa_ai_user_prompts_path(config_path)
  if (!nzchar(path)) return(invisible(FALSE))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ok <- tryCatch({
    jsonlite::write_json(items, path, auto_unbox = TRUE, pretty = TRUE)
    TRUE
  }, error = function(e) FALSE)
  invisible(isTRUE(ok))
}

fpa_ai_user_prompt_add <- function(config_path, name, text) {
  name <- substr(trimws(name %||% ""), 1L, 80L)
  text <- trimws(text %||% "")
  if (!nzchar(name) || !nzchar(text)) return(invisible(NULL))
  items <- fpa_ai_user_prompts_read(config_path)
  items[[length(items) + 1L]] <- list(
    id = paste0("u", format(Sys.time(), "%Y%m%d%H%M%OS3"), sprintf("%03d", sample.int(999L, 1L))),
    name = name,
    text = text
  )
  fpa_ai_user_prompts_write(config_path, items)
  invisible(items[[length(items)]])
}

fpa_ai_user_prompt_drop <- function(config_path, id) {
  items <- fpa_ai_user_prompts_read(config_path)
  items <- Filter(function(x) !identical(x$id, id), items)
  fpa_ai_user_prompts_write(config_path, items)
  fpa_ai_fav_purge(config_path, id)
  invisible(items)
}

fpa_ai_fav_path <- function(config_path) {
  if (!nzchar(config_path %||% "")) return("")
  file.path(dirname(config_path), "work", "ai-favorites.json")
}

fpa_ai_fav_default <- function() list(favorites = character(), sets = list())

fpa_ai_fav_read <- function(config_path) {
  path <- fpa_ai_fav_path(config_path)
  out <- fpa_ai_fav_default()
  if (!nzchar(path) || !file.exists(path)) return(out)
  got <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.list(got)) return(out)
  fav <- as.character(got$favorites %||% character())
  fav <- unique(fav[nzchar(fav)])
  sets <- got$sets %||% list()
  if (!is.list(sets)) sets <- list()
  clean <- list()
  for (s in sets) {
    if (!is.list(s)) next
    sid <- as.character(s$id %||% "")
    name <- substr(trimws(as.character(s$name %||% "")), 1L, 80L)
    items <- as.character(s$items %||% character())
    items <- items[nzchar(items)]
    if (!nzchar(sid) || !nzchar(name)) next
    clean[[length(clean) + 1L]] <- list(id = sid, name = name, items = as.list(items))
  }
  list(favorites = fav, sets = clean)
}

fpa_ai_fav_write <- function(config_path, data) {
  path <- fpa_ai_fav_path(config_path)
  if (!nzchar(path)) return(invisible(FALSE))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  payload <- list(
    favorites = as.character(data$favorites %||% character()),
    sets = data$sets %||% list()
  )
  ok <- tryCatch({
    jsonlite::write_json(payload, path, auto_unbox = TRUE, pretty = TRUE)
    TRUE
  }, error = function(e) FALSE)
  invisible(isTRUE(ok))
}

fpa_ai_fav_toggle <- function(config_path, id) {
  id <- as.character(id %||% "")
  if (!nzchar(id)) return(invisible(FALSE))
  data <- fpa_ai_fav_read(config_path)
  if (id %in% data$favorites) {
    data$favorites <- setdiff(data$favorites, id)
  } else {
    data$favorites <- c(data$favorites, id)
  }
  fpa_ai_fav_write(config_path, data)
  invisible(id %in% data$favorites)
}

fpa_ai_fav_is <- function(config_path, id) {
  id %in% fpa_ai_fav_read(config_path)$favorites
}

fpa_ai_fav_purge <- function(config_path, id) {
  id <- as.character(id %||% "")
  if (!nzchar(id)) return(invisible(FALSE))
  data <- fpa_ai_fav_read(config_path)
  data$favorites <- setdiff(data$favorites, id)
  data$sets <- lapply(data$sets, function(s) {
    s$items <- as.list(setdiff(as.character(unlist(s$items)), id))
    s
  })
  fpa_ai_fav_write(config_path, data)
  invisible(TRUE)
}

fpa_ai_set_new_id <- function() {
  paste0("s", format(Sys.time(), "%Y%m%d%H%M%OS3"), sprintf("%03d", sample.int(999L, 1L)))
}

fpa_ai_set_get <- function(config_path, set_id) {
  data <- fpa_ai_fav_read(config_path)
  hit <- Filter(function(s) identical(s$id, set_id), data$sets)
  if (!length(hit)) NULL else hit[[1]]
}

fpa_ai_set_add <- function(config_path, name) {
  name <- substr(trimws(name %||% ""), 1L, 80L)
  if (!nzchar(name)) return(invisible(NULL))
  data <- fpa_ai_fav_read(config_path)
  set <- list(id = fpa_ai_set_new_id(), name = name, items = list())
  data$sets[[length(data$sets) + 1L]] <- set
  fpa_ai_fav_write(config_path, data)
  invisible(set)
}

fpa_ai_set_rename <- function(config_path, set_id, name) {
  name <- substr(trimws(name %||% ""), 1L, 80L)
  if (!nzchar(name) || !nzchar(set_id %||% "")) return(invisible(FALSE))
  data <- fpa_ai_fav_read(config_path)
  found <- FALSE
  data$sets <- lapply(data$sets, function(s) {
    if (identical(s$id, set_id)) {
      s$name <- name
      found <<- TRUE
    }
    s
  })
  if (!found) return(invisible(FALSE))
  fpa_ai_fav_write(config_path, data)
  invisible(TRUE)
}

fpa_ai_set_drop <- function(config_path, set_id) {
  data <- fpa_ai_fav_read(config_path)
  data$sets <- Filter(function(s) !identical(s$id, set_id), data$sets)
  fpa_ai_fav_write(config_path, data)
  invisible(TRUE)
}

fpa_ai_set_item_add <- function(config_path, set_id, item_id) {
  item_id <- as.character(item_id %||% "")
  if (!nzchar(item_id) || !nzchar(set_id %||% "")) return(invisible(FALSE))
  data <- fpa_ai_fav_read(config_path)
  found <- FALSE
  dup <- FALSE
  data$sets <- lapply(data$sets, function(s) {
    if (!identical(s$id, set_id)) return(s)
    found <<- TRUE
    ids <- as.character(unlist(s$items))
    if (item_id %in% ids) {
      dup <<- TRUE
      return(s)
    }
    s$items <- as.list(c(ids, item_id))
    s
  })
  if (!found || dup) return(invisible(FALSE))
  fpa_ai_fav_write(config_path, data)
  invisible(TRUE)
}

#' Append every current favorite that is not already in the set, in favorite order.
#' Returns how many prompts were added.
fpa_ai_set_items_add_favs <- function(config_path, set_id) {
  set_id <- as.character(set_id %||% "")
  if (!nzchar(set_id)) return(invisible(0L))
  data <- fpa_ai_fav_read(config_path)
  favs <- as.character(unlist(data$favorites))
  favs <- favs[nzchar(favs)]
  if (!length(favs)) return(invisible(0L))
  n_add <- 0L
  found <- FALSE
  data$sets <- lapply(data$sets, function(s) {
    if (!identical(s$id, set_id)) return(s)
    found <<- TRUE
    ids <- as.character(unlist(s$items))
    add <- favs[!favs %in% ids]
    n_add <<- length(add)
    if (length(add)) s$items <- as.list(c(ids, add))
    s
  })
  if (!found || n_add < 1L) return(invisible(0L))
  fpa_ai_fav_write(config_path, data)
  invisible(n_add)
}

fpa_ai_set_item_remove <- function(config_path, set_id, index) {
  index <- suppressWarnings(as.integer(index))
  if (!is.finite(index) || index < 1L || !nzchar(set_id %||% "")) return(invisible(FALSE))
  data <- fpa_ai_fav_read(config_path)
  found <- FALSE
  data$sets <- lapply(data$sets, function(s) {
    if (!identical(s$id, set_id)) return(s)
    found <<- TRUE
    ids <- as.character(unlist(s$items))
    if (index > length(ids)) return(s)
    s$items <- as.list(ids[-index])
    s
  })
  if (!found) return(invisible(FALSE))
  fpa_ai_fav_write(config_path, data)
  invisible(TRUE)
}

fpa_ai_set_move <- function(config_path, set_id, index, dir, to = NULL) {
  index <- suppressWarnings(as.integer(index))
  dir <- as.character(dir %||% "")
  to <- suppressWarnings(as.integer(to %||% NA_integer_))
  if (!is.finite(index) || index < 1L || !nzchar(set_id %||% "")) return(invisible(FALSE))
  data <- fpa_ai_fav_read(config_path)
  moved <- FALSE
  data$sets <- lapply(data$sets, function(s) {
    if (!identical(s$id, set_id)) return(s)
    ids <- as.character(unlist(s$items))
    n <- length(ids)
    if (!n || index > n) return(s)
    if (identical(dir, "top")) {
      if (index == 1L) return(s)
      id <- ids[[index]]
      ids <- c(id, ids[-index])
    } else if (identical(dir, "bottom")) {
      if (index == n) return(s)
      id <- ids[[index]]
      ids <- c(ids[-index], id)
    } else if (identical(dir, "to")) {
      if (!is.finite(to) || to < 1L || to > n || to == index) return(s)
      id <- ids[[index]]
      rest <- ids[-index]
      ids <- append(rest, id, after = to - 1L)
    } else {
      j <- if (identical(dir, "up")) index - 1L else if (identical(dir, "down")) index + 1L else index
      if (j < 1L || j > n || j == index) return(s)
      tmp <- ids[[index]]
      ids[[index]] <- ids[[j]]
      ids[[j]] <- tmp
    }
    s$items <- as.list(ids)
    moved <<- TRUE
    s
  })
  if (!moved) return(invisible(FALSE))
  fpa_ai_fav_write(config_path, data)
  invisible(TRUE)
}

#' Build the ordered ask queue for a set. Unknown ids are skipped.
fpa_ai_batch_queue <- function(set, catalog = NULL, user_prompts = NULL, lang = "fr") {
  if (is.null(set) || !is.list(set)) return(list())
  if (is.null(catalog)) catalog <- if (exists("fpa_ai_catalog", mode = "function")) fpa_ai_catalog() else list()
  if (is.null(user_prompts)) user_prompts <- list()
  lang <- if (identical(lang, "en")) "en" else "fr"
  out <- list()
  for (id in as.character(unlist(set$items %||% list()))) {
    if (!nzchar(id)) next
    cat_hit <- Filter(function(x) identical(x$id, id), catalog)
    if (length(cat_hit)) {
      item <- cat_hit[[1]]
      out[[length(out) + 1L]] <- list(
        id = id,
        recipe = fpa_ai_pick(item, "recipe", lang),
        slice = item$slice,
        title = fpa_ai_pick(item, "label", lang)
      )
      next
    }
    user_hit <- Filter(function(x) identical(x$id, id), user_prompts)
    if (length(user_hit)) {
      item <- user_hit[[1]]
      out[[length(out) + 1L]] <- list(
        id = id,
        recipe = item$text,
        slice = NULL,
        title = item$name
      )
    }
  }
  out
}
