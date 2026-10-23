## WMed-EwEbuilder - building Ecopath with Ecosim inputs for the Western Mediterranean
## (biomass, fisheries, P/B, Q/B, traits and diets). This file: lib_gfcm_saf_online.R
## =================================================================
## lib_gfcm_saf_online.R
## Created by: Daniel Vilas
##
## Latest GFCM stock assessments, straight from the GFCM website.
##
## GFCM publishes every validated Stock Assessment Form (SAF) as a PDF in a
## public Azure container (the same files the "GFCM validated SAFs" Power BI
## browser links to, https://www.fao.org/gfcm/data/public-data-and-
## information/fisheries-and-aquaculture-data/stock-assessment-data-and-
## information/en). The container can be listed, so each run:
##   1. lists every SAF PDF (gfcm_saf_list_online()),
##   2. reads stock (FAO 3-alpha code), GSAs and reference year from the
##      file name (gfcm_saf_parse_names()), both naming styles:
##        SAF_HKE_01_05_06_07_RefY2024.pdf   (2021 onwards)
##        HKE_GSA_9-11_2019_FRA_ITA.pdf      (up to ~2020)
##   3. keeps stocks whose GSAs are all in the model GSAs,
##   4. downloads the newest SAF of each stock (cached, not downloaded again),
##   5. reads the summary results table from the PDF text
##      (gfcm_saf_parse_pdf(): the table with a year column and an SSB, or
##      else a total biomass, column; values in tonnes),
##   6. if the newest SAF has no usable table, tries the older SAFs of the
##      same stock (GFCM_SAF_MAX_BACK of them),
##   7. picks a set of stocks that do not overlap: per species, newest
##      reference year first, then the larger stock unit (e.g. hake GSAs
##      1-5-6-7 over hake GSA 1),
##   8. writes data/fisheries/GFCM_SAF/gfcm_saf_stock_assessments_ALL.csv
##      (same columns as the hand-transcribed gfcm_saf_stock_assessments.csv,
##      plus use_for_biomass and parse_status) and
##      out_dir/biomass/gfcm_saf_parse_REVIEW.csv (one row per stock tried).
## Hand-transcribed rows (gfcm_saf_stock_assessments.csv) are kept for any
## stock whose PDF could not be read. Stocks still without a GFCM value can
## be filled from RAM Legacy (GFCM_SAF_FILL_FROM_RAM), only where no GFCM
## stock of the same species overlaps.
##
## LIMITS (flagged): many data-limited assessments (SPiCT, CMSY, survey-
## based advice) and most small-pelagic SAFs give biomass only in figures,
## not tables - those stocks get no value. The table reader uses word
## positions; a table whose header mixes two quantities over one column is
## flagged "check: ambiguous header" and not used. Check
## gfcm_saf_parse_REVIEW.csv after the first run.
## Needs packages pdftools and xml2, and internet on the first run.
## =================================================================

GFCM_SAF_BLOB <- "https://gfcmsitestorage.blob.core.windows.net/website"
if (!exists("GFCM_SAF_MAX_BACK")) GFCM_SAF_MAX_BACK <- 2          # older SAFs tried when the newest has no table
if (!exists("GFCM_SAF_FILL_FROM_RAM")) GFCM_SAF_FILL_FROM_RAM <- TRUE

## --- 1. list the container -------------------------------------------
gfcm_saf_list_online <- function(prefix = "5.Data/SAFs/") {
  out <- list(); marker <- ""
  repeat {
    u <- paste0(GFCM_SAF_BLOB, "?restype=container&comp=list&prefix=", utils::URLencode(prefix, reserved = TRUE),
                if (nzchar(marker)) paste0("&marker=", utils::URLencode(marker, reserved = TRUE)) else "")
    x <- xml2::read_xml(u)
    b <- xml2::xml_find_all(x, ".//Blob")
    out[[length(out) + 1]] <- data.table(
      name = xml2::xml_text(xml2::xml_find_first(b, "./Name")),
      bytes = suppressWarnings(as.numeric(xml2::xml_text(xml2::xml_find_first(b, "./Properties/Content-Length")))),
      last_modified = xml2::xml_text(xml2::xml_find_first(b, "./Properties/Last-Modified")))
    marker <- xml2::xml_text(xml2::xml_find_first(x, ".//NextMarker"))
    if (is.na(marker) || !nzchar(marker)) break
  }
  rbindlist(out)
}

## --- 2. stock, GSAs and reference year from the file name -----------
.saf_gsa_list <- function(raw) {
  g <- character(0)
  for (part in strsplit(raw, "_", fixed = TRUE)[[1]]) {
    if (!nzchar(part)) next
    r <- strsplit(part, "-", fixed = TRUE)[[1]]; r <- r[nzchar(r)]
    if (length(r) == 2 && !any(grepl(".", r, fixed = TRUE))) g <- c(g, as.character(seq(as.integer(r[1]), as.integer(r[2]))))
    else g <- c(g, sub("^0+(?=\\d)", "", r, perl = TRUE))
  }
  g
}
gfcm_saf_parse_names <- function(lst) {
  lst <- lst[grepl("\\.pdf$", name, ignore.case = TRUE)]
  f <- basename(lst$name)
  rows <- lapply(seq_along(f), function(i) {
    m <- regmatches(f[i], regexec("^SAF_([A-Z]{3})_([0-9._]+?)_RefY(\\d{4})\\.pdf$", f[i], ignore.case = TRUE))[[1]]
    if (length(m) == 4) return(list(code = toupper(m[2]), gsas = .saf_gsa_list(m[3]), ref_year = as.integer(m[4])))
    m <- regmatches(f[i], regexec("^([A-Z]{3})_GSA[-_ ]?([0-9][0-9_.-]*?)_((?:(?:19|20)[0-9]{2}[-_]?)+)(?:_[A-Za-z_-]+)?\\.pdf$", f[i], ignore.case = TRUE))[[1]]
    if (length(m) == 4) {
      ys <- as.integer(regmatches(m[4], gregexpr("(19|20)[0-9]{2}", m[4]))[[1]])
      return(list(code = toupper(m[2]), gsas = .saf_gsa_list(m[3]), ref_year = max(ys)))
    }
    NULL
  })
  ok <- !vapply(rows, is.null, logical(1))
  d <- lst[ok]; rows <- rows[ok]
  d[, `:=`(code = vapply(rows, `[[`, "", "code"),
           gsa = vapply(rows, function(r) paste(r$gsas, collapse = ","), ""),
           ref_year = vapply(rows, `[[`, 1L, "ref_year"),
           folder_year = suppressWarnings(as.integer(vapply(strsplit(name, "/"), function(v) v[4], ""))),
           group = fifelse(grepl("SmallPelagics", name), "Small pelagics", "Demersal"))]
  d[, stock_key := paste0(code, "_", gsub(",", "_", gsa))]
  d[, url := paste0(GFCM_SAF_BLOB, "/", vapply(strsplit(name, "/"), function(v) paste(utils::URLencode(v, reserved = TRUE), collapse = "/"), ""))]
  d[]
}
.saf_gsa_int <- function(gsa) floor(suppressWarnings(as.numeric(strsplit(gsa, ",", fixed = TRUE)[[1]])))

## --- 4. download (cached; one write per file on pCloud) ---------------
gfcm_saf_download <- function(url, dest) {
  if (file.exists(dest) && isTRUE(file.size(dest) > 1000)) return(TRUE)
  tmp <- tempfile(fileext = ".pdf")
  ok <- tryCatch({ utils::download.file(url, tmp, mode = "wb", quiet = TRUE); TRUE },
                 error = function(e) { message("[GFCM SAF] download failed: ", basename(dest), " - ", conditionMessage(e)); FALSE })
  if (ok) { dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE); ok <- file.copy(tmp, dest, overwrite = TRUE) }
  unlink(tmp); ok
}

## --- 5. read the summary table from the PDF ---------------------------
## Numbers: "11,133" (thousands) -> 11133; "122,4" (decimal comma) -> 122.4.
.saf_num <- function(s) {
  s <- gsub("[*†]", "", s)
  th <- grepl("^-?[0-9]{1,3}(,[0-9]{3})+(\\.[0-9]+)?$", s)
  s[th] <- gsub(",", "", s[th])
  dc <- !th & grepl("^-?[0-9]+,[0-9]+$", s)
  s[dc] <- sub(",", ".", s[dc], fixed = TRUE)
  out <- rep(NA_real_, length(s))
  ok <- grepl("^-?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", s)
  out[ok] <- as.numeric(s[ok]); out
}
## words: data.table(page, x, y, width, text) - pdftools::pdf_data() words.
gfcm_saf_parse_words <- function(words, ref_year) {
  bad <- "/|ratio|rel\\.|relative|index|msy|lim\\b|low|high|upp|inf\\b|sup\\b|\\bcv\\b|\\bsd\\b|stdev|%|density|kg/km|n/km|survey|forecast|scenario"
  qty <- c("recruit|\\brec\\b|\\br\\(", "\\bssb\\b|spawning", "\\btsb\\b|\\btb\\b|total biomass|biomass", "catch|landing|yield", "\\bf\\b|fbar|f bar|f\\(")
  cands <- list()
  for (p in sort(unique(words$page))) {
    pw <- words[page == p][order(y, x)]
    if (!nrow(pw)) next
    ## lines: words within 2.5 units of the line's first y
    ly <- numeric(0); lid <- integer(nrow(pw))
    for (i in seq_len(nrow(pw))) {
      j <- which(abs(ly - pw$y[i]) < 2.5)
      if (length(j)) lid[i] <- j[1] else { ly <- c(ly, pw$y[i]); lid[i] <- length(ly) }
    }
    pw[, line := lid]
    L <- lapply(order(ly), function(l) {
      w <- pw[line == l][order(x)]
      ## join a number split in two pieces ("733." "5")
      if (nrow(w) > 1) for (k in nrow(w):2) {
        if (w$x[k] - (w$x[k - 1] + w$width[k - 1]) < 1.5 && grepl("[0-9.,]$", w$text[k - 1]) && grepl("^[0-9.,]", w$text[k])) {
          w$width[k - 1] <- w$x[k] + w$width[k] - w$x[k - 1]; w$text[k - 1] <- paste0(w$text[k - 1], w$text[k]); w <- w[-k]
        }
      }
      list(y = ly[l], w = w)
    })
    is_row <- vapply(L, function(l) {
      t <- l$w$text[1]
      grepl("^(19|20)[0-9]{2}$", t) && as.integer(t) >= 1950 && as.integer(t) <= ref_year + 1 &&
        nrow(l$w) > 1 && any(!is.na(.saf_num(l$w$text[-1])))
    }, logical(1))
    i <- 1
    while (i <= length(L)) {
      if (!is_row[i]) { i <- i + 1; next }
      j <- i; rows <- integer(0); gap <- 0
      while (j <= length(L)) { if (is_row[j]) { rows <- c(rows, j); gap <- 0 } else { gap <- gap + 1; if (gap > 1) break }; j <- j + 1 }
      if (length(rows) >= 5) {
        nums <- lapply(rows, function(r) { w <- L[[r]]$w[-1]; w[!is.na(.saf_num(text))] })
        ref_r <- nums[[which.max(vapply(nums, nrow, 1L))]]
        cc <- ref_r$x + ref_r$width / 2
        ## header: lines just above the first row (gaps <= 24 units)
        k <- rows[1]; hdr <- integer(0); h <- k - 1
        while (h >= 1 && h >= k - 5) {
          if ((if (h == k - 1) L[[k]]$y else L[[h + 1]]$y) - L[[h]]$y > 24) break
          if (any(grepl("[A-Za-z]", L[[h]]$w$text))) hdr <- c(hdr, h)
          h <- h - 1
        }
        yx <- L[[k]]$w$x[1] + L[[k]]$w$width[1]
        lab <- rep("", length(cc))
        for (hh in rev(hdr)) {
          w <- L[[hh]]$w
          ## phrases: words less than 8 units apart
          ph <- cumsum(c(1, diff(w$x) - head(w$width, -1) >= 8))
          for (q in unique(ph)) {
            ww <- w[ph == q]; t <- paste(ww$text, collapse = " ")
            x1 <- max(ww$x + ww$width)
            if (grepl("^year", t, ignore.case = TRUE) || x1 < yx) next
            ci <- which.min(abs(cc - (min(ww$x) + x1) / 2))
            lab[ci] <- trimws(paste(lab[ci], t))
          }
        }
        capl <- if (k - length(hdr) - 1 >= 1) max(1, k - length(hdr) - 4):(k - length(hdr) - 1) else integer(0)
        cap <- paste(vapply(capl, function(z) paste(L[[z]]$w$text, collapse = " "), ""), collapse = " ")
        cands[[length(cands) + 1]] <- list(page = p, rows = lapply(rows, function(r) L[[r]]$w), cc = cc, lab = lab, cap = cap)
      }
      i <- max(j, i + 1)
    }
  }
  best <- NULL
  for (c in cands) {
    if (grepl("survey|forecast|scenario|projection|short.?term", c$cap, ignore.case = TRUE)) next
    s <- vapply(c$lab, function(l) {
      if (grepl("\\bSSB\\b|spawning", l, ignore.case = TRUE) && !grepl(bad, l, ignore.case = TRUE)) 2
      else if (grepl("\\bTSB\\b|\\bTB\\b|total biomass|biomass|^B\\b|^B ?\\(", l, ignore.case = TRUE) && !grepl(bad, l, ignore.case = TRUE) &&
               !grepl("SSB", l, ignore.case = TRUE)) 1 else 0
    }, numeric(1))
    if (max(s) == 0) next
    sc <- max(s) * 1000 + length(c$rows)
    if (is.null(best) || sc > best$score) best <- c(c, list(ci = which.max(s), score = sc, type = if (max(s) == 2) "SSB" else "B"))
  }
  if (is.null(best)) return(list(status = "no biomass table found", data = NULL))
  label <- best$lab[best$ci]
  n_qty <- sum(vapply(qty, function(q) grepl(q, label, ignore.case = TRUE), logical(1)))
  ci_c <- which(grepl("catch|landing|yield", best$lab, ignore.case = TRUE) & !grepl(bad, best$lab, ignore.case = TRUE))[1]
  sp <- if (length(best$cc) > 1) min(diff(best$cc)) else 50
  pick <- function(w, ci) {
    if (is.na(ci)) return(NA_real_)
    w <- w[-1]; v <- .saf_num(w$text); w <- w[!is.na(v)]; v <- v[!is.na(v)]
    if (!length(v)) return(NA_real_)
    d <- abs(w$x + w$width / 2 - best$cc[ci]); j <- which.min(d)
    if (d[j] < sp * 0.6) v[j] else NA_real_
  }
  dt <- data.table(year = vapply(best$rows, function(w) as.integer(w$text[1]), 1L),
                   biomass = vapply(best$rows, pick, 1, ci = best$ci),
                   catches = vapply(best$rows, pick, 1, ci = ci_c))
  ## units: tonnes unless the label says kg or thousand tonnes
  if (grepl("\\(kg\\)|\\bkg\\b", label, ignore.case = TRUE)) dt[, biomass := biomass / 1000]
  if (grepl("000 ?t|thousand t|'000", label, ignore.case = TRUE)) dt[, biomass := biomass * 1000]
  nb <- dt[is.finite(biomass) & biomass > 0, .N]
  last <- suppressWarnings(dt[is.finite(biomass), max(year)])
  flags <- c(if (n_qty > 1 || nchar(label) > 40) "ambiguous header",
             if (nb < 5) "fewer than 5 values",
             if (is.finite(last) && last < ref_year - 2) paste0("last year ", last, " < reference year - 2"))
  list(status = if (length(flags)) paste0("check: ", paste(flags, collapse = "; ")) else "ok",
       data = dt[is.finite(biomass) & biomass > 0], page = best$page, label = label, type = best$type)
}
gfcm_saf_parse_pdf <- function(pdf, ref_year) {
  pg <- tryCatch(pdftools::pdf_data(pdf), error = function(e) NULL)
  if (is.null(pg)) return(list(status = "PDF could not be read", data = NULL))
  words <- rbindlist(lapply(seq_along(pg), function(i) if (nrow(pg[[i]])) data.table(page = i, x = pg[[i]]$x, y = pg[[i]]$y, width = pg[[i]]$width, text = pg[[i]]$text)))
  if (!nrow(words)) return(list(status = "no text in PDF (scanned?)", data = NULL))
  gfcm_saf_parse_words(words, ref_year)
}

## --- main ----------------------------------------------------------------
## Returns the path of gfcm_saf_stock_assessments_ALL.csv, or NULL (offline
## with no earlier download, or nothing readable): the caller then uses RAM Legacy.
update_gfcm_saf_assessments <- function(pcloud_dir, review_dir, model_gsas, asfis_file = NULL, update = TRUE,
                                        ram_path = file.path(pcloud_dir, "data/fisheries/STAR_RAMLegacy/combined_medbs_star_ramlegacy.csv")) {
  saf_dir <- file.path(pcloud_dir, "data/fisheries/GFCM_SAF")
  all_path <- file.path(saf_dir, "gfcm_saf_stock_assessments_ALL.csv")
  hand_path <- file.path(saf_dir, "gfcm_saf_stock_assessments.csv")
  if (!isTRUE(update) && file.exists(all_path)) { message("[GFCM SAF] GFCM_SAF_UPDATE = FALSE - using ", all_path); return(all_path) }
  if (!requireNamespace("pdftools", quietly = TRUE) || !requireNamespace("xml2", quietly = TRUE)) {
    message("[GFCM SAF] packages pdftools/xml2 missing (run install_packages.R) - ",
            if (file.exists(all_path)) "using the last downloaded table." else "falling back to RAM Legacy.")
    return(if (file.exists(all_path)) all_path else NULL)
  }
  lst <- tryCatch(gfcm_saf_list_online(), error = function(e) { message("[GFCM SAF] could not list the GFCM website (", conditionMessage(e), ")"); NULL })
  if (is.null(lst) || !nrow(lst)) {
    message("[GFCM SAF] offline - ", if (file.exists(all_path)) "using the last downloaded table." else "falling back to RAM Legacy.")
    return(if (file.exists(all_path)) all_path else NULL)
  }
  saf <- gfcm_saf_parse_names(lst)
  saf <- saf[vapply(gsa, function(g) { v <- .saf_gsa_int(g); length(v) > 0 && !anyNA(v) && all(v %in% model_gsas) }, logical(1))]
  message("[GFCM SAF] ", nrow(lst), " files online; ", nrow(saf), " SAF(s) of ", uniqueN(saf$stock_key), " stock(s) inside GSAs ",
          paste(model_gsas, collapse = ","), ".")
  setorder(saf, stock_key, -ref_year, -folder_year)
  pdf_dir <- file.path(saf_dir, "pdf")
  review <- list(); res <- list()
  for (k in unique(saf$stock_key)) {
    tries <- head(saf[stock_key == k], 1 + GFCM_SAF_MAX_BACK)
    for (i in seq_len(nrow(tries))) {
      s <- tries[i]
      dest <- file.path(pdf_dir, basename(s$name))
      r <- if (gfcm_saf_download(s$url, dest)) gfcm_saf_parse_pdf(dest, s$ref_year) else list(status = "download failed", data = NULL)
      review[[length(review) + 1]] <- data.table(stock_key = k, code = s$code, gsa = s$gsa, ref_year = s$ref_year, file = basename(s$name),
                                                 status = r$status, page = if (is.null(r$page)) NA_integer_ else r$page,
                                                 column = if (is.null(r$label)) NA_character_ else r$label,
                                                 biomass_type = if (is.null(r$type)) NA_character_ else r$type,
                                                 n_years = if (is.null(r$data)) 0L else nrow(r$data),
                                                 first_year = if (is.null(r$data) || !nrow(r$data)) NA_integer_ else min(r$data$year),
                                                 last_year = if (is.null(r$data) || !nrow(r$data)) NA_integer_ else max(r$data$year), url = s$url)
      if (identical(r$status, "ok")) {
        res[[k]] <- r$data[, `:=`(stock_key = k, code = s$code, gsa = s$gsa, ref_year = s$ref_year, biomass_type = r$type,
                                  column = r$label, page = r$page, url = s$url, group = s$group)]
        break
      }
    }
  }
  review <- rbindlist(review)
  out <- rbindlist(res, fill = TRUE)
  ## species names from the ASFIS list (FAO 3-alpha codes); needed to match
  ## the hand-transcribed and RAM Legacy rows, which carry species names
  if (is.null(asfis_file) || !file.exists(asfis_file))
    asfis_file <- head(list.files(file.path(pcloud_dir, "data"), pattern = "^ASFIS_sp_.*\\.csv$", recursive = TRUE, full.names = TRUE), 1)
  if (!length(asfis_file)) { message("[GFCM SAF] ASFIS species list not found under data/ - species kept as FAO codes; hand-transcribed and RAM rows cannot be checked for overlap."); asfis_file <- NULL }
  if (nrow(out)) {
    sp <- if (!is.null(asfis_file) && file.exists(asfis_file)) unique(fread(asfis_file, encoding = "UTF-8")[, .(code = Alpha3_Code, species = Scientific_Name, common_name = English_name)]) else data.table(code = character(0), species = character(0), common_name = character(0))
    out <- merge(out, sp[!duplicated(code)], by = "code", all.x = TRUE)
    out[is.na(species), species := code]
  }
  ## 7. non-overlapping set: per species, newest reference year, then larger unit
  if (nrow(out)) {
    st <- unique(out[, .(stock_key, species, gsa, ref_year)])
    st[, n_gsa := lengths(lapply(gsa, .saf_gsa_int))]
    setorder(st, species, -ref_year, -n_gsa)
    st[, use_for_biomass := FALSE]
    for (spp in unique(st$species)) {
      taken <- numeric(0)
      for (i in which(st$species == spp)) {
        g <- .saf_gsa_int(st$gsa[i])
        if (!any(g %in% taken)) { st$use_for_biomass[i] <- TRUE; taken <- c(taken, g) }
      }
    }
    out <- merge(out, st[, .(stock_key, use_for_biomass)], by = "stock_key")
    review <- merge(review, st[, .(stock_key, use_for_biomass)], by = "stock_key", all.x = TRUE)
  }
  ## same columns as the hand-transcribed table
  if (nrow(out)) out <- out[, .(source = "GFCM SAF (online)", stock_key, species, common_name, gsa, subregion = "", year, biomass, biomass_type,
                                catches, landings = NA_real_, landings_flag = NA_character_, model = NA_character_, ref_year,
                                reference = paste0("GFCM Stock Assessment Form: ", species, " in GSA ", gsub(",", "-", gsa), ", reference year ",
                                                   ref_year, " (column '", column, "', page ", page, "; read automatically from the PDF)"),
                                url, use_for_biomass, parse_status = "ok")]
  ## hand-transcribed rows for stocks with no usable PDF table
  if (file.exists(hand_path)) {
    hand <- fread(hand_path, encoding = "UTF-8")
    hand[, stock_key := vapply(strsplit(stock_key, "_"), function(v) paste(c(v[1], sub("^0+(?=\\d)", "", v[-1], perl = TRUE)), collapse = "_"), "")]
    hand <- hand[!stock_key %in% out$stock_key]
    if (nrow(hand)) {
      hand[, `:=`(use_for_biomass = TRUE, parse_status = "hand-transcribed")]
      for (i in which(hand$species %in% out$species)) {       # drop if it overlaps a GFCM stock in use
        g <- .saf_gsa_int(hand$gsa[i])
        if (out[species == hand$species[i] & use_for_biomass == TRUE, any(g %in% unlist(lapply(gsa, .saf_gsa_int)))]) hand$use_for_biomass[i] <- FALSE
      }
      out <- rbindlist(list(out, hand), fill = TRUE)
    }
  }
  ## RAM Legacy for stocks GFCM does not cover (no overlap with a GFCM stock of the same species)
  if (isTRUE(GFCM_SAF_FILL_FROM_RAM) && file.exists(ram_path)) {
    ram <- fread(ram_path, encoding = "UTF-8")
    if (all(c("stock_key", "species", "gsa") %in% names(ram))) {
      ram <- ram[vapply(as.character(gsa), function(g) { v <- floor(suppressWarnings(as.numeric(trimws(unlist(strsplit(g, "[,;]")))))); length(v) > 0 && !anyNA(v) && all(v %in% model_gsas) }, logical(1))]
      keep <- vapply(seq_len(nrow(ram)), function(i) {
        g <- floor(suppressWarnings(as.numeric(trimws(unlist(strsplit(as.character(ram$gsa[i]), "[,;]"))))))
        !out[species == ram$species[i] & use_for_biomass == TRUE, any(g %in% unlist(lapply(as.character(gsa), function(z) floor(suppressWarnings(as.numeric(trimws(unlist(strsplit(z, "[,;]")))))))))]
      }, logical(1))
      ram <- ram[keep]
      if (nrow(ram)) {
        ram[, `:=`(use_for_biomass = TRUE, parse_status = "RAM Legacy (no GFCM SAF table for this species and area)")]
        ram[, source := paste0("RAM Legacy fill: ", source)]
        message("[GFCM SAF] ", uniqueN(ram$stock_key), " stock(s) filled from RAM Legacy: ", paste(unique(ram$stock_key), collapse = ", "))
        out <- rbindlist(list(out, ram), fill = TRUE)
      }
    }
  }
  if (!nrow(out)) { message("[GFCM SAF] no stock assessment table could be read - falling back to RAM Legacy."); return(NULL) }
  dir.create(review_dir, recursive = TRUE, showWarnings = FALSE)
  fwrite(review[order(stock_key, -ref_year)], file.path(review_dir, "gfcm_saf_parse_REVIEW.csv"))
  fwrite(out, all_path)
  message("[GFCM SAF] ", out[parse_status == "ok", uniqueN(stock_key)], " stock(s) read from the GFCM PDFs (",
          out[parse_status == "ok" & use_for_biomass == TRUE, uniqueN(stock_key)], " used for biomass, the rest overlap a newer or larger stock); ",
          review[, uniqueN(stock_key)] - out[parse_status == "ok", uniqueN(stock_key)], " stock(s) without a readable table (",
          "see gfcm_saf_parse_REVIEW.csv). Written to ", all_path)
  all_path
}
