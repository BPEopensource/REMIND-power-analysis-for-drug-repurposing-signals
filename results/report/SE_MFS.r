# ============================================================
# Report Results
# ============================================================

library(data.table)

# --- Dir ---
cartella <- "results/report"
pattern  <- "\\.csv$"         


# --- FILES ---
file_csv <- list.files(
  path       = cartella,
  pattern    = pattern,
  full.names = TRUE
)


# --- IMPORT ---
invisible(lapply(file_csv, function(f) {
  nome <- tools::file_path_sans_ext(basename(f))
  assign(nome, fread(f, encoding = "UTF-8"), envir = .GlobalEnv)
}))


summary_dt <- function(dt) {
  cat("SE:", dt[select_percent >= 50, .N] / dt[, .N], "\n")
  cat("MFS:", dt[, mean(nFalse_signals, na.rm = TRUE)], "\n")
}


for (nm in ls()) {
  obj <- get(nm)
  if (is.data.table(obj)) {
    cat("\n---", nm, "---\n")
    summary_dt(obj)
  }
}

