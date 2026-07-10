## ---------------------------------------------------------------
## Download & inspect FAO Global Capture Production dataset
## (includes FAO Major Fishing Area 37 = Mediterranean & Black Sea,
## the same underlying data GFCM's capture-production dashboard draws from)
## ---------------------------------------------------------------

url      <- "https://www.fao.org/fishery/static/Data/Capture_2026.1.0.zip"
zip_file <- "Capture_2026.1.0.zip"
out_dir  <- "FAO_Capture_2026"

## --- 1. Download (skip if already present) ---------------------------

if (!file.exists(zip_file)) {
  message("Downloading FAO Global Capture Production dataset...")
  download.file(url, destfile = zip_file, mode = "wb", method = "libcurl")
} else {
  message("Zip already exists locally, skipping download.")
}

## --- 2. Unzip ------------------------------------------------------------

if (!dir.exists(out_dir)) dir.create(out_dir)
unzip(zip_file, exdir = out_dir)

files <- list.files(out_dir, recursive = TRUE, full.names = TRUE)
message("Extracted ", length(files), " file(s):")
print(files)

## --- 3. Preview headers of every CSV --------------------------------------

csv_files <- files[grepl("\\.csv$", files, ignore.case = TRUE)]

for (f in csv_files) {
  cat("\n============================\n")
  cat(basename(f), "\n")
  cat("============================\n")
  
  first_line <- readLines(f, n = 1, warn = FALSE)
  sep <- if (grepl(";", first_line)) ";" else ","
  
  df <- tryCatch(
    read.csv(f, sep = sep, nrows = 5, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) {
      cat("Error reading file:", conditionMessage(e), "\n")
      NULL
    }
  )
  
  if (!is.null(df)) {
    cat("Columns (", ncol(df), "):\n", sep = "")
    print(names(df))
    cat("\nFirst rows:\n")
    print(df)
  }
}
