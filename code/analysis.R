#!/usr/bin/env Rscript
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
if (!file.exists(script)) script <- gsub("~+~", " ", script, fixed = TRUE)
root <- dirname(dirname(normalizePath(script, mustWork = TRUE)))
options(stringsAsFactors = FALSE)
args <- commandArgs(trailingOnly = TRUE)
data_dir <- if (length(args) >= 1L) normalizePath(args[1], mustWork = TRUE) else file.path(root, "data")
output_root <- if (length(args) >= 2L) args[2] else file.path(root, "output")
for (name in c("estimation", "partworths", "kmeans")) {
  dir.create(file.path(output_root, name), recursive = TRUE, showWarnings = FALSE)
}
excluded_ids <- c(11L, 25L, 27L, 41L)
coefficient_names <- c("intercept_l", paste0("V", 1:7, "_l"),
                       "intercept_d", paste0("V", 1:7, "_d"), "NRT_d", "Barrier_d")
item_names <- c("Starbucks_Latte", "Starbucks_Cappuccino", "McCafe_Latte",
                "McCafe_Cappuccino", "Peets_Latte", "Peets_Cappuccino",
                "discount_25", "discount_99")
write_output <- function(x, section, filename) {
  write.csv(x, file.path(output_root, section, filename), row.names = FALSE, na = "")
}

attribute_partworths <- function(d, model) {
  v <- function(i) d[[paste0("V", i, "_", model)]]
  mc <- (v(1) + v(2)) / 2
  ans <- cbind(McCafe = 0, Peets = (v(3) + v(4)) / 2 - mc,
               Starbucks = v(5) / 2 - mc, Cappuccino = 0,
               Latte = (v(2) + v(4) - v(1) - v(3) - v(5)) / 3,
               discount_0 = 0, discount_25 = v(6), discount_99 = v(7))
  if (model == "d") ans <- ans * d$Barrier_d
  ans
}

for (name in c("estimates", "partworths", "kmeans")) {
  source(file.path(root, "code", paste0(name, ".R")))
}
cat("Results written to", output_root, "\n")
