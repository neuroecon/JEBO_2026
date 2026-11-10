estimates <- read.csv(file.path(data_dir, "estimates.csv"))
fit_input_dir <- file.path(data_dir, "model_fits")
stopifnot(nrow(estimates) == 59L, !anyDuplicated(estimates$ID),
          identical(sort(estimates$ID), setdiff(1:63, excluded_ids)))
rebuilt <- data.frame(ID = estimates$ID)
rebuilt[coefficient_names] <- NA_real_
for (j in seq_len(nrow(rebuilt))) {
  id <- rebuilt$ID[j]
  d <- read.csv(file.path(fit_input_dir, id, paste0(id, "_ddm_estimates.csv")), row.names = 1)
  l <- read.csv(file.path(fit_input_dir, id, paste0(id, "_logit_estimates.csv")), row.names = 1)
  ddm_rows <- c("v_Intercept", paste0("v_item[T.V_", 1:5, "]"), "v_Discount_25", "v_Discount_99", "t", "a")
  logit_rows <- c("Intercept", paste0("V_", 1:5), "Discount_25", "Discount_99")
  stopifnot(all(ddm_rows %in% rownames(d)), all(logit_rows %in% rownames(l)))
  rebuilt[j, coefficient_names] <- c(l[logit_rows, "mean"], d[ddm_rows, "mean"])
}
stopifnot(all(is.finite(as.matrix(rebuilt[coefficient_names]))))
write_output(rebuilt, "estimation", "posterior_means.csv")
parameters <- c("NRT_d", "Barrier_d", "discount_25_d", "discount_25_l", "discount_99_d", "discount_99_l")
for (item in c("McCafe_Cappuccino", "McCafe_Latte", "Peets_Cappuccino", "Peets_Latte", "Starbucks_Cappuccino", "Starbucks_Latte")) {
  parameters <- c(parameters, paste0(item, c("_d", "_l")))
}
table_W1 <- data.frame(parameter = parameters)
table_W1[c("mean", "sd", "min", "q25", "q75", "max")] <- t(vapply(parameters, function(name) {
  x <- estimates[[name]]
  c(mean(x), sd(x), min(x), quantile(x, .25, type = 7), quantile(x, .75, type = 7), max(x))
}, numeric(6)))
write_output(table_W1, "estimation", "table_W1.csv")
