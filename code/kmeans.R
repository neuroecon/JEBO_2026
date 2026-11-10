table_W2 <- data.frame(attribute = c(item_names, "N"))
assignments <- data.frame(ID = estimates$ID)
for (model in c("l", "d")) {
  x <- as.matrix(estimates[paste0(item_names, "_", model)])
  if (model == "d") x <- x * estimates$Barrier_d
  colnames(x) <- item_names
  set.seed(1)
  fit <- kmeans(x, centers = 2, iter.max = 100, nstart = 1000, algorithm = "Hartigan-Wong")
  price <- which.max(fit$centers[, "discount_99"])
  brand <- setdiff(1:2, price)
  label <- if (model == "l") "Logit" else "DDM"
  table_W2[[paste0(label, "_price_sensitive")]] <- c(fit$centers[price, ], fit$size[price])
  table_W2[[paste0(label, "_brand_loyal")]] <- c(fit$centers[brand, ], fit$size[brand])
  assignments[[label]] <- ifelse(fit$cluster == price, "price_sensitive", "brand_loyal")
  stopifnot(fit$ifault == 0L, sum(fit$size) == 59L)
  write_output(data.frame(ID = estimates$ID, x), "kmeans", paste0(label, "_clustering_input.csv"))
}
write_output(table_W2, "kmeans", "table_W2.csv")
write_output(assignments, "kmeans", "segment_assignments.csv")
