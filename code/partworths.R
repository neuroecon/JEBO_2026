run_aggregate <- function() {
  pdf(file.path(output_root, "partworths", "aggregate_partworths.pdf"), width = 9, height = 6)
  on.exit(dev.off())
  e <- new.env(parent = globalenv())
  suppressMessages(sys.source(file.path(root, "code", "aggregate.R"), envir = e))
  e
}
aggregate_result <- run_aggregate()
aggregate_partworths <- data.frame(
  attribute = colnames(attribute_partworths(estimates, "l")),
  Logit = c(aggregate_result$McCafe_l, aggregate_result$Peets_l, aggregate_result$Starbucks_l,
            aggregate_result$Cappuccino_l, aggregate_result$Latte_l, aggregate_result$discount_0_l,
            aggregate_result$discount_25_l, aggregate_result$discount_99_l),
  DDM = c(aggregate_result$McCafe_d, aggregate_result$Peets_d, aggregate_result$Starbucks_d,
          aggregate_result$Cappuccino_d, aggregate_result$Latte_d, aggregate_result$discount_0_d,
          aggregate_result$discount_25_d, aggregate_result$discount_99_d))
write_output(aggregate_partworths, "partworths", "aggregate_partworths.csv")
brand_differences <- subset(aggregate_partworths, attribute %in% c("Peets", "Starbucks"))
brand_differences$percentage_difference <- with(brand_differences, 100 * (Logit - DDM) / Logit)
write_output(brand_differences, "partworths", "brand_percentage_differences.csv")
individual <- data.frame(ID = estimates$ID,
                         setNames(as.data.frame(attribute_partworths(estimates, "l")),
                                  paste0(aggregate_partworths$attribute, "_l")),
                         setNames(as.data.frame(attribute_partworths(estimates, "d")),
                                  paste0(aggregate_partworths$attribute, "_d")))
write_output(individual, "partworths", "individual_partworths.csv")

red <- adjustcolor("red", alpha.f = .5)
blue <- adjustcolor("blue", alpha.f = .7)
limits <- list(Peets = c(-11, 15), discount_25 = c(-5, 10),
               Starbucks = c(-6, 13), Latte = c(-7, 12), discount_99 = c(-1, 15))
scatter <- function(attribute, title, tag) {
  par(cex = 1.1, mar = c(4.5, 4.3, 3.4, .8), font.main = 1)
  plot(individual[[paste0(attribute, "_d")]], individual[[paste0(attribute, "_l")]],
       xlim = limits[[attribute]], ylim = limits[[attribute]],
       pch = 16, col = red, cex = 1.15,
       xlab = "DDM Part-worth", ylab = "Logit Part-worth", main = title)
  abline(0, 1)
  mtext(tag, side = 3, adj = -.16, line = 1, font = 2, cex = 1.8)
}
aggregate_panel <- function() {
  par(cex = 1.1, mar = c(4.5, 4.5, 6.5, .8), font.main = 1)
  x <- c(.5, 1, 1.5, 2.5, 3.5, 4.5, 5, 5.5)
  plot(NA, xlim = c(0, 6), ylim = c(-2, 6), xaxt = "n", xlab = "Attribute",
       ylab = "Part-worth Estimates", main = "Estimates Averaged Over All Subjects")
  for (idx in list(1:3, 4:5, 6:8)) {
    lines(x[idx], aggregate_partworths$Logit[idx], col = red, lwd = 1.3)
    points(x[idx], aggregate_partworths$Logit[idx], col = red, pch = 18, cex = 1.25)
    lines(x[idx] - .02, aggregate_partworths$DDM[idx], col = blue, lwd = 1.3)
    points(x[idx] - .02, aggregate_partworths$DDM[idx], col = blue, pch = 16, cex = 1.1)
  }
  text(x, -1.8, c("McCafe", "Peets", "Starbucks", "Cappuccino", "Latte", "0c", "25c", "99c"), cex = .73)
  text(c(1, 3, 5), 5.6, c("Brand", "Flavor", "Discount"))
  legend(.10, 5.0, legend = c("Logit", "DDM"), pch = c(18, 16), col = c(red, blue))
  mtext("a", side = 3, adj = -.10, line = 5, font = 2, cex = 1.8)
}
draw_w9 <- function() {
  layout(matrix(c(1, 2, 1, 3), nrow = 2, byrow = TRUE), widths = c(2.2, 1))
  aggregate_panel(); scatter("Peets", "Peets", "b"); scatter("discount_25", "25c Discount", "c")
}
draw_w10 <- function() {
  par(mfrow = c(1, 3))
  scatter("Starbucks", "Starbucks", "a"); scatter("Latte", "Latte", "b")
  scatter("discount_99", "99c Discount", "c")
}
for (f in c("W9", "W10")) {
  height <- if (f == "W9") 7 else 3.8
  pdf(file.path(output_root, "partworths", paste0("figure_", f, ".pdf")), width = 12, height = height)
  if (f == "W9") draw_w9() else draw_w10()
  dev.off()
  png(file.path(output_root, "partworths", paste0("figure_", f, ".png")), width = 1800,
      height = round(height * 150), res = 150)
  if (f == "W9") draw_w9() else draw_w10()
  dev.off()
}
