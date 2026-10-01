source("../../R/diffmap.R")

dm_metric <- "spearman"
dm_k_min <- 10L
dm_variables <- 10L

x <- as.matrix(read.csv("random_matrix_20x10_rowsum1.csv"))

DM <- diffmap(
  x,
  metric = dm_metric,
  k_min = dm_k_min,
  dm_variables = dm_variables
)

dm_scores <- as.data.frame(DM$DM)
names(dm_scores) <- paste0("DM", seq_len(ncol(dm_scores)))
write.csv(dm_scores, "random_matrix_dm.csv", row.names = FALSE)

plot(DM$DM[, 1], DM$DM[, 2],
     pch = 19,
     col = viridisLite::turbo(100)[cut(DM$DM[, 3], 100, labels = FALSE)],
     xlab = "DM1", ylab = "DM2")
