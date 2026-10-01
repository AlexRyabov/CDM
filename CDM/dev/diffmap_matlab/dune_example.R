source("../../R/diffmap.R")

data(dune, package = "vegan")
data(dune.env, package = "vegan")

x <- as.matrix(dune)
management <- dune.env$Management

DM <- diffmap(
  x,
  metric = "spearman",
  k_min = 10L,
  dm_variables = 3L
)

plot(
  DM$DM[, 1],
  DM$DM[, 2],
  col = as.integer(management),
  pch = 19,
  xlab = "DM1",
  ylab = "DM2"
)

legend(
  "topright",
  legend = levels(management),
  col = seq_along(levels(management)),
  pch = 19
)
