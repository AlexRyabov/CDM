set.seed(1)

latent <- seq(-2, 2, length.out = 20)
loadings <- seq(0.4, 1.6, length.out = 10)

x <- outer(latent, loadings, function(z, a) exp(a * z))
x <- x + matrix(rlnorm(20 * 10, meanlog = -2, sdlog = 0.35), nrow = 20)
x <- x / rowSums(x)

write.csv(x, "random_matrix_20x10_rowsum1.csv", row.names = FALSE)
