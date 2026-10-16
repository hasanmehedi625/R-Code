# ===================== PCA on USArrests =====================

# View the data
states <- row.names(USArrests)
states
names(USArrests)

# Check means and variances before scaling
apply(USArrests, 2, mean)
apply(USArrests, 2, var)

# Run PCA with scaling (mean = 0, sd = 1)
pr.out <- prcomp(USArrests, scale = TRUE)

# Inspect output
names(pr.out)
pr.out$center      # means used for scaling
pr.out$scale       # sds used for scaling
pr.out$rotation    # principal component loadings
dim(pr.out$x)      # PC scores matrix (50 states x 4 PCs)

# Biplot of first two principal components
biplot(pr.out, scale = 0)

# Flip signs for a cleaner plot (PCs are unique only up to sign)
pr.out$rotation <- -pr.out$rotation
pr.out$x <- -pr.out$x
biplot(pr.out, scale = 0)

# Variance explained
pr.out$sdev
pr.var <- pr.out$sdev^2
pve <- pr.var / sum(pr.var)
pve

# Scree plot + cumulative variance explained
par(mfrow = c(1, 2))
plot(pve, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")
plot(cumsum(pve), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")

