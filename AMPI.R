library(ggplot2)

# load the data
data <- read.csv("synthetic_data.csv")

# check correlation of the data
cor(data)
summary(data)

# plot the overlapping histograms
dat_long <- reshape2::melt(data)
ggplot(dat_long, aes(x = value, fill = variable)) +
  geom_histogram(alpha = 0.4, position = "identity", bins = 30) +
  labs(title = "Overlapping Histograms",
       x = "Value",
       y = "Count") +
  theme_minimal()

# Normalization unsing constrained min-max, the goalpost is the mean
AMPI_norm <- function(x) {
  70+(60 * ((x - mean(x, na.rm = TRUE)) / 
        (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))))
}

# Apply to your dataset
dat_std <- as.data.frame(apply(data, 2, AMPI_norm))

# check correlation of the data
cor(dat_std)
summary(dat_std)

# plot the overlapping histograms
dat_long <- reshape2::melt(dat_std)
ggplot(dat_long, aes(x = value, fill = variable)) +
  geom_histogram(alpha = 0.4, position = "identity", bins = 30) +
  labs(title = "Overlapping Histograms",
       x = "Value",
       y = "Count") +
  theme_minimal()

# comparing the coefficients of variation
sapply(data, function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE))
sapply(dat_std, function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE))

# aggregation using the following formula: AMPI= mean(x1,x2,x3) + *sd(x1,x2,x3)*cv(x1,x2,x3)
dat_std$AMPI <- rowMeans(dat_std) + (apply(dat_std, 1, sd) * (apply(dat_std, 1, function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE))))
summary(dat_std$AMPI)

# compare the r^2 of the regression of AMPI on x1, x2, x3 after normalization
modX1 <- lm(AMPI ~ x1, data = dat_std)
modX2 <- lm(AMPI ~ x2, data = dat_std)
modX3 <- lm(AMPI ~ x3, data = dat_std)
summary(modX1) #0.37
summary(modX2) #0.48
summary(modX3) #0.42
