library(dplyr)
library(tidyr)
library(gt)
library(corrplot)
library(moments)
library(car)
library(boot)

file_path <- 'songs.csv'
df <- read.csv(file_path, header=TRUE)
str(df)


# ==================
# Summary Statistics
# ==================

df %>%
  summarise(across(c(popularity, tempo, loudness, duration_ms),
     list(Median = median, Mean = mean, SD = sd, Min = min, Max = max))) %>%
  pivot_longer(everything(), names_to = c("Variable", "Statistic"),
     names_pattern = "(.+)_(\\w+)$", values_to = "Value") %>%
  pivot_wider(names_from = Statistic, values_from = Value) %>%
  gt() %>%
  tab_header(title = "Fig. 1: Descriptive Statistics") %>%
  fmt_number(columns = c(Median, Mean, SD, Min, Max), decimals = 2)


# =========================
# Exploratory Data Analysis
# =========================

set.seed(42)
df_sample <- df[sample(nrow(df), 1000), ]

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))

hist(df$popularity, main = "Song Popularity Histogram", 
     xlab = "Score", ylab = "Frequency", breaks = 15)
plot(df_sample$tempo, df_sample$popularity, xlab='Tempo', ylab='Popularity', 
     main='Tempo vs. Popularity')
plot(df_sample$loudness, df_sample$popularity, xlab='Loudness', ylab='Popularity', 
     main='Loudness vs. Popularity')
plot(df_sample$duration_ms, df_sample$popularity, xlab='Duration', ylab='Popularity', 
     main='Duration vs. Popularity')

mtext("Fig. 1: Exploratory Data Analysis", outer = TRUE, cex = 1.25)
par(mfrow = c(1, 1))


# ==================
# Initial OLS Models
# ==================

df$mode <- as.factor(df$mode)
levels(df$mode)

df$genre <- as.factor(df$genre)
levels(df$genre)

model_1 <- lm(popularity ~ tempo + energy + loudness + duration_ms, data = df)
summary(model_1) # Model with audio characteristics only

model_2 <- lm(popularity ~ tempo + energy + avg_artist_popularity + loudness + duration_ms + mode + genre, data = df)
summary(model_2) # Model with added covariates

# ----- ANOVA for Model 1 and Model 2 ----- #
anova(model_1, model_2)

# ----- Correlation Matrix ----- #
X <- model.matrix(model_2)[, -1]
cor_matrix <- cor(X)
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.cex = 0.6, tl.srt = 45,
   title = "Fig. 2: Correlation Matrix of Predictor Variables", mar = c(0, 0, 2, 0))

# ----- Residual Analysis of Model #2 ----- #
stud_res <- rstudent(model_2)
y_hat <- fitted(model_2)

set.seed(42)
sample_idx <- sample(1:length(stud_res), 1000)

par(mfrow = c(2, 3), oma = c(0, 0, 2, 0))

qqnorm(stud_res, main = 'Quantile Plot')
qqline(stud_res, col='black')
hist(stud_res, main = 'Histogram',
     xlab = 'Studentized Deleted Residuals', breaks = 10)
plot(y_hat[sample_idx], stud_res[sample_idx], main = 'Scatter Plot',
     xlab = 'Fitted Values', ylab = 'Studentized Deleted Residuals',
     pch = 16, cex = 0.5)
abline(h=0, col='black', lty=2)
plot(df$tempo[sample_idx], stud_res[sample_idx], main = 'Scatter Plot',
     xlab = 'Tempo', ylab = 'Studentized Deleted Residuals',
     pch = 16, cex = 0.5)
abline(h=0, col='black', lty=2)
plot(df$loudness[sample_idx], stud_res[sample_idx], main = 'Scatter Plot',
     xlab = 'Loudness', ylab = 'Studentized Deleted Residuals',
     pch = 16, cex = 0.5)
abline(h=0, col='black', lty=2)
plot(df$duration_ms[sample_idx], stud_res[sample_idx], main = 'Scatter Plot',
     xlab = 'Duration', ylab = 'Studentized Deleted Residuals',
     pch = 16, cex = 0.5)
abline(h=0, col='black', lty=2)

mtext("Fig. 3: Residual Plots", outer = TRUE, cex = 1.25)
par(mfrow = c(1, 1))

# ----- Examining Normality ----- #
skewness(stud_res)
kurtosis(stud_res)

# ----- Box Cox Transformation ----- #
df$shifted_pop <- df$popularity + 1
shifted_model <- lm(shifted_pop ~ tempo + energy + avg_artist_popularity + loudness + duration_ms + mode + genre, 
  data = df)

result <- boxCox(shifted_model, main = "Fig. 4: Box-Cox Transformation", font.main = 1)
lambda <- result$x[which.max(result$y)]
lambda


# =================
# Fourth Root Model
# =================

df$root_pop <- df$popularity^0.25

root_model <- lm(root_pop ~ tempo + energy + avg_artist_popularity + loudness + duration_ms + mode + genre, 
  data = df)
summary(root_model)

# ----- Residual Analysis of Root Model ----- #
root_stud_res <- rstudent(root_model)
root_y_hat <- fitted(root_model)

set.seed(42)
sample_idx <- sample(1:length(root_stud_res), 1000)

par(mfrow = c(2, 3), oma = c(0, 0, 2, 0))

qqnorm(root_stud_res, main = 'Quantile Plot')
qqline(root_stud_res, col='black')
hist(root_stud_res, main = 'Histogram',
     xlab = 'Studentized Deleted Residuals', breaks = 10)
plot(root_y_hat[sample_idx], root_stud_res[sample_idx], main = 'Scatter Plot',
     xlab = 'Fitted Values', ylab = 'Studentized Deleted Residuals',
     pch = 16, cex = 0.5)
abline(h=0, col='black', lty=2)
plot(df$tempo[sample_idx], root_stud_res[sample_idx], main = 'Scatter Plot',
     xlab = 'Tempo', ylab = 'Studentized Deleted Residuals',
     pch = 16, cex = 0.5)
abline(h=0, col='black', lty=2)
plot(df$loudness[sample_idx], root_stud_res[sample_idx], main = 'Scatter Plot',
     xlab = 'Loudness', ylab = 'Studentized Deleted Residuals',
     pch = 16, cex = 0.5)
abline(h=0, col='black', lty=2)
plot(df$duration_ms[sample_idx], root_stud_res[sample_idx], main = 'Scatter Plot',
     xlab = 'Duration', ylab = 'Studentized Deleted Residuals',
     pch = 16, cex = 0.5)
abline(h=0, col='black', lty=2)

mtext("Fig. 5: Residual Plots", outer = TRUE, cex = 1.25)
par(mfrow = c(1, 1))

# ----- Examining Normality ----- #
skewness(root_stud_res)
kurtosis(root_stud_res)


# =======================================
# Generalized Linear Model: Quasi-Poisson
# =======================================
 
df$log_duration <- log(df$duration_ms)

qp_model <- glm(popularity ~ tempo + energy + avg_artist_popularity + loudness + log_duration + mode + genre, 
  family = quasipoisson(link = "log"), data = df)
summary(qp_model)

# ----- Calculating Pseudo R-Squared ----- #
null_model <- glm(popularity ~ 1, family = quasipoisson(link = "log"), data = df)
pseudo_r2 <- 1 - (deviance(qp_model) / deviance(null_model))
pseudo_r2

# ----- Interpreting Coefficients ----- #
exp(coef(qp_model))

# ----- Residual Analysis of Quasi-Poisson Model ----- #
qp_stud_res <- rstudent(qp_model)
qp_y_hat <- fitted(qp_model)

set.seed(42)
sample_idx <- sample(1:length(qp_stud_res), 1000)

par(mfrow = c(2, 3), oma = c(0, 0, 2, 0))

qqnorm(qp_stud_res, main = 'Quantile Plot')
qqline(qp_stud_res, col='black')
hist(qp_stud_res, main = 'Histogram',
     xlab = 'Studentized Deleted Residuals', breaks = 10)
plot(qp_y_hat[sample_idx], qp_stud_res[sample_idx], main = 'Scatter Plot',
     xlab = 'Fitted Values', ylab = 'Studentized Deleted Residuals',
     pch = 16, cex = 0.5)
abline(h=0, col='black', lty=2)
plot(df$tempo[sample_idx], qp_stud_res[sample_idx], main = 'Scatter Plot',
     xlab = 'Tempo', ylab = 'Studentized Deleted Residuals',
     pch = 16, cex = 0.5)
abline(h=0, col='black', lty=2)
plot(df$loudness[sample_idx], qp_stud_res[sample_idx], main = 'Scatter Plot',
     xlab = 'Loudness', ylab = 'Studentized Deleted Residuals',
     pch = 16, cex = 0.5)
abline(h=0, col='black', lty=2)
plot(df$log_duration[sample_idx], qp_stud_res[sample_idx], main = 'Scatter Plot',
     xlab = 'Log of Duration', ylab = 'Studentized Deleted Residuals',
     pch = 16, cex = 0.5)
abline(h=0, col='black', lty=2)

mtext("Fig. 6: Residual Plots", outer = TRUE, cex = 1.25)
par(mfrow = c(1, 1))

# ----- Examining Normality ----- #
skewness(qp_stud_res)
kurtosis(qp_stud_res)

# ----- Identifying Influential Cases ----- #
cooks_d <- cooks.distance(qp_model)

n <- nrow(df)
threshold <- 4 / n
influential <- which(cooks_d > threshold)

plot(cooks_d, type = "h", main = "Fig. 7: Cook's Distance",
     ylab = "Cook's Distance", xlab = "Observation Index", font.main = 1)
abline(h = 4/n, col = "red", lty = 2)

print(paste("Number of influential observations:", length(influential)))
print(paste("Threshold:", round(threshold, 4)))
print(paste("Max Cook's Distance:", round(max(cooks_d), 4)))


# ================
# Cross Validation
# ================

# Cost Function
mae <- function(y, yhat) mean(abs(y - yhat))

# OLS Model
ols_model <- glm(popularity ~ tempo + energy + avg_artist_popularity + loudness + duration_ms + mode + genre,
  family = gaussian, data = df)
cv_ols <- cv.glm(df, ols_model, cost = mae, K = 5)

# Fourth Root Model
mae_root <- function(y, yhat) mean(abs(y - yhat^4))
root_model <- glm(root_pop ~ tempo + energy + avg_artist_popularity + loudness + duration_ms + mode + genre,
  family = gaussian, data = df)
cv_root <- cv.glm(df, root_model, cost = mae_root, K = 5)

# Quasi-Poisson Model
qp_model <- glm(popularity ~ tempo + energy + avg_artist_popularity + loudness + log_duration + mode + genre,
  family = quasipoisson(link = "log"), data = df)
cv_qp <- cv.glm(df, qp_model, cost = mae, K = 5)

# Comparison
cv_ols$delta[1]
cv_root$delta[1]
cv_qp$delta[1]