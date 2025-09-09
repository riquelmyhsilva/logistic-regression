# ISLR2 :: Weekly

install.packages("ISLR")
install.packages("ISLR2")
install.packages("caret")
install.packages("pROC")

library("ISLR")
library("ISLR2")
library("caret")
library("pROC")

df_weekly <- ISLR2::Weekly
head(df_weekly)

weekly_glm <- glm(Direction ~ . -Today, data = df_weekly, family = binomial)
formula(weekly_glm)
summary(weekly_glm)

# Linear Regression

set.seed(123)
particao <- sample(seq_len(nrow(df_weekly)), size = 0.7 * nrow (df_weekly))
df_weekly_ml <- df_weekly[particao, ]
df_weekly_test <- df_weekly[-particao, ]
prop.table(table(df_weekly_ml$Direction));
prop.table(table(df_weekly_test$Direction))

pairs(df_weekly, col=df_weekly$Direction)

ggplot(df_weekly, aes(Lag1, fill = Direction)) +
  geom_histogram(bins = 30, alpha = .8, position = "identity") +
  labs(title = "Distribuição de Direction por Today", x = "Direction", y = "Contagem")

# Prediction

pred_weekly_glm <- predict(weekly_glm, newdata = df_weekly_test, type = "response")
summary(pred_weekly_glm)

answer_weekly_glm <- ifelse(pred_weekly_glm < 0.5, "Down", "Up")
prop.table(table(answer_weekly_glm==df_weekly_test$Direction))
