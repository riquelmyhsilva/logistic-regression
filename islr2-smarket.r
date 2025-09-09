# ISLR2 :: Smarket

install.packages("ISLR")
install.packages("ISLR2")
install.packages("caret")
install.packages("pROC")

library("ISLR")
library("ISLR2")
library("caret")
library("pROC")

df_smarket2 <- ISLR2::Smarket
head(df_smarket)

smarket2_glm <- glm(Direction ~ . -Today, data = df_smarket2, family = binomial)
formula(smarket2_glm)
summary(smarket2_glm)

# Linear Regression

set.seed(123)
particao <- sample(seq_len(nrow(df_smarket2)), size = 0.7 * nrow (df_smarket2))
df_smarket2_ml <- df_smarket2[particao, ]
df_smarket2_test <- df_smarket2[-particao, ]
prop.table(table(df_smarket2_ml$Direction));
prop.table(table(df_smarket2_test$Direction))

pairs(df_smarket2, col=df_smarket2$Direction)

ggplot(df_smarket2, aes(Lag1, fill = Direction)) +
  geom_histogram(bins = 30, alpha = .8, position = "identity") +
  labs(title = "Distribuição de Direction por Today", x = "Direction", y = "Contagem")

# Prediction

pred_smarket2_glm <- predict(smarket2_glm, newdata = df_smarket2_test, type = "response")
summary(pred_smarket2_glm)

answer_smarket2_glm <- ifelse(pred_smarket2_glm < 0.5, "Down", "Up")
prop.table(table(answer_smarket2_glm==df_smarket2_test$Direction))
