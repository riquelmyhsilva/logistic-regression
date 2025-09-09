# ISLR2 :: Caravan

install.packages("ISLR")
install.packages("ISLR2")
install.packages("caret")
install.packages("pROC")

library("ISLR")
library("ISLR2")
library("caret")
library("pROC")

df_caravan <- ISLR2::Caravan
head(df_caravan)

caravan_glm <- glm(Purchase ~ . , data = df_caravan, family = binomial)
summary(caravan_glm)

# Linear Regression

set.seed(123)
particao <- sample(seq_len(nrow(df_caravan)), size = 0.7 * nrow (df_caravan))
df_caravan_ml <- df_caravan[particao, ]
df_caravan_test <- df_caravan[-particao, ]
prop.table(table(df_caravan_ml$Purchase));
prop.table(table(df_caravan_test$Purchase))

subset_cols <- c("Purchase", "MOSTYPE", "MAANTHUI", "MGEMOMV", "MGEMLEEF")
pairs(df_caravan[, subset_cols], col = df_caravan$Purchase)

ggplot(df_caravan, aes(MGEMLEEF, fill = Purchase)) +
  geom_histogram(bins = 30, alpha = .8, position = "identity") +
  labs(title = "Distribuição de Purchase por Today", x = "Purchase", y = "Contagem")

# Prediciton

pred_caravan_glm <- predict(caravan_glm, newdata = df_caravan_test, type = "response")
summary(pred_caravan_glm)

answer_caravan_glm <- ifelse(pred_caravan_glm < 0.5, "No", "Yes")
prop.table(table(answer_caravan_glm==df_caravan_test$Purchase))
