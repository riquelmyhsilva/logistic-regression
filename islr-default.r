# ISLR :: Default

install.packages("ISLR")
install.packages("ISLR2")
install.packages("caret")
install.packages("pROC")

library("ISLR")
library("ISLR2")
library("caret")
library("pROC")

df_default <- ISLR::Default
head(df_default)

default_glm <- glm(student ~ . , data = df_default, family = binomial)
formula(default_glm)
summary(default_glm)

# Linear Regression

set.seed(123)
particao <- sample(seq_len(nrow(df_default)), size = 0.7 * nrow (df_default))
df_default_ml <- df_default[particao, ]
df_default_test <- df_default[-particao, ]
prop.table(table(df_default_ml$student));
prop.table(table(df_default_test$student))

pairs(df_default, col=df_default$student)

ggplot(df_default, aes(income, fill = student)) +
  geom_histogram(bins = 30, alpha = .8, position = "identity") +
  labs(title = "Distribuição de student por income", x = "student", y = "Contagem")

# Predicition

pred_default_glm <- predict(default_glm, newdata = df_default_test, type = "response")
summary(pred_default_glm)

answer_default_glm <- ifelse(pred_default_glm < 0.5, "Yes", "No")
prop.table(table(answer_default_glm==df_default_test$student))
