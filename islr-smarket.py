# ISLR :: Smarket

df_smarket <- ISLR::Smarket
names(df_smarket)

smarket_glm <- glm(Direction ~ . -Today, data = df_smarket, family = binomial)
formula(smarket_glm)
summary(smarket_glm)

## Linear Regression

set.seed(123)
particao <- sample(seq_len(nrow(df_smarket)), size = 0.7 * nrow (df_smarket))
df_smarket_ml <- df_smarket[particao, ]
df_smarket_test <- df_smarket[-particao, ]
prop.table(table(df_smarket_ml$Direction));
prop.table(table(df_smarket_test$Direction))

pairs(df_smarket, col=df_smarket$Direction)

ggplot(df_smarket, aes(Lag4, fill = Direction)) +
  geom_histogram(bins = 30, alpha = .8, position = "identity") +
  labs(title = "Distribuição de Direction por Today", x = "Direction", y = "Contagem")

## Predicition

pred_smarket_glm <- predict(smarket_glm, newdata = df_smarket_test, type = "response")
summary(pred_smarket_glm)

answer_smarket_glm <- ifelse(pred_smarket_glm < 0.5, "Down", "Up")
prop.table(table(answer_smarket_glm==df_smarket_test$Direction))
