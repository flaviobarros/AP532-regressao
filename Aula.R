## Carregando os pacotes necessários

## Lendo o conjunto de dados
data("mtcars")

## Criando um modelo simples
fit <- lm(data=mtcars, mpg~drat)

###########################################
## DESCIDA DO GRADIENTE
###########################################
x <- mtcars$drat
y <- mtcars$mpg
n <- nrow(ads)
beta0 <- 1
beta1 <- 1
alpha <- 0.1
delta <- 1
yhat <- beta0 + beta1*x
MSE <- sum((y - yhat) ^ 2) / nrow(ads)
cost_history <- double()
theta_history <- list()
count <- 1

while (delta >= 1e-11) {
  
  beta0_new <- beta0 - alpha * (1/n) * ((sum(yhat - y)))
  beta1_new <- beta1 - alpha * (1/n) * ((sum((yhat - y)*x)))
  beta0 <- beta0_new
  beta1 <- beta1_new
  yhat <- beta0 + beta1*x
  MSE_new <- sum((y - yhat) ^ 2) / nrow(ads)
  delta <- abs(MSE - MSE_new)
  print(paste('delta = ', delta))
  print(paste('Beta0 = ', beta0))
  print(paste('Beta1 = ', beta1))
  MSE <- MSE_new
  theta <- c(beta0, beta1)
  theta_history[[count]] <- theta
  cost_history[count] <- MSE
  count <- count + 1
}

## Plotando os modelos
plot(x,y, col=rgb(0.2,0.4,0.6,0.4), main='Linear regression by gradient descent')
for (i in c(1,3,6,10,14,seq(20,1000,by=100))) {
  abline(coef=theta_history[[i]], col=rgb(0.8,0,0,0.3))
}
abline(coef=theta, col='blue')

## Plotando o custo
plot(cost_history[1:100], type='line', col='blue', lwd=2, main='Cost function', ylab='cost', xlab='Iterations')

###########################################
## Usando o lm() - Regressão Linear Simples
###########################################
## Sumário e tabela ANOVA
ads <- read.csv(file = 'Advertising.csv')
fit <- lm(data = ads, sales~TV)
summary(fit)
anova(fit)

## R² e coeficiente de determinação
cor(x = ads$sales, y = ads$TV)
cor(x = ads$sales, y = ads$TV)^2

## Verifique os valores da estatística t para os betas e da estatística F

###########################################
## Usando o lm() - Regressão Múltipla
###########################################
n = 100; x = rnorm(n); x2 = rnorm(n); x3 = rnorm(n)
## Gerando dados
y = 1 + x + x2 + x3 + rnorm(n, sd = .1)

## Obtendo os resíduos tendo removido X2 e X3 de X1 e Y
ey = resid(lm(y ~ x2 + x3))
ex = resid(lm(x ~ x2 + x3))

## Regressão pela origem com os resíduos
coef(lm(ey ~ ex - 1))
ex

## Fit the full linear model to show that it agrees
coef(lm(y ~ x + x2 + x3))

###########################################
## Diagnóstico
###########################################

## Gerando dados
n <- 100; x <- c(10, rnorm(n)); y <- c(10, c(rnorm(n)))

## Plotando
plot(x, y, frame = FALSE, cex = 2, pch = 21, bg = "lightblue", col = "black")
abline(lm(y ~ x))

## Vendo o impacto nos betas de cada medida
fit <- lm(y ~ x)
dfbetas(fit)
round(dfbetas(fit)[1 : 10, 2], 3)

## Vamos olhar o leverage
round(hatvalues(fit)[1 : 10], 3)

###########################################
n = 100; x = rnorm(n); 
## Gerando dados
y = 1 + x + rnorm(n, sd = .5)
y = c(11, y)
x = c(10, x)
plot(x, y, frame = FALSE, cex = 2, pch = 21, bg = "lightblue", col = "black")
fit <- lm(y~x)
abline(fit)

## Impacto nos betas
round(dfbetas(fit)[1 : 10, 1], 3)

## Vamos olhar o leverage
round(hatvalues(fit)[1 : 10], 3)

###########################################
## Importância de olhar o resíduo
###########################################
dat <- read.table('eistein.txt', header = FALSE)
pairs(dat)

## Vamos ver um modelo
summary(lm(V1 ~ . -1, data = dat))

## Agora vamos explorar os resíduos
fit <- lm(V1 ~ . -1, data = dat)
plot(predict(fit), resid(fit), pch = '.')

###########################################
## Exercícios
###########################################
## Utilizando o seguinte dataset
data("Seatbelts")
seatbelts <- as.data.frame(Seatbelts)

## Ajuste um modelo linear com as variáveis kms, PetrolPrice e law

## Estime a variância residual pela função resid e compare
## com a estimativa fornecida pelo R

## Faça uma análise de diagnsótico

###########################################
## Analisando as correlações
###########################################
## Analisando as correlações
M <- cor(mtcars, use = 'complete.obs')
corrplot(M, method='circle')
summary(M[upper.tri(M)])

## por que é importante?
