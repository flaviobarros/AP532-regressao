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
## Usando o lm()
###########################################
## Sumário e tabela ANOVA
ads <- read.csv(file = 'Advertising.csv')
fit <- lm(data = ads, sales~TV)
summary(fit)
anova(fit)

## R² e coeficiente de determinação
cor(x = ads$sales, y = ads$TV)
cor(x = ads$sales, y = ads$TV)^2
