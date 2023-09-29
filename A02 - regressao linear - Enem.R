# Aula 02 - Regressão linear

# 0. Setup ---------------------------------------------------------------------

# Remove notação científica
options(scipen = 999)

# Limpando a memória
rm(list=ls()) # Remove all objects - remove objetos
gc()          # Garbage Clean - limpeza de lixos

# Caminho simplificado para carregar pacotes (PACMAN)
#install.packages(pacman)
library(pacman)
p_load(tidyverse,ggplot2,rio,
       equatiomatic,kableExtra,stargazer)

# 1. Importar base de dados ----------------------------------------------------

# Saeb 2019
mydata <- import("0_data/ENEM/mydata_enem_2021_sp.csv")

# 2. Manuseio dos dados --------------------------------------------------------

# Já fizemos

mydata <- mutate(.data = mydata,
                 educMae_sd = scale(educMae),
                 rendFamP_sd = scale(rendFamP),
                 depAdmin2 = case_when(depAdmin == "Pública" ~ 0,
                                       depAdmin == "Privada" ~ 1) )

# Variável dependente: nota em matemática

# Variáveis independentes:
# 1. escolaridade da mãe
# 2. renda familiar per capta
# 3. tipo de escola (dep Admin)

# 3.1 Análises: Parte exploratória -------------------------------

# Como se comportam nossas variáveis?

# Histograma da renda familiar per capta
mydata %>%
  ggplot(aes(x = media)) +
  geom_histogram(bins = 50)

# Histograma da renda familiar per capta
mydata %>%
  ggplot(aes(x = rendFamP)) +
  geom_histogram()

# Histograma da escolaridade da mãe
mydata %>%
  ggplot(aes(x = educMae))+
  geom_histogram()

# 3.2 Análises: regressão ------------------------------------------------------

# Modelos lineares - Linear Models (LM) ----------------------------------------

mydata <- mydata %>% select(media,educMae,rendFamP,depAdmin,
                            educMae_sd,rendFamP_sd,depAdmin2) %>% na.exclude()

reg = lm(data = mydata,media ~ educMae + rendFamP + depAdmin2)

reg
summary(reg)

reg_sd = lm(data = mydata,media ~ educMae_sd + rendFamP_sd + depAdmin2)

reg_sd
summary(reg_sd)

# Outras formas de exportar resultados
#pacote 'stargazer'
stargazer(reg_sd, nobs = T, type = "text")

# Manipulando o objeto de regressão --------------------------------------------

# Podemos extrair informações de interesse:

# - Coeficientes
beta = coef(reg)
beta %>% round(2)

# Valores observados
yobs = mydata$media

# - Valores preditos pela regressão
yhat = predict(reg)

# - Resíduos
# Os resíduos são a diferença entre valor observado e valor predito pela reg
e = yobs - yhat
head(e)

e = residuals(reg)
head(e)

# NA REAL, temos tudo no objeto.

# Qual é o modelo usado para o cálculo da regressão?

modelo <- reg$model
view(modelo)

# Quais foram os valores preditos face os observados?

modelo$preditos <- reg$fitted.values

# Quais são os erros do modelo?

modelo$erro <- reg$residuals

# Temos tudo no nosso modelo para reproduzir os dados observados
intercepto = reg$coefficients[1]
b1 = reg$coefficients[2]
b2 = reg$coefficients[3]
b3 = reg$coefficients[4]

observados = intercepto + b1*modelo$educMae + b2*modelo$rendFamP + b3*modelo$depAdmin2 + e

# Vamos conferir
modelo$obs <- observados

modelo <- relocate(.data = modelo,
                   media,obs,preditos,erro,
                   .after = depAdmin2)

# É ou não é legal isso daqui? 
# <3 == coração == s2

# Estatística R2 ou coeficiente de determinação
cor_modelo <- cor(modelo$preditos,modelo$media)
cor_modelo

cor_modelo^2

# Podemos calcular "na mão"
n = nrow(modelo)
n

# - Variância total da variável de interesse
var_y <- var(yobs)
var_y

# - Variância dos resíduos (ou Mean Square Error - MSE)
var_e <- var(e)
var_e

# Essa é a fórmula da variância
sum(e^2)/(n-1)

# Estatística R2
1 - var_e/var_y

# Interpretação: parte da variância que é explicada pelo modelo. Isto é,
# diferença entre a variância total e a parte não explicada (erro)


# Análises gráficas ------------------------------------------------------------

# Valores observados e valores preditos
plt1 <- ggplot()+
        geom_point(aes(y=yobs,x=yhat))
plt1

# O que seria esperado e nosso modelo explicasse tudo?

plt2 <- plt1 +
        geom_abline(slope = 1, 
                    intercept = 0,
                    lty=2,col="red") # Linha a 45 graus
plt2

plt2+
  theme_bw()+
  scale_y_continuous(limits = c(0,1000))+
  scale_x_continuous(limits = c(0,1000))

# Como se comportam os erros?
hist(e)

# Distribuição aderente à curva normal
x=scale(e)
hist(x, breaks=50, probability=TRUE, col="lightblue",
     main="Renda Familiar per capta",ylab="Densidade",
     ylim=c(0,0.75),xlim=c(-4,4))
curve(dnorm(x),add=T)