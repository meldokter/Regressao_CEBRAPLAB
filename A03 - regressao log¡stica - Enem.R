# A03 - Regressão logística

# Variável dependente (a ser explicada): binária com valores 0 e 1

# 1. Setup ---------------------------------------------------------------------

# Remove notação científica
options(scipen = 999)

# Limpando a memória
rm(list=ls()) # Remove all objects - remove objetos
gc()          # Garbage Clean - limpeza de lixos

# Caminho simplificado para carregar pacotes (PACMAN)
#install.packages(pacman)
library(pacman)
p_load(tidyverse,ggplot2,plotly,rio,
       pROC,ROCR,
       equatiomatic,kableExtra,stargazer)

# 2. Importar base de dados ----------------------------------------------------

# Saeb 2019
mydata <- import("0_data/ENEM/mydata_enem_2021_sp.csv")

# 3. Manuseio dos dados --------------------------------------------------------

# Já fizemos

mydata <- mutate(.data = mydata,
                 
                 media = (NU_NOTA_MT+NU_NOTA_LC+NU_NOTA_CH+NU_NOTA_CN+NU_NOTA_REDACAO)/5,
                 
                 educMae_sd = scale(educMae),
                 rendFamP_sd = scale(rendFamP),
                 depAdmin2 = case_when(depAdmin == "Pública" ~ 0,
                                       depAdmin == "Privada" ~ 1),
                 faixas_rendFamP = case_when(
                   rendFamP < 1500 ~ "1. Baixa",
                   rendFamP >= 1500 & rendFamP < 5000 ~ "2. Média",
                   rendFamP > 5000 ~ "3. Alta"
                 )
)

# Evento: nota para ingresso ser maior que o mínimo necessário
# para ingresso na USP (650)

corte = 700

mydata$NPI <- ifelse(mydata$media > corte,1,0)

# Variável dependente: Nota Para Ingresso (NPI)
# 1 = "Sim"
# 0 = "Não"

# Variáveis independentes:
# 1. escolaridade da mãe
# 2. renda familiar per capta
# 3. tipo de escola (dep Admin)

# Análises descritivas (I): Gráficos Univariados -------------------------------

# Como se comportam nossas variáveis?

# Histograma da renda familiar per capta
mydata %>%
  ggplot(aes(x = media)) +
  theme_bw()+
  geom_histogram(bins = 50,fill="steelblue",alpha=.7)+
  geom_vline(xintercept = corte,lty=2,size=.7,color="red")

# Quantidade de 0 e 1
mydata %>% mutate(NPI = factor(NPI)) %>% drop_na(NPI) %>% 
  ggplot(aes(x = NPI,fill=NPI)) +
  theme_bw()+
  geom_bar()+
  labs(fill="")

# Análises descritivas (II): Gráficos Bivariados -------------------------------

# Regressao --------------------------------------------------------------------

mydata.2 <- mydata %>% select(NPI,educMae,rendFamP,depAdmin,
                            educMae_sd,rendFamP_sd,
                            depAdmin2,faixas_rendFamP) %>% na.exclude()

# Generalized Linear Models (GLM): função para regressão logística

reg <- glm(formula = NPI ~ educMae + rendFamP + depAdmin2, 
                      data = mydata.2, 
                      family = "binomial")
summary(reg)

# Outras maneiras de apresentar os outputs do modelo
# Função 'stargazer' do pacote 'stargazer'

stargazer(reg, nobs = T, type = "text") # mostra o valor de Log-Likelihood

# Distribuições de probabilidade
hist(reg$fitted.values)

# Por que, em geral, a probabilidade predita é baixa? 
# [lembrar discriminação da nota de corte]

mydata.2$phat <- reg$fitted.values

# Distrib. de probabilidade
mydata.2 %>%
ggplot(aes(x = phat))+
  theme_bw()+
  geom_density(aes(col=depAdmin))+
  scale_y_continuous(expand = c(0.01,0))+
  labs(col="Dep. \n Adm.")

mydata.2 %>%
  ggplot(aes(x = phat))+
  theme_bw()+
  geom_density(aes(col=faixas_rendFamP))+
  scale_y_continuous(expand = c(0.01,0))+
  labs(col="Renda Familiar \n per capta")

# Manipulando o objeto de regressão --------------------------------------------

# Podemos extrair informações de interesse:

# Coeficientes
reg$coefficients

# Valores preditos
phat <- reg$fitted.values

# Modelo
modelo <- reg$model

modelo$phat <- reg$fitted.values

modelo <- relocate(.data = modelo,
                   NPI,phat,
                   .after = depAdmin2)
view(modelo)

# valor de Log-Likelihood (LL)
logLik(reg)

#Sigmóide
ggplotly(
  reg %>% 
    ggplot() +
    geom_point(aes(x = educMae, y = phat), color = "orange", size = 2) +
    geom_smooth(aes(x = educMae, y = phat), 
                method = "glm", formula = y ~ x, 
                method.args = list(family = "binomial"), 
                se = FALSE,
                color = "darkorchid", size = 2) +
    labs(x = "Escolarização da mãe",
         y = "Nota Para Ingresso (NPI)") +
    theme_bw()
)

# Fazendo predições para o modelo

predicao <- predict(object = reg,type = "response")

predicao <- as.numeric(predicao)

hist(predicao,probability = F,breaks = 50,col="lightblue",
     main="Histograma",ylab="Frequência",xlab="Probabilidade predita")
axis(side = 1, at = seq(0,1,0.1))

# Recomendações para interpretação ----

# https://blog.metodosquantitativos.com/razaodechances/