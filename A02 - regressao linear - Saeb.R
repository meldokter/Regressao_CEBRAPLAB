# Aula 02 - Regressão linear

# 1. Setup ---------------------------------------------------------------------

# Limpando a memória
rm(list=ls()) # Remove all objects - remove objetos
gc()          # Garbage Clean - limpeza de lixos

# Caminho simplificado para carregar pacotes (PACMAN)
#install.packages(pacman)
library(pacman)
p_load(tidyverse,ggplot2,rio)

# 2. Importar base de dados ----------------------------------------------------

# Saeb 2019
saeb_esc_2019 <- import("0_data/SAEB_ESC_2019.xlsx")

# 3. Manuseio dos dados --------------------------------------------------------

mydata <- saeb_esc_2019

# Filtro escolas públicas (FILTER)
# Dependência adm. diferente (!=) de 4
mydata <- mydata %>% filter(TP_DEPENDENCIA != 4)

# Estrutura dos dados
str(mydata)

# Renomear variáveis - RENAME

mydata <- rename(.data = mydata,
                 "ME_NSE"=ME_CE_2019,
                 "IEE"=IEE_2019,
                 "ME_MT"=ME_MT_2019,                  
)

# Verificando nome das variáveis
names(mydata)

# Selecionando variáveis (SELECT)
mydata <- select(.data = mydata,ID_ESCOLA,ME_MT,ME_NSE,IEE)

# Excluindo missing para cálculos da regressão
mydata <- na.exclude(mydata)

# Variável dependente: proficiência em matemática

# Variáveis independentes:
# 1. Nível Socioeconômico Médio (ME_NSE)
# 3. Estrutura da escola (IEE)

# Análises descritivas (I): Gráficos Univariados -------------------------------

# Como se comportam nossas variáveis?

# Histograma do Nível Socioeconômico Médio (ME_NSE)
mydata %>%
  ggplot(aes(x = ME_NSE))+
  geom_histogram()

# Histograma da Estrutura da escola (IEE)
mydata %>%
  ggplot(aes(x = IEE)) +
  geom_bar()

# Análises descritivas (II): Gráficos Bivariados -------------------------------

# Desempenho em MT e NSE
mydata %>% 
  ggplot(aes(y=ME_MT,x=ME_NSE))+
  theme_bw()+
  geom_point(col="gray")+
  geom_smooth(method = "lm",formula = y~x)+
  labs(title="Desempenho em Matemática",
       subtitle = "Por Nível Socioeconômico Médio",
       x="Nível Socioeconômico Médio",
       y="Média do desempenho em MT")

# Desempenho em MT e IEE
mydata %>% 
  ggplot(aes(y=ME_MT,x=IEE))+
  theme_bw()+
  geom_point(col="gray")+
  geom_smooth(method = "lm",formula = y~x)+
  labs(title="Desempenho em Matemática",
       subtitle = "Por Infraestrutura Escolar",
       x="Indicador de Infraestrutura Escolar",
       y="Média do desempenho em MT")

# Regressao --------------------------------------------------------------------

reg = lm(data = mydata,ME_MT ~ ME_NSE + IEE)
summary(reg)

# Manipulando o objeto de regressão --------------------------------------------

# Podemos extrair informações de interesse:

# - Coeficientes
beta = coef(reg)

# Valores observados
yobs = mydata$ME_MT

# - Valores preditos pela regressão
yhat = predict(reg)

# - Resíduos
# Os resíduos são a diferença entre valor observado e valor predito pela reg
e = yobs - yhat
head(e)

e = residuals(reg)
head(e)

# Podemos calcular "na mão" algumas estatísticas de interesse

# - Variância dos resíduos (ou Mean Square Error - MSE)
mse = 1/length(yobs) * sum(e^2)

# - Coeficiente de determinação
var_y = var(yobs)

r2 = 1 - mse/var_y

r = cor(yobs, yhat)
r^2

# Análise gráfica de resíduos --------------------------------------------------

# Gráfico de resíduos
t = tibble(yobs,yhat,e,
           IEE      = mydata$IEE,
           experiencia  = mydata$ME_CE) 


hist(t$e)