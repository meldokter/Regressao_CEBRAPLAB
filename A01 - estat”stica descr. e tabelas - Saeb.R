# Aula 01 - Estatística descritiva e tabelas

# 1. Setup ---------------------------------------------------------------------

# Limpando a memória
rm(list=ls()) # Remove all objects - remove objetos
gc()          # Garbage Clean - limpeza de lixos

# Set Working Directory - Define o diretório de trabalho.
# Insira o caminho entre aspas e com duas barras \\ como no caminho abaixo
# Caso tenha aberto pelo Rproject, este passo não é necessário.

# setwd("C:caminho_do_diretório\\arquivo")

# Get Working Directory - Checa se o diretório está correto
getwd()

# Verificando arquivos e pastas no diretório
dir()

#Pacotes

# Rotina no R: instalação e carregamento de pacotes
install.packages("pacman")
library(pacman)

# Simplificação da rotina com o pacote "pacman"
# p_load: função que instala e carrega pacotes

p_load(tidyverse,ggplot2,rio,
       psych,descr,gridExtra,ggpubr,scales,patchwork,
       effects,reshape2,foreign,sjPlot)

# 2. Importar base de dados ----------------------------------------------------

# Saeb 2019
saeb_esc_2019 <- import("0_data/SAEB/SAEB_ESC_2019.xlsx")

# 3. Manuseio dos dados --------------------------------------------------------

# Novo objeto para manuseio (recomendado para evitar ficar carregando originais)
mydata <- saeb_esc_2019

# Filtro escolas públicas
# Dependência adm. diferente (!=) de 4
mydata <- subset(mydata,subset = TP_DEPENDENCIA != 4)

# Estrutura dos dados
str(mydata)

# Renomear variáveis - RENAME

mydata <- rename(.data = mydata,
                
                "ME_NSE"=ME_CE_2019,
                "ME_EDUCMAE"=ME_escMae_2019,
                
                "IEE"=IEE_2019,
                "PROP_BRANCOS"=PROP_aluBrancos_2019,
                "PROP_REGULAR"=PROP_aluFluxoReg_2019,
                "PROP_PRETENS"=PROP_aluPretenSoEstudar_2019,
                "ME_MT"=ME_MT_2019,                  
                "ME_LP"=ME_LP_2019,                  
                "ME_CN"=ME_CN_2019,                 
                "ME_CH"=ME_CH_2019
                 )

# Verificando nome das variáveis
names(mydata)

# Modificar variáveis (MUTATE)
mydata <- mutate(.data = mydata,
                 
                 tx_participacao_saeb = QT_alunoSaeb_2019 / QT_matrFunAF_2019,
                 
                 nvl_NSE = case_when(
                   ME_NSE <= median(ME_NSE,na.rm=T) ~ "Baixo",
                   ME_NSE >  median(ME_NSE,na.rm=T) ~ "Alto",
                 ),
                 
                 local = case_when(
                   TP_LOCALIZACAO == 1 ~ "Urbana",
                   TP_LOCALIZACAO == 2 ~ "Rural",
                 ),
                 
                 depAdmin = factor(TP_DEPENDENCIA,
                                   levels=c(1:3),
                                   labels=c("Federal","Estadual","Municipal"),
                                   ordered = TRUE)
                 )

mydata <- mutate(.data = mydata,
                 nvl_NSE = factor(nvl_NSE,levels=c("Baixo","Alto")),
)

# Para integrar os dados ao ambiente do R
attach(mydata)

# 4. Análises ------------------------------------------------------------------

# Parte II - Estatística descritiva --------------------------------------------

#Média, mediana e amplitude - anos de estudo (v4803) e rendimento mensal do 
#trabalho principal (v4719)

mean(ME_MT, na.rm=TRUE)
mean(ME_EDUCMAE, na.rm=TRUE)

median(ME_MT, na.rm=TRUE)
median(ME_EDUCMAE, na.rm=TRUE)

range(ME_NSE, na.rm=TRUE)
range(ME_EDUCMAE, na.rm=TRUE)

#Quantis

Q1 <- quantile(ME_NSE, probs = 0.25, na.rm=TRUE)
Q2 <- quantile(ME_NSE, probs = 0.50, na.rm=TRUE)
Q3 <- quantile(ME_NSE, probs = 0.75, na.rm=TRUE)

Q1
Q2
Q3

#Box-plot

boxplot(ME_NSE, col="lightblue", main="Média do Capital Econômico")

boxplot(ME_MT~nvl_NSE, data=mydata, 
        main="Desempenho em Matemática \n Por Nível Socioeconômico Médio", 
        xlab="Nível Socioeconômico Médio", ylab="Média do desempenho em MT", na.rm=TRUE)

# Com GGPlot - Grammar of Graphics
mydata %>% drop_na(nvl_NSE) %>% 
ggplot(data = .,aes(x=nvl_NSE,y=ME_MT))+
  theme_bw()+
  geom_boxplot(fill="steelblue")+
  labs(title="Desempenho em Matemática",
       subtitle = "Por Nível Socioeconômico Médio",
       x="Nível Socioeconômico Médio",
       y="Média do desempenho em MT")

#Comparando grupos

#Sumário descritivo

summary(ME_NSE, na.rm=TRUE)
summary(ME_EDUCMAE, na.rm=TRUE)

#Sumário descritivo comparativo por dependência administrativa

describeBy(ME_NSE, depAdmin, na.rm=TRUE)
describeBy(ME_EDUCMAE, depAdmin, na.rm=TRUE)

#Box-plot comparativo (ME_NSE por DepAdmin)

boxplot(ME_NSE~depAdmin, data=mydata, 
        main="Nível Socioeconômico Médio \n Por Dependência Administrativa", 
        xlab="Dependência Administrativa", ylab="Nível Socioeconômico Médio", 
        na.rm=TRUE)

#Histogramas

hist(ME_NSE, breaks=20)
hist(ME_EDUCMAE, breaks=10)

x=scale(mydata$ME_EDUCMAE)
hist(x, breaks=50, probability=TRUE, col="lightblue",
     main="Nível Socioeconômico Médio",ylab="Densidade",
     ylim=c(0,0.75),xlim=c(-4,4))
curve(dnorm(x),add=T)

#Correlação

cor(ME_NSE,ME_EDUCMAE, use = "complete.obs")

corr.test(ME_NSE,ME_EDUCMAE, use = "complete.obs")

# Filtro para município de SP
mydata.2 <- mydata %>% filter(ID_MUNICIPIO == 3550308)

ggscatter(data = mydata.2, x = "ME_NSE", y = "ME_EDUCMAE", 
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Média Nível Socioeconômico", 
          ylab = "Escolaridade da mãe")

# Parte II - Tabela de frequências ---------------------------------------------

#Frequencias

freq(depAdmin,plot = FALSE)
freq(depAdmin,plot = TRUE)

#Tabelas de contingência

#Acesso à internet - Tabelas, percentuais e gráfico pizza

table(depAdmin)
prop.table (table (depAdmin))
x<-c(0.4032,0.5960)
labels<-c("Estadual","Municipal")
pie(x,labels=x, main = "Dependência Adm.",col = rainbow(length(x)))
legend("topright", labels, cex = 0.8, fill = rainbow(length(x)))

#Participação no MT - Tabelas Bivariadas e Qui-quadrado

tabela1 <- table(local,nvl_NSE)
tabela1

prop.table(tabela1)
prop.table(tabela1, margin = 1)
prop.table(tabela1, margin = 2)

summary(tabela1)

##Trabalho na semana de referência - Tabelas Bivariadas e Qui-quadrado

tabela2 <- table(depAdmin,nvl_NSE)
tabela2

prop.table(tabela2)
prop.table(tabela2, 1)
prop.table(tabela2, 2)

summary(tabela2)
