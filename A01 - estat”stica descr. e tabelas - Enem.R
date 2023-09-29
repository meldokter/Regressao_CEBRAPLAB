# Aula 01 - Estatística descritiva e tabelas

# 1. Setup ---------------------------------------------------------------------

# Limpando a memória
rm(list=ls()) # Remove all objects - remove objetos
gc()          # Garbage Clean - limpeza de lixos

# Set Working Directory - Define o diretório de trabalho.
# Insira o caminho entre aspas e com duas barras \\ como no caminho abaixo
# Caso tenha aberto pelo Rproject, este passo não é necessário.

# setwd("C:caminho_do_diretório\\arquivo")

# Get Working Directory - ver em qual diretório está trabalhando
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
       effects,reshape2,foreign,sjPlot,gt)

# 2. Importar base de dados ----------------------------------------------------

# Enem 2021 SP
sample_enem2021_sp <- import("0_data/ENEM/sample_enem2021_sp.csv")

# 3. Manuseio dos dados --------------------------------------------------------

# Novo objeto para manuseio (recomendado para evitar ficar carregando originais)
mydata <- sample_enem2021_sp

# Verificando nome das variáveis
names(mydata)

# Estrutura dos dados
str(mydata)

# Filtro escolas públicas
# Excluindo treineiros
mydata <- subset(mydata,subset = IN_TREINEIRO == 0)

# Seleção de variáveis
mydata <- subset(mydata,select = c(TP_COR_RACA,TP_SEXO,TP_FAIXA_ETARIA,
                                   CO_MUNICIPIO_ESC,TP_DEPENDENCIA_ADM_ESC,
                                   Q002,Q003,Q005,Q006,
                                   NU_NOTA_MT,NU_NOTA_LC,NU_NOTA_CH,NU_NOTA_CN,
                                   NU_NOTA_REDACAO
                                   ))

# Renomear variáveis - RENAME

mydata <- rename(.data = mydata,
                
                "raca"=TP_COR_RACA,
                "sexo"=TP_SEXO,
                "faixaEtaria"=TP_FAIXA_ETARIA,
                "depAdminEsc"=TP_DEPENDENCIA_ADM_ESC,
                
                "educMae" = Q002,
                "ocupPai" = Q003,
                "rendFam" = Q006,
                "pessoasResid" = Q005
                 )

# Verificando nome das variáveis
names(mydata)

# Modificar variáveis (MUTATE)
mydata <- mutate(.data = mydata,
                 
                 raca2 = case_when(
                   raca == 0 ~ "ND",
                   raca == 1 ~ "Branca",
                   raca == 4 ~ "Branca",
                   raca %in% c(2,3,5) ~ "PPI",
                   raca == 6 ~ "NA"
                 ),
                 
                 raca = case_when(
                   raca == 0 ~ "ND",
                   raca == 1 ~ "Branca",
                   raca == 2 ~ "Preta",
                   raca == 3 ~ "Parda",
                   raca == 4 ~ "Amarela",
                   raca == 5 ~ "Indígena",
                   raca == 6 ~ "NA"
                 ),
                 
                 faixaEtaria = case_when(
                   faixaEtaria %in% c(1:8) ~ "< 23",
                   faixaEtaria %in% c(9:11) ~ "24-30",
                   faixaEtaria %in% c(12:15) ~ "31-50",
                   faixaEtaria %in% c(15:20) ~ "> 50"
                 ),
                 
                 educMae = case_when(
                                    educMae == 'A' ~ 0,
                                    educMae == 'B' ~ 2,
                                    educMae == 'C' ~ 5,
                                    educMae == 'D' ~ 9,
                                    educMae == 'E' ~ 12,
                                    educMae == 'F' ~ 16,
                                    educMae == 'G' ~ 17,
                                    educMae == 'H' ~ 99),
                 
                 ocupPai = case_when(
                                  ocupPai == 'A' ~ 'G1',
                                  ocupPai == 'B' ~ 'G2',
                                  ocupPai == 'C' ~ 'G3',
                                  ocupPai == 'D' ~ 'G4',
                                  ocupPai == 'E' ~ 'G5',
                                  ocupPai == 'F' ~ 'NS'),
                                  
                 
                 rendFam = case_when(
                                    rendFam == "A" ~ 0,
                                    rendFam == "B" ~ 1100,
                                    rendFam == "C" ~ 1650,
                                    rendFam == "D" ~ 2200,
                                    rendFam == "E" ~ 2750,
                                    rendFam == "F" ~ 3300,
                                    rendFam == "G" ~ 4400,
                                    rendFam == "H" ~ 5500,
                                    rendFam == "I" ~ 6600,
                                    rendFam == "J" ~ 7700,
                                    rendFam == "K" ~ 8800,
                                    rendFam == "L" ~ 9900,
                                    rendFam == "M" ~ 11000,
                                    rendFam == "N" ~ 13200,
                                    rendFam == "O" ~ 16500,
                                    rendFam == "P" ~ 22000,
                                    rendFam == "Q" ~ 30000),
                 
                 depAdminEsc = factor(depAdminEsc,
                                  levels = 1:4,
                                  labels = c('Federal','Estadual',
                                             'Municipal','Privada'),
                                  ordered = T),
                 
                 media = (NU_NOTA_MT+NU_NOTA_LC+NU_NOTA_CH+NU_NOTA_CN)/4,
)


mydata <- mutate(.data = mydata,
                 
                 educMae = ifelse(educMae > 17,NA,educMae),
                 
                 rendFamP = rendFam/pessoasResid,
                 
                 raca = ifelse(raca == "NA",NA,raca),
                 raca2 = ifelse(raca2 %in% c('NA','ND'),NA,raca2),
                 
                 depAdmin = case_when(
                   depAdminEsc %in% c('Federal','Estadual','Municipal') ~ "Pública",
                   depAdminEsc %in% c('Privada') ~ "Privada",
                 )
)

# Estrutura dos dados (pós manuseio)
str(mydata)

# Salvando dados manuseados ----
export(x = mydata,file = "0_data/ENEM/mydata_enem_2021_sp.csv")

# Para integrar os dados ao ambiente do R
attach(mydata)

# 4. Análises ------------------------------------------------------------------

# Parte II - Estatística descritiva --------------------------------------------

#Média, mediana e amplitude - renda familiar per capita (rendFamP) e nível educacional 
da mãe (educMae)

mean(rendFamP, na.rm=TRUE)
mean(educMae, na.rm=TRUE)

median(rendFamP, na.rm=TRUE)
median(educMae, na.rm=TRUE)

range(rendFamP, na.rm=TRUE)
range(educMae, na.rm=TRUE)

var(rendFamP, na.rm=TRUE) # %>% sqrt() # Para calcular desvio-padrão
var(educMae, na.rm=TRUE)  # %>% sqrt()

sd(rendFamP, na.rm=TRUE)
sd(educMae, na.rm=TRUE)

#Quantis

Q1 <- quantile(rendFamP, probs = 0.25, na.rm=TRUE)
Q2 <- quantile(rendFamP, probs = 0.50, na.rm=TRUE)
Q3 <- quantile(rendFamP, probs = 0.75, na.rm=TRUE)

Q1
Q2
Q3

#Box-plot

boxplot(media, col="lightblue", main="Renda Familiar per capta")

boxplot(media~raca2, data=mydata, 
        main="Desempenho em Matemática \n Por raça", 
        xlab="", ylab="média do desempenho", 
        na.rm=TRUE)

# Com GGPlot - Grammar of Graphics

# Desempenho médio no Enem por raça
mydata %>% drop_na(raca2) %>% 
ggplot(data = .,aes(x=raca2,y=media))+
  theme_bw()+
  #geom_violin(fill="steelblue")+ # Inclui densidades ao longo da distrib
  geom_boxplot(fill="steelblue",alpha=1)+
  labs(title="Desempenho no ENEM",
       subtitle = "Por raça",
       x="",
       y="média do desempenho")

# Desempenho médio no Enem por ocupação do pai
mydata %>% drop_na(ocupPai) %>% 
  ggplot(data = .,aes(x=ocupPai,y=media))+
  theme_bw()+
  #geom_violin(fill="steelblue")+
  geom_boxplot(fill="steelblue",alpha=1)+
  labs(title="Desempenho no ENEM",
       subtitle = "Por ocupação do pai",
       x="",
       y="média do desempenho")

# Densidade
plt_density <- mydata %>% drop_na(raca2) %>% 
  ggplot(data = .,aes(fill=raca2,x=media))+
  theme_bw()+
  geom_density(alpha=.7)+
  scale_fill_manual(values = c("red","blue"))+
  labs(title="Desempenho no ENEM",subtitle = "Por raça",fill="",
    x = "", y = "Densidade")+
  geom_vline(xintercept = 550.2)+ # Linha mediana
  geom_vline(xintercept = 508.67) # Linha mediana

plt_boxplot <- mydata %>% drop_na(raca2) %>% 
  ggplot(data = .,aes(x=raca2,y=media,fill=raca2))+
  theme_bw()+ 
  geom_boxplot(alpha=.7)+
  #scale_y_continuous(limits = c(100,350))+
  scale_fill_manual(values = c("red","blue"))+
  labs(y = "Desempenho",x="")+
  theme(legend.position = "none"      # rm legend of year colors
  )+
  coord_flip()

plt_density / plt_boxplot + plot_layout(heights = c(3, 1))

#Comparando grupos

#Sumário descritivo

summary(rendFamP, na.rm=TRUE)
summary(educMae, na.rm=TRUE)

# Sumário mais completo
describe(rendFamP)

#Sumário descritivo comparativo por grupos (variável qualitativa)

describeBy(media, raca2, na.rm=TRUE)
describeBy(educMae, raca2, na.rm=TRUE)

#Box-plot comparativo (ME_NSE por DepAdmin)
mydata %>% drop_na(depAdminEsc) %>% 
  ggplot(data = .,aes(x=depAdminEsc,y=NU_NOTA_MT))+
  theme_bw()+
  geom_boxplot(fill="steelblue")+
  labs(title="Desempenho",
       subtitle = "Por Dep. Adm. da Escola",
       x="",
       y="média do desempenho")

#Histogramas

hist(media, breaks=20)
hist(rendFamP, breaks=10)

# Nota
x=scale(mydata$media)
hist(x, breaks=50, probability=TRUE, col="lightblue",
     main="",ylab="Densidade",
     xlab="Desempenho em desvio padrão",
     ylim=c(0,0.75),xlim=c(-4,4))
curve(dnorm(x),add=T,col="blue",lwd=2)

x=scale(mydata$rendFamP)
hist(x, breaks=50, probability=TRUE, col="lightblue",
     main="Renda Familiar per capta",ylab="Densidade",
     ylim=c(0,0.75),xlim=c(-4,4))
curve(dnorm(x),add=T)

#Correlação

cor(media,rendFamP, use = "complete.obs")

corr.test(media,rendFamP, use = "complete.obs")

# Filtro para o município de SP
mydata.2 <- mydata %>% filter(CO_MUNICIPIO_ESC == 3550308)

ggscatter(data = mydata.2, x = "rendFamP", y = "media", 
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Renda Familiar per capta", 
          ylab = "Nota em MT")

# Explorando a correlação graficamente
# Médias por renda familiar per capta

mediasRenda <- mydata.2 %>% group_by(rendFamP) %>% summarise(
  me = mean(media,na.rm=T)) %>% na.exclude()

# Observando relação

plt_nuvemPontos <- ggplot()+
  theme_bw()+
  geom_point(data = mydata.2,aes(x=rendFamP,y=media),
             size=2.5,colour="grey",alpha=.6
             )
plt_nuvemPontos

plt_lm <- plt_nuvemPontos+geom_smooth(data = mydata.2,aes(x = rendFamP,y = media),
                             method = "lm",formula = y~x)
plt_lm

plt_me_lm <- plt_lm + geom_point(data = mediasRenda,
                                       aes(x=rendFamP,y=me))
plt_me_lm

# Parte II - Tabela de frequências ---------------------------------------------

#Frequencias

freq(raca2,plot = FALSE)
freq(raca,plot = TRUE)

#Tabelas de contingência

#Dependência Administrativa - Tabelas, percentuais e gráfico pizza

table(depAdmin)
prop.table (table (depAdmin))

x<-c(0.59,0.41)
labels<-c("Públicas","Privada")
pie(x,labels=x, main = "Dependência Adm.",col = rainbow(length(x)))
legend("topright", labels, cex = 0.8, fill = rainbow(length(x)))

# Dep Admin e Raca - Tabelas Bivariadas e Qui-quadrado

tabela1 <- table(depAdmin,raca2)
tabela1

prop.table(tabela1)
prop.table(tabela1, margin = 1)
prop.table(tabela1, margin = 2)

summary(tabela1)

# Opção para exportar tabela
sjt.xtab(var.row = depAdmin,var.col = raca2,
         show.col.prc = T,
         title = "Tabela bonitona",
         var.labels = c("Dep. Admin. Esc.","Raça"),
         encoding = "UTF-8",
         #file = "onde salvar.xlsx"
         )
