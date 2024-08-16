# Mini-avaliação 4


# Carregando pacotes ------------------------------------------------------
# ---
library(readr)
library(ggplot2)
library(survival)
library(gamlss)
library(gamlss.cens)
library(broom)
library(tidyverse)
library(pammtools)
library(numDeriv)
library(KMsurv)
library(survminer)
library(EstimationTools)

# Carregando dados --------------------------------------------------------
# ---

# Lista os datasets disponíveis no pacote KMsurv
data(package = "KMsurv")

# Carregar um dataset específico, por exemplo, o dataset "std"
data("std", package = "KMsurv")

# As primeiras linhas do dataset
head(std)


# Selecionando as colunas de interesse
dados <- subset(std, select = c("time", "rinfct", "yschool",
                                         "npartner", "abdpain", "os12m"))
#head(dados)

# Análise exploratória de Dados

summary(dados)

# Histograma 

# Criar histograma
hist(dados$time, main = "Tempos de sobrevivência", xlab = "Tempo", ylab = "Frequência")

# Criar gráfico de densidade
plot(density(dados$time), main = "Tempos de sobrevivência", xlab = "Tempo", ylab = "Densidade")

# Questão 1 

# Construindo o TTT Plot ---------------------------------------------------

TTT = TTTE_Analytical(Surv(dados$time, dados$rinfct)~1, method='censored')

dadosTTT = tibble(
  x = TTT$`i/n`,
  y = TTT$phi_n
)

G = 
  dadosTTT %>% 
  ggplot(aes(x=x, y=y)) +
  geom_step() + 
  geom_point() +
  geom_abline(slope=1, intercept=0, linetype="dashed") +
  lims(y=c(0,1), x=c(0,1)) +
  labs(x = bquote(u[i]), y = bquote(varphi[i])) +
  theme_light(); G

# Questão 2 


# Função de sobrevivência de Kaplan-Meier

# Ajuste do modelo Kaplan-Meier
fit <- survfit(Surv(time, rinfct) ~ 1, data=dados)

# Plotagem da curva de sobrevivência Kaplan-Meier
ggsurvplot(
  fit,                     # Resultado do ajuste do modelo
  data = dados,             # Dados
  risk.table = TRUE,       # Mostrar tabela de riscos
  pval = TRUE,             # Mostrar valor p para testes log-rank
  conf.int = TRUE,         # Mostrar intervalo de confiança
  xlab = "Tempo",          # Rótulo do eixo x
  ylab = "Probabilidade de sobrevivência", # Rótulo do eixo y
  ggtheme = theme_light()  # Tema do gráfico
)

# Questão 3

# Categorizando a variável yschool em duas categorias
media_yschool <- mean(dados$yschool)  # Calcula a média de yschool
media_yschool

dados$yschool_cat <- ifelse(dados$yschool >= media_yschool, "Maior", "Menor")

# Verificando a nova variável criada
table(dados$yschool_cat)

# Análise da nova variável categorizada em relação à variável de sobrevivência
fit_yschool <- survfit(Surv(time, rinfct) ~ yschool_cat, data = dados)

# Plotagem da curva de sobrevivência Kaplan-Meier para cada categoria
ggsurvplot(
  fit_yschool,
  data = dados,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = TRUE,
  xlab = "Tempo",
  ylab = "Probabilidade de sobrevivência",
  ggtheme = theme_light(),
  legend.title = "Categoria de yschool"
)

# Questão 4

# Categorizando a variável npartner em duas categorias com base na média
media_npartner <- mean(dados$npartner)  # Calcula a média de npartner

dados$npartner_cat <- ifelse(dados$npartner >= media_npartner, "Maior", "Menor")

# Ajuste do modelo Kaplan-Meier estratificado pela variável npartner
fit_npartner <- survfit(Surv(time, rinfct) ~ npartner_cat, data = dados)

# Paleta de cores 
cores <- c("#66CDAA", "#B03060")

# Plotagem da curva de sobrevivência Kaplan-Meier para cada categoria com cores diferentes
ggsurvplot(
  fit_npartner,
  data = dados,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = TRUE,
  xlab = "Tempo",
  ylab = "Probabilidade de sobrevivência",
  ggtheme = theme_light(),
  legend.title = "Categoria de npartner",
  palette = cores
)

# Questão 5

# Ajuste do modelo de Kaplan-Meier estratificado por abdpain
fit <- survfit(Surv(time, rinfct) ~ abdpain, data = dados)

# Paleta de cores
cores <- c("#1f77b4", "#ff7f0e")

ggsurvplot(fit, data = dados, risk.table = TRUE, pval = TRUE,conf.int = TRUE,
           title = "Curva de Sobrevivência de Kaplan-Meier\nEstratificada por dor abdominal",
           xlab = "Tempo (dias)", ylab = "Probabilidade de sobrevivência",
           ggtheme = theme_minimal(),
           legend.title = "Presença de dor abdominal",
           palette = cores,
           legend.labs = c("Sem dor abdominal", "Com dor abdominal"),
           risk.table.y.text.col = TRUE,
           tables.theme = theme_cleantable(),
           tables.col = "black",
           tables.y.text.col = TRUE,
           tables.y.text.size = 3,
           font.x = 12,
           font.y = 12,
           font.tickslab = 10,
           font.title = 14,
           font.caption = 10,
           font.legend = 12
)

# Verificando informações

# Ajuste do modelo de Kaplan-Meier estratificado por abdpain
fit <- survfit(Surv(time, rinfct) ~ abdpain, data = dados)
summary(fit)


# Questão 6

dados <- subset(std, select = c("time", "rinfct", "yschool",
                                "npartner", "abdpain", "os12m"))

# Covariável: os12m (Realização de sexo oral nos últimos 12 meses)

media_os12m <- mean(dados$os12m)  # Calcula a média de os12m

dados$os12m_cat <- ifelse(dados$os12m >= media_os12m, "Maior", "Menor")

#TTTplot os12m para cada estratificação (Realização de sexo oral nos últimos 12 meses)
plot(TTTE_Analytical(Surv(time, rinfct) ~ dados$os12m_cat, method = "censored", data = std), 
     main = "Realização de sexo oral nos últimos 12 meses - Maior e Menor que a Média", 
     col = c('#98FB98', '#B03060'), lwd = 2)
legend("bottomright", legend = c("Maior", "Menor"), col = c("#98FB98", "#B03060"), lty = 1, cex = 0.8, lwd = 2)


# Covariável: yschool (Anos	de	estudo)

media_yschool <- mean(dados$yschool)  # Calcula a média de yschool

dados$yschool_cat <- ifelse(dados$yschool >= media_yschool, "Maior", "Menor")

#TTTplot yschool para cada estratificação
plot(TTTE_Analytical(Surv(time, rinfct) ~ dados$yschool_cat, method = "censored", data = std), 
     main = "Anos 	de estudo - Maior e Menor que a Média", 
     col = c('#FFA500', '#9A32CD'), lwd = 2)
legend("bottomright", legend = c("Maior", "Menor"), col = c("#FFA500", "#9A32CD"), lty = 1, cex = 0.8, lwd = 2)


# Covariável: npartner (Número	de	parceiros	nos	últimos	30	dias) 

media_npartner <- mean(dados$npartner)  # Calcula a média de npartner

dados$npartner_cat <- ifelse(dados$npartner >= media_npartner, "Maior", "Menor")

#TTTplot npartner para cada estratificação
plot(TTTE_Analytical(Surv(time, rinfct) ~ dados$yschool_cat, method = "censored", data = std), 
     main = "Nº de parceiros	nos	últimos	30dias - Maior e Menor que a Média", 
     col = c('#4169E1', '#FF0000'), lwd = 2)
legend("bottomright", legend = c("Maior", "Menor"), col = c("#4169E1", "#FF0000"), lty = 1, cex = 0.8, lwd = 2)


# Covariável: abdpain	(Presença	de	dor	abdominal)

media_abdpain <- mean(dados$abdpain)  # Calcula a média de abdpain

dados$abdpain_cat <- ifelse(dados$abdpain >= media_abdpain, "Maior", "Menor")

#TTTplot abdpain para cada estratificação
plot(TTTE_Analytical(Surv(time, rinfct) ~ dados$abdpain_cat, method = "censored", data = std), 
     main = "Nº de parceiros	nos	últimos	30dias - Maior e Menor que a Média", 
     col = c('#2E8B57', '#FA8072'), lwd = 2)
legend("bottomright", legend = c("Maior", "Menor"), col = c("#2E8B57", "#FA8072"), lty = 1, cex = 0.8, lwd = 2)


# Questão 7

dados <- subset(std, select = c("time", "rinfct", "yschool",
                                "npartner", "abdpain", "os12m"))

# =========================================================================
# LOGRANK PARA MULTIPLOS GRUPOS
# =========================================================================


# Teste de log-rank para a variável 'os12m'
os12m_test <- survdiff(Surv(time, rinfct) ~ os12m, data = dados)
print(os12m_test)

# Teste de log-rank para a variável 'yschool'
yschool_test <- survdiff(Surv(time, rinfct) ~ yschool, data = dados)
print(yschool_test)

# Teste de log-rank para a variável 'npartner'
npartner_test <- survdiff(Surv(time, rinfct) ~ npartner, data = dados)
print(npartner_test)

# Teste de log-rank para a variável 'abdpain'
abdpain_test <- survdiff(Surv(time, rinfct) ~ abdpain, data = dados)
print(abdpain_test)

# Conclusão:
# VFVF 


# Questão 8 

# Carregando pacotes ------------------------------------------------------
# ---
library(gamlss)
library(gamlss.cens)
library(broom)
library(tidyverse)
library(pammtools)
library(numDeriv)

# Carregando dados --------------------------------------------------------
data("std", package = "KMsurv")
data <- std

t = std$time
cens = std$rinfct
dados = tibble(tempos=t, cens=cens)
# Kaplan-Meier ------------------------------------------------------------
# ---
ekm = survfit(Surv(tempos,cens)~1, data=dados)
dadosAux =
  tidy(ekm) %>%
  dplyr::rename(
    'S' = 'estimate',
    't' = 'time',
    'liS' = 'conf.low',
    'lsS' = 'conf.high',
    'nEvento' = 'n.event'
  ) %>%
  dplyr::select(t,S,liS,lsS,nEvento) %>%
  bind_rows(tibble(t=0,S=1,liS=1,lsS=1,nEvento=0))
G =
  dadosAux %>%
  ggplot() +
  geom_step(aes(x=t, y=S), size=0.4) +
  geom_stepribbon(aes(x=t, ymin=liS, ymax=lsS), alpha=0.15) +
  geom_point(data=dadosAux %>% filter(nEvento==0,t>0), aes(x=t, y=S), shape=3,
             size=3) +
  coord_cartesian(ylim=c(0,1), xlim=c(0,max(dadosAux$t))) +
  labs(x='t (meses)', y='S(t)') +
  theme_minimal() +
  theme(legend.position='bottom'); G


# Estimacao de maxima verossimilhanca (com o pacote gamlss) ---------------
# WEI: Weibull
gen.cens(WEI)
mod = gamlss(
  formula=Surv(tempos,cens)~1, 
  family=WEIrc(mu.link = 'identity', sigma.link = 'identity'), 
  data=dados
)

# informe a sigla correspondente para a distribuição escolhida
# ---
dist = 'WEI'

# Estimativas de maxima verossimilhanca e matriz de variancias -----------
# ---
emv = c(
  mu = mod$mu.coefficients %>% as.numeric(), 
  sigma = mod$sigma.coefficients %>% as.numeric()
)
print("Os valores estimados dos parâmetros foram:")
emv
mcov = vcov(mod)

summary(mod)

# Conclusão: FVFF

# Questão 9 

library(tibble)
library(purrr)
library(ggplot2)
library(dplyr)
library(numDeriv)
library(ggpubr)

# Estimativas para a mediana --------------------------------------------
# ---
# LOG MEDIANA
# ---
logm = function(par){ log( do.call(paste0('q',dist), c(list(p=0.5), par)) ) }
derivada.logm = grad(func=logm, x=emv)
logm.pt = logm(emv)
ep.logm = sqrt( c( derivada.logm %*% mcov %*% derivada.logm ) )
logm.li = logm.pt-qnorm(0.975)*ep.logm
logm.ls = logm.pt+qnorm(0.975)*ep.logm
mediana.pt = exp( logm.pt )
mediana.li = exp( logm.li )
mediana.ls = exp( logm.ls )
# Estimativas para as curvas --------------------------------------------
# ---
dadosAux =
  tibble(t=seq(1e-5,max(t),l=100)) %>%
mutate(
  densidade = purrr::map(.x=t, .f=function(x){
  # LOG DENSIDADE
    # ---
      logd = function(par){ do.call(paste0('d',dist), c(list(x=x), par,
                                                        log=TRUE)) }
      derivada.logd = grad(func=logd, x=emv)
      
      log.pt = logd(emv)
      ep.log = sqrt( c( derivada.logd %*% mcov %*% derivada.logd ) )
      log.li = log.pt-qnorm(0.975)*ep.log
      log.ls = log.pt+qnorm(0.975)*ep.log
      tibble(
        d.pt = exp(log.pt),
        d.li = exp(log.li),
        d.ls = exp(log.ls)
      )
  })
) %>%
  mutate(
    risco = purrr::map(.x=t, .f=function(x){
      # LOG RISCO
      # ---
      logh = function(par){
        do.call(paste0('d',dist), c(list(x=x), par, log=TRUE)) -
          do.call(paste0('p',dist), c(list(q=x), par, log=TRUE, 
                                      lower.tail=FALSE))
      }
      derivada.logh = grad(func=logh, x=emv)
      log.pt = logh(emv)
      ep.log = sqrt( c( derivada.logh %*% mcov %*% derivada.logh ) )
      log.li = log.pt-qnorm(0.975)*ep.log
      log.ls = log.pt+qnorm(0.975)*ep.log
      tibble(
        h.pt = exp(log.pt),
        h.li = exp(log.li),
        h.ls = exp(log.ls)
      )
    })
  ) %>%
  mutate(
    riscoAcumulado = purrr::map(.x=t, .f=function(x){
      # LOG RISCO ACUMULADO
      # ---
      logH = function(par){
        
        H = -log( do.call(paste0('p',dist), c(list(q=x), par, log=FALSE, 
                                              lower.tail=FALSE)) )
        log(H)
      }
      derivada.logH = grad(func=logH, x=emv)
      log.pt = logH(emv)
      ep.log = sqrt( c( derivada.logH %*% mcov %*% derivada.logH ) )
      log.li = log.pt-qnorm(0.975)*ep.log
      log.ls = log.pt+qnorm(0.975)*ep.log
      tibble(
        H.pt = exp(log.pt),
        H.li = exp(log.li),
        H.ls = exp(log.ls)
      )
    })
  ) %>%
  mutate(
    sobrevivencia = purrr::map(.x=t, .f=function(x){
      # LOG-LOG SOBREVIVENCIA
      # ---
      loglogS = function(par){
        log(-log( do.call(paste0('p',dist), c(list(q=x), par, log=FALSE, 
                                              lower.tail=FALSE)) ) )
      }
      derivada.loglogS = grad(func=loglogS, x=emv)
      loglog.pt = loglogS(emv)
      ep.loglog = sqrt( c( derivada.loglogS %*% mcov %*% derivada.loglogS ) )
      loglog.li = loglog.pt-qnorm(0.975)*ep.loglog
      loglog.ls = loglog.pt+qnorm(0.975)*ep.loglog
      tibble(
        S.pt = exp(-exp(loglog.pt)),
        S.li = exp(-exp(loglog.li)),
        S.ls = exp(-exp(loglog.ls))
      )
    })
  ) %>%
  unnest_legacy(); dadosAux

# Graficos das curvas estimadas -----------------------------------------
# ---
G0.S =
  G +
  geom_line(data=dadosAux, aes(x=t, y=S.pt), colour='red', linewidth=1.25) +
  geom_ribbon(data=dadosAux, aes(x=t, ymin=S.li, ymax=S.ls), fill='red',
              alpha=0.15) +
  geom_segment(aes(x=mediana.li, xend=mediana.ls, y=0, yend=0), colour='red',
               linewidth=1.25) +
  geom_point(aes(x=mediana.pt, y=0), colour='red', size=3) +
  geom_vline(aes(xintercept=mediana.pt), colour='red', linewidth=0.25,
             linetype='dashed') +
  geom_hline(aes(yintercept=0.5), colour='red', linewidth=0.25,
             linetype='dashed') +
  geom_label(aes(x=mediana.pt, y=0, label='Tempo Mediano\nde Falha'),
             colour='red', size=3, vjust=-0.5, check_overlap=TRUE) +
  labs(title='SOBREVIVÊNCIA') +
  theme(
    legend.position='bottom',
    plot.title = element_text(face='bold', hjust=0.5, size=16)
  ); G0.S
G0.h =
  dadosAux %>%
  ggplot() +
  geom_line(aes(x=t, y=h.pt), colour='red', linewidth=1.25) +
  geom_ribbon(aes(x=t, ymin=h.li, ymax=h.ls), fill='red', alpha=0.15) +
  coord_cartesian(ylim=c(0,0.05), xlim=c(0,max(dadosAux$t))) +
  labs(x='t (meses)', y='h(t)', title='RISCO') +
  theme_minimal() +
  theme(
    legend.position='bottom',
    plot.title = element_text(face='bold', hjust=0.5, size=16)
  ); G0.h
G0.d =
  dadosAux %>%
  ggplot() +
  geom_line(aes(x=t, y=d.pt), colour='red', linewidth=1.25) +
  geom_ribbon(aes(x=t, ymin=d.li, ymax=d.ls), fill='red', alpha=0.15) +
  coord_cartesian(ylim=c(0,0.02), xlim=c(0,max(dadosAux$t))) +
  labs(x='t (meses)', y='f(t)', title='DENSIDADE') +
  theme_minimal() +
  theme(
    legend.position='bottom',
    plot.title = element_text(face='bold', hjust=0.5, size=16)
  ); G0.d
G0.H =
  dadosAux %>%
  ggplot() +
  geom_line(aes(x=t, y=H.pt), colour='red', linewidth=1.25) +
  geom_ribbon(aes(x=t, ymin=H.li, ymax=H.ls), fill='red', alpha=0.15) +
  coord_cartesian(xlim=c(0,max(dadosAux$t))) +
  labs(x='t (meses)', y='H(t)', title='RISCO ACUMULADO') +
  theme_minimal() +
  theme(
    legend.position='bottom',
    plot.title = element_text(face='bold', hjust=0.5, size=16)
  ); G0.H
G0 = ggpubr::ggarrange(G0.d,G0.S,G0.h,G0.H, ncol=2, nrow=2); G0

# Questão 10

sprintf("IC para a mediana: [%s ; %s]", mediana.li, mediana.ls )

