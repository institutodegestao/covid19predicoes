library(forecast)
library(ggplot2)
library(urca)

source('./global.R')
source('./exponencial_pernambuco.R')
source('./linear_pernambuco.R')
source('./logistico_pernambuco.R')
source('./potencia_pernambuco.R')
source('./sir_pernambuco.R')
source('./log_log_pernambuco.R')

metrica_potencia <- as.data.frame(
  forecast::accuracy(predict(fit_lp, pe_relatorio), pe_relatorio$totalCases)
  )

metrica_linear <- as.data.frame(forecast::accuracy(exp(predict(fit_lm, pe_relatorio)), pe_relatorio$totalCases))

metrica_logistico <- as.data.frame(forecast::accuracy(predict(fit_log, pe_relatorio), pe_relatorio$totalCases))

metrica_exponencial <- as.data.frame(forecast::accuracy(predict(fit_exp, pe_relatorio), pe_relatorio$totalCases))

metrica_log_log <- as.data.frame(forecast::accuracy(predict(fit_ll, pe_relatorio), pe_relatorio$totalCases))

avaliacao_sir <- data.frame(ode(y = init, times = pe_relatorio$dia, func = SIR, parms = Opt_par))
metrica_sir <- as.data.frame(forecast::accuracy(avaliacao_sir$I, pe_relatorio$totalCases))

metricas <- rbind(metrica_potencia, metrica_linear)
metricas <- rbind(metricas, metrica_logistico)
metricas <- rbind(metricas, metrica_exponencial)
metricas <- rbind(metricas, metrica_sir)
metricas <- rbind(metricas, metrica_log_log)
rownames(metricas) <- c('potencia', 'linear', 'logistico', 'exponencial','sir', 'log_log')
metricas
