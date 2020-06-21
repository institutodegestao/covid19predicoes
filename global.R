library(aomisc)
library(bbmle)
library(deSolve)
library(dplyr)
library(drc)
library(DT)
library(GenSA)
library(GA)
library(ggplot2)
library(Hmisc)
library(lubridate)
library(Metrics)
library(mgcv)
library(nplr)
library(nlme)
library(plotly)
library(plyr)
library(quantmod)
library(RColorBrewer)
library(stats)
library(tidyr)
library(timeDate)
library(xts)

quadro_pernambuco <- read.csv2('evolucao_geral.csv', sep = ';')

mundo <- read.csv2('../../mundo/resultado/covid19gravidade.csv', sep = ';')

colnames(mundo) <- c('local', 'casos', 'novos_casos', 'obitos', 'novos_obitos', 'recuperados', 'ativos', 'criticos', 'casos_milhao', 'obitos_milhao', 'concluidos', 'testes', 'testes_milhao', 'txConc', 'txConcObitos', 'txCriticos', 'txPopObitos', 'data_extracao', 'lat', 'long')

mundo$obitos_milhao <- as.integer(as.character(mundo$obitos_milhao))

belgicaCasosMilhao <- mundo %>% filter(local == 'Belgium') %>% select(casos_milhao) %>% as.integer

belgicaMortesMilhao <- mundo %>% filter(local == 'Belgium') %>% select(obitos_milhao) %>% as.integer

brasilCasosMilhao <- mundo %>% filter(local == 'Brazil') %>% select(casos_milhao) %>% as.integer

brasilMortesMilhao <- mundo %>% filter(local == 'Brazil') %>% select(obitos_milhao) %>% as.integer

espanhaCasosMilhao <- mundo %>% filter(local == 'Spain') %>% select(casos_milhao) %>% as.integer

espanhaMortesMilhao <- mundo %>% filter(local == 'Spain') %>% select(obitos_milhao) %>% as.integer

url = 'https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv'
covidBr = read.csv2(url, encoding='latin1', sep = ',')

pe_relatorio <- subset(covidBr, state == 'PE')

pe_relatorio$date <- as.Date(pe_relatorio$date, format = "%Y-%m-%d")

pe_relatorio$dia <- seq(1:length(pe_relatorio$date))

colnames(pe_relatorio)[1] <- 'data'

pred_dia = data.frame(dia = pe_relatorio$dia)
pred_seq = data.frame(dia = seq(max(pred_dia$dia)+1, max(pred_dia$dia)+180))

pred_dia <- rbind(pred_dia, pred_seq)

idades <- read.csv2('base_idade_desc.csv', sep = ';')
idades$propocao <- round(idades$propocao,3)
idades$chance_obito <- round(idades$chance_obito,3)

urlGoogle = 'https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=911a386b6c9c230f'
googleMob = read.csv2(urlGoogle, sep = ',')

googleMob <- googleMob %>% filter(sub_region_1 == 'State of Pernambuco')

googleMob$date <- as.Date(googleMob$date, format = "%Y-%m-%d")

googleMob$isolamento <- scale(googleMob$residential_percent_change_from_baseline)

write.table(googleMob, '../../covidpe/resultado/googleMob.csv', sep = ';', row.names=FALSE)

appleMob <- read.csv2('../avaliacao/applemobilitytrends.csv', sep = ',')

appleMob <- appleMob %>% filter(region == 'Pernambuco')

appleMob <- appleMob %>% 
  tidyr::pivot_longer(
    cols = starts_with("X"), 
    names_to = "Date", 
    values_to = "Mob", 
    names_prefix = "X")

appleMob$Date <- as.Date(appleMob$Date, format = "%Y.%m.%d")

appleMob$Mob <- as.numeric(as.character(appleMob$Mob))

appleMob$isolamento <- scale(appleMob$Mob)

write.table(appleMob, '../../covidpe/resultado/appleMob.csv', sep = ';', row.names=FALSE)

inlocoMob <- read.csv2('../avaliacao/timeline_city_data.csv', sep = ';', encoding = 'UTF-8')

colnames(inlocoMob)[c(1,5)] <- c('Date', 'isolamento_pe')
inlocoMob$Date <- as.Date(inlocoMob$Date, format = "%d/%m/%Y")

inlocoMob$isolamento <- scale(inlocoMob$isolamento_pe)

write.table(inlocoMob, '../../covidpe/resultado/inlocoMob.csv', sep = ';', row.names=FALSE)

base_obitos_dt <- read.csv2('base_obitos.csv', sep = ';')

base_obitos_dt$data <- as.Date(base_obitos_dt$data, format = "%Y-%m-%d")

base_obitos_dt <- base_obitos_dt %>% group_by(data = data) %>% summarise(obitosNovos = sum(CONFIRMADO))

base_obitos_dt <- merge(pe_relatorio, base_obitos_dt, by = 'data', all.y = T)

base_obitos_dt <- base_obitos_dt[, c('data', 'newDeaths', 'deaths', 'obitosNovos')]

base_obitos_dt <- base_obitos_dt %>% replace(is.na(.), 0)
base_obitos_dt$obitos <- cumsum(base_obitos_dt$obitosNovos)

obitos_mm10 <- rollmean(base_obitos_dt[,4], 7, fill = list(NA, NULL, NA), align = "right")
base_obitos_dt <- cbind(base_obitos_dt, obitos_mm10 = round(obitos_mm10,2))

deaths_mm10 <- rollmean(base_obitos_dt[,2], 7, fill = list(NA, NULL, NA), align = "right")
base_obitos_dt <- cbind(base_obitos_dt, deaths_mm10 = round(deaths_mm10,2))

base_obitos_dt <- base_obitos_dt %>% replace(is.na(.), 0)

solicitacoes_totais <- read.csv2('solicitacoes_totais.csv', sep = ';')
solicitacoes_totais$data_sol <- as.Date(solicitacoes_totais$data_sol, format = "%Y-%m-%d")
solicitacoes_totais <- solicitacoes_totais[, c('data_sol', 'solicitacoes')]

base_obitos_dt <- merge(base_obitos_dt, solicitacoes_totais, by.x = 'data', by.y = 'data_sol')

solicitacoes_mm10 <- rollmean(base_obitos_dt[,8], 7, fill = list(NA, NULL, NA), align = "right")
base_obitos_dt <- cbind(base_obitos_dt, solicitacoes_mm10 = round(solicitacoes_mm10,2))
base_obitos_dt <- base_obitos_dt %>% replace(is.na(.), 0)

base_obitos_dt$dia <- weekdays(base_obitos_dt$data, abbreviate=T)

solicitacoes_totais$dia <- weekdays(solicitacoes_totais$data_sol, abbreviate=T)

solicitacoes_totais$dia <- revalue(solicitacoes_totais$dia , c("sáb"="sab"))

solicitacoes_totais$dia <- ordered(solicitacoes_totais$dia, levels=c("seg", "ter", "qua", "qui", "sex", "sab", "dom"))

solicitacoes_totais$dia <- solicitacoes_totais$dia %>% replace(is.na(.), 'sab')

sol_dia <- solicitacoes_totais %>% group_by(dia) %>% summarise(media = mean(solicitacoes))

sol_ts <- ts(solicitacoes_totais$solicitacoes, start = decimal_date(as.Date("2020-03-19")), frequency = 7)

sol_ts_dec <- decompose(sol_ts)

# appleMob$dia <- weekdays(appleMob$Date, abbreviate=T)
# 
# plot_ly(appleMob, y = ~Mob, color = ~dia, type = "box") %>% plotly::layout(
#   title = 'Mobilidade por dia da Semana - Apple (Histograma)',
#   yaxis = list(title = "Mobilidade Relativa"),
#   xaxis = list(title = "Dia da Semana"),
#   legend = list(x = 0.1, y = 0.9),
#   hovermode = "compare"
# ) 

solicitacoes_municipios <- read.csv2('solicitacoes_municipios.csv', sep = ';')
solicitacoes_municipios$data <- as.Date(solicitacoes_municipios$data, format = "%Y-%m-%d")

solicitacoes_municipios$total <- solicitacoes_municipios$ENFERMARIA + solicitacoes_municipios$UTI

solicitacoes_municipios_tl <- solicitacoes_municipios %>% group_by(data, municipio) %>% summarise(sol = sum(total))

solicitacoes_municipios_tl_rec <- solicitacoes_municipios_tl %>% filter (municipio == 'RECIFE')

write.table(base_obitos_dt, 'base_comparativa.csv', sep = ';', row.names=FALSE)

write.table(base_obitos_dt, '../../SDEC/base_comparativa.csv', sep = ';', row.names=FALSE)

write.table(base_obitos_dt, '../../covidpe/resultado/base_comparativa.csv', sep = ';', row.names=FALSE)

write.table(solicitacoes_totais, '../../covidpe/resultado/solicitacoes_totais.csv', sep = ';', row.names=FALSE)

