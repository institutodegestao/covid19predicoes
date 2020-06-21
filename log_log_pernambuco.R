fit_ll <- drm(totalCases ~ dia, fct = W1.4(),
data = pe_relatorio)
summary(fit_ll)
plot(fit_ll, log="", main = "Log logistic")
# x <- getMeanFunctions()
# LL.2 LL2.2 LL2.5 LL.3 LL.3u LL.4 LL.5 W1.2 W1.3 W1.4 AR.2

fit_ll_ob <- drm(deaths ~ dia, fct = LL.5(),
              data = pe_relatorio)
summary(fit_ll_ob)
plot(fit_ll_ob, log="", main = "Log logistic")

pred_ll <- data.frame(predicao = ceil(predict(fit_ll, pred_dia)))
pred_ll$data <- seq.Date(as.Date('2020-03-12'), by = 'day', length.out = length(pred_dia$dia))

pred_ll_ob <- data.frame(predicaoOb = ceil(predict(fit_ll_ob, pred_dia)))
pred_ll_ob$data <- seq.Date(as.Date('2020-03-12'), by = 'day', length.out = length(pred_dia$dia))

pred_ll <- merge(pred_ll, pe_relatorio, by = 'data', all.x = T)
pred_ll <- merge(pred_ll, pred_ll_ob, by = 'data', all.x = T)

pred_ll$pred_obitos <- ceil(pred_ll$predicao * last(quadro_pernambuco$tx_obitos))
pred_ll$pred_uti <- ceil(pred_ll$predicao * last(quadro_pernambuco$tx_uti))

pred_ll <- pred_ll[,c('data', 'dia', 'totalCases', 'predicao', 'deaths', 'pred_obitos', 'pred_uti', 'predicaoOb')]
colnames(pred_ll)[c(3,5)] <- c('confirmados', 'obitos')

pred_ll$pred_dia <- pred_ll$predicao - Lag(pred_ll$predicao, +1)
pred_ll$pred_ob_dia <- pred_ll$predicaoOb - Lag(pred_ll$predicaoOb, +1)

base_pred <- subset(pred_ll, data == '2020-04-19')

idades$pred_obitos <- ceil(base_pred$predicao * idades$chance_obito * idades$propocao)
idades <- idades[, c('faixa_etaria', 'confirmados', 'obitos', 'propocao', 'chance_obito', 'pred_obitos')]
colnames(idades)[c(4,5)] <- c('proporcao', 'letalidade') 

write.table(pred_ll, '../../covidpe/resultado/pred_ll.csv', sep = ';', row.names=FALSE)