######################LOG#################################
fit_log <- drm(totalCases ~ dia, fct = L.3(),
             data = pe_relatorio)
summary(fit_log)
plot(fit_log, log="", main = "Curva Logística")

pred_log <- data.frame(predicao = ceil(predict(fit_log, pred_dia)))
pred_log$data <- seq.Date(as.Date('2020-03-12'), by = 'day', length.out = length(pred_dia$dia))

pred_log <- merge(pred_log, pe_relatorio, by = 'data', all.x = T)

write.table(pred_log, '../../covidpe/resultado/pred_log.csv', sep = ';', row.names=FALSE)
