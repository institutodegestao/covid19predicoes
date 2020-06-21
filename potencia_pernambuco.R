######################LEI DA POTENCIA#####################
fit_lp <- drm(totalCases ~ dia, fct = DRC.powerCurve(),
             data = pe_relatorio)
summary(fit_lp)
#plot(fit_lp, log="", main = "Power curve")

pred_lp <- data.frame(predicao = ceil(predict(fit_lp, pred_dia)))
pred_lp$data <- seq.Date(as.Date('2020-03-12'), by = 'day', length.out = length(pred_dia$dia))

pred_lp <- merge(pred_lp, pe_relatorio, by = 'data', all.x = T)

pred_lp$pred_obitos <- ceil(pred_lp$predicao * last(quadro_pernambuco$tx_obitos))
pred_lp$pred_uti <- ceil(pred_lp$predicao * last(quadro_pernambuco$tx_uti))

pred_lp <- pred_lp[,c('data', 'dia', 'totalCases', 'predicao', 'deaths', 'pred_obitos', 'pred_uti')]
colnames(pred_lp)[c(3,5)] <- c('confirmados', 'obitos')
