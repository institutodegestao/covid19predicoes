fit_exp <- drm(totalCases ~ dia, fct = DRC.expoDecay(),
              data = pe_relatorio)
summary(fit_exp)
plot(fit_exp, log="", main = "Power curve")

pred_exp <- data.frame(predicao = ceil(predict(fit_exp, pred_dia)))
pred_exp$data <- seq.Date(as.Date('2020-03-12'), by = 'day', length.out = length(pred_dia$dia))

pred_exp <- merge(pred_exp, pe_relatorio, by = 'data', all.x = T)