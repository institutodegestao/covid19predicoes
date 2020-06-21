max_pred_plato <- subset(pred_sir_comp, pred_sir_comp$I == max(pred_sir_comp$I))
pred_sir_plato <- subset(pred_sir_comp, data <= max_pred_plato$data)

fit_plato <- drm(I ~ time, fct = LL.4(), data = pred_sir_plato)
summary(fit_plato)
plot(fit_plato, log="", main = "Log logistic")

pred_dia_plato = data.frame(dia = pred_sir_plato$time)
pred_seq_plato = data.frame(dia = seq(max(pred_sir_plato$time)+1, max(pred_sir_plato$time)+60))

pred_dia_plato <- rbind(pred_dia_plato, pred_seq_plato)

pred_ll_plato <- data.frame(predicao = ceil(predict(fit_plato, pred_dia_plato)))
pred_ll_plato$data <- seq.Date(as.Date('2020-03-12'), by = 'day', length.out = length(pred_dia_plato$dia))

pred_ll_plato <- merge(pred_ll_plato, pred_sir_plato, by = 'data', all.x = T)

plot(pred_ll_plato$data, pred_ll_plato$predicao, main = "Log logistic Platô")

pred_ll_plato$pred_ob_let_alta <- ceil(pred_ll_plato$predicao * last(quadro_pernambuco$tx_obitos))
pred_ll_plato$pred_ob_let_tend <- ceil(pred_ll_plato$predicao * 0.05)
pred_ll_plato$pred_ob_let_mod <- ceil(pred_ll_plato$predicao * 0.03)
pred_ll_plato$pred_ob_let_baixa <- ceil(pred_ll_plato$predicao * 0.02)

pred_ll_plato$pred_dia <- pred_ll_plato$predicao - Lag(pred_ll_plato$predicao, +1)
pred_ll_plato$ob_alta_dia <- pred_ll_plato$pred_ob_let_alta - Lag(pred_ll_plato$pred_ob_let_alta, +1)
pred_ll_plato$ob_tend_dia <- pred_ll_plato$pred_ob_let_tend - Lag(pred_ll_plato$pred_ob_let_tend, +1)
pred_ll_plato$ob_mod_dia <- pred_ll_plato$pred_ob_let_mod - Lag(pred_ll_plato$pred_ob_let_mod, +1)
pred_ll_plato$ob_baixa_dia <- pred_ll_plato$pred_ob_let_baixa - Lag(pred_ll_plato$pred_ob_let_baixa, +1)

# plot(pred_ll_plato$data, pred_ll_plato$pred_ob_let_alta, main = "Log logistic Platô")

base_obitos_dt_f <- base_obitos_dt[, c(1, 4:9)]

pred_ll_plato <- merge(pred_ll_plato, base_obitos_dt_f, by = 'data', all = T)
