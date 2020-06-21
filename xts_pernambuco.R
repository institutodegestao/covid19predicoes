####FATI
ob_fato_dia_xts = xts(pred_ll_plato$obitosNovos,order.by=pred_ll_plato$data)

ob_fato_dia_df <- as.data.frame(xts::apply.weekly(ob_fato_dia_xts, mean))

ob_fato_dia_df <- cbind(data = rownames(ob_fato_dia_df), data.frame(ob_fato_dia_df, row.names=NULL))

####RELATÓRIO
ob_real_dia_xts = xts(pred_ll_plato$newDeaths,order.by=pred_ll_plato$data)

ob_real_dia_df <- as.data.frame(xts::apply.weekly(ob_real_dia_xts, mean))

ob_real_dia_df <- cbind(data = rownames(ob_real_dia_df), data.frame(ob_real_dia_df, row.names=NULL))

####ALTA
ob_alta_dia_xts = xts(pred_ll_plato$ob_alta_dia,order.by=pred_ll_plato$data)

ob_alta_dia_df <- as.data.frame(xts::apply.weekly(ob_alta_dia_xts, mean))

ob_alta_dia_df <- cbind(data = rownames(ob_alta_dia_df), data.frame(ob_alta_dia_df, row.names=NULL))

####TEND
ob_tend_dia_xts = xts(pred_ll_plato$ob_tend_dia,order.by=pred_ll_plato$data)

ob_tend_dia_df <- as.data.frame(xts::apply.weekly(ob_tend_dia_xts, mean))

ob_tend_dia_df <- cbind(data = rownames(ob_tend_dia_df), data.frame(ob_tend_dia_df, row.names=NULL))

####MOD
ob_mod_dia_xts = xts(pred_ll_plato$ob_mod_dia,order.by=pred_ll_plato$data)

ob_mod_dia_df <- as.data.frame(xts::apply.weekly(ob_mod_dia_xts, mean))

ob_mod_dia_df <- cbind(data = rownames(ob_mod_dia_df), data.frame(ob_mod_dia_df, row.names=NULL))

####BAIXA
ob_baixa_dia_xts = xts(pred_ll_plato$ob_baixa_dia,order.by=pred_ll_plato$data)

ob_baixa_dia_df <- as.data.frame(xts::apply.weekly(ob_baixa_dia_xts, mean))

ob_baixa_dia_df <- cbind(data = rownames(ob_baixa_dia_df), data.frame(ob_baixa_dia_df, row.names=NULL))

ob_dia_df <- merge(ob_alta_dia_df, ob_tend_dia_df, by = 'data')
ob_dia_df <- merge(ob_dia_df, ob_mod_dia_df, by = 'data')
ob_dia_df <- merge(ob_dia_df, ob_baixa_dia_df, by = 'data')
ob_dia_df <- merge(ob_dia_df, ob_real_dia_df, by = 'data')
ob_dia_df <- merge(ob_dia_df, ob_fato_dia_df, by = 'data')

colnames(ob_dia_df) <- c('data', 'alta', 'tend', 'mod', 'baixa', 'real', 'fato')

ob_dia_df$data <- as.Date(ob_dia_df$data, format = "%Y-%m-%d")

write.table(ob_dia_df, '../../covidpe/resultado/ob_dia_df.csv', sep = ';', row.names=FALSE)
