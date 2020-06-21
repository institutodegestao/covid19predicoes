##########################LM#############################
pe_relatorio$totalCasesLog <- log(pe_relatorio$totalCases)
pe_relatorio$deathsLog <- log(pe_relatorio$deaths)
pe_relatorio$deathsLog[is.infinite(pe_relatorio$deathsLog)] <- 0

fit_lm <- lm(totalCasesLog ~ dia, data = pe_relatorio)
summary(fit_lm)

fit_lm_d <- lm(deathsLog ~ dia, data = pe_relatorio)
summary(fit_lm_d)

pred_lm <- predict(fit_lm, pred_dia, interval = 'confidence')

pred_lm_d <- predict(fit_lm_d, pred_dia, interval = 'confidence')

pred_lm <- as.data.frame(pred_lm)
pred_lm_d <- as.data.frame(pred_lm_d)

pred_lm[, 1:3] <- lapply(pred_lm[, 1:3], exp)
pred_lm[, 1:3] <- lapply(pred_lm[, 1:3], ceil)
pred_lm_d[, 1:3] <- lapply(pred_lm_d[, 1:3], exp)
pred_lm_d[, 1:3] <- lapply(pred_lm_d[, 1:3], ceil)

pred_lm$dia <- pred_dia
pred_lm$data <- seq.Date(as.Date('2020-03-12'), by = 'day', length.out = length(pred_dia$dia))

pred_lm <- merge(pred_lm, pe_relatorio, by = 'data', all.x = T)