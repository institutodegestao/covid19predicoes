##########################PE##############################
Infected <- pe_relatorio$totalCases
Day <- 1:(length(Infected))
N <- 9557071 # population of mainland china

SIR <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  with(par, {
    dS <- -beta/N * I * S
    dI <- beta/N * I * S - gamma * I
    dR <- gamma * I
    list(c(dS, dI, dR))
  })
}

library(deSolve)
init <- c(S = N-Infected[1], I = Infected[1], R = 0)
RSS <- function(parameters) {
  names(parameters) <- c("beta", "gamma")
  out <- ode(y = init, times = Day, func = SIR, parms = parameters)
  fit <- out[ , 3]
  sum((Infected - fit)^2)
}

Opt <- optim(c(0.5, 0.5), RSS, method = "L-BFGS-B", lower = c(0.1, 0.1), upper = c(0.99, 0.99)) 

Opt_par <- setNames(Opt$par, c("beta", "gamma"))
Opt_par

beta1 <- as.numeric(unname(Opt_par)[1])
gama1 <- as.numeric(unname(Opt_par)[2])

Opt2 <- GenSA(par = c(beta1, gama1), lower = c(0.1, 0.1), upper = c(0.99, 0.99), fn = RSS, control=list(max.time=10, verbose=TRUE, seed = -1))

Opt2_par <- setNames(Opt2$par, c("beta", "gamma"))
Opt2_par

t <- 1:150
pred_sir_comp <- data.frame(ode(y = init, times = t, func = SIR, parms = Opt2_par))
pred_sir_comp[,3:4] <- lapply(pred_sir_comp[,3:4], ceil)
pred_sir_comp$data <- seq.Date(as.Date('2020-03-12'), by = 'day', length.out = length(t))
pred_sir_comp <- merge(pred_sir_comp, pe_relatorio, by = 'data', all.x = T)

R0 <- setNames(Opt2_par["beta"] / Opt2_par["gamma"], "R0")
R0

reproducao_basica <- as.data.frame(R0)

write.table(reproducao_basica, '../../covidpe/resultado/r0.csv', sep = ';', row.names=FALSE)

max_sir_comp <- pred_sir_comp[pred_sir_comp$I == max(pred_sir_comp$I), "I", drop = FALSE] 

max_sir_comp_d <- max(pred_sir_comp$I) * 0.02

pred_sir_comp$pred_ob_let_alta <- ceil(pred_sir_comp$I * last(quadro_pernambuco$tx_obitos))

pred_sir_comp$pred_ob_let_tend <- ceil(pred_sir_comp$I * 0.05)

pred_sir_comp$pred_ob_let_mod <- ceil(pred_sir_comp$I * 0.03)

pred_sir_comp$pred_ob_let_baixa <- ceil(pred_sir_comp$I * 0.02)

pred_sir_comp_filter <- pred_sir_comp[,c('data', 'time', 'I', 'totalCases', 'deaths', 'pred_ob_let_baixa', 'pred_ob_let_mod', 'pred_ob_let_alta', 'pred_ob_let_tend')]
colnames(pred_sir_comp_filter)[c(4,5)] <- c('confirmados', 'obitos')

pred_sir_ext <- subset(pred_sir_comp, pred_sir_comp$I == max(pred_sir_comp$I))

write.table(pred_sir_ext, '../../covidpe/resultado/pred_sir_ext.csv', sep = ';', row.names=FALSE)
