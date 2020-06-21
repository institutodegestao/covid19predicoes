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
Opt$message

Opt_par <- setNames(Opt$par, c("beta", "gamma"))
Opt_par

pred_sir <- data.frame(ode(y = init, times = pred_dia$dia, func = SIR, parms = Opt_par))
pred_sir[,3:4] <- lapply(pred_sir[,3:4], ceil)
pred_sir$data <- seq.Date(as.Date('2020-03-12'), by = 'day', length.out = length(pred_dia$dia))
pred_sir <- merge(pred_sir, pe_relatorio, by = 'data', all.x = T)

