library(dplyr)
library(readr)

prices <- read_rds('static/data/seligson.rds')

library(ggplot2)

ggplot(prices %>%
         filter(fund %in% c('suomi_exc',
                            'rahamarkkina_exc')),
       aes(x = date, y = prc, color = fund)) +
  geom_line() +
  theme(legend.position = 'bottom') +
  labs(x = 'Aika', y = 'Hinta', color = 'Rahasto') +
  scale_color_brewer(palette = 'Set1')

prices %>%
  group_by(fund) %>%
  summarise(start_date = min(date)) %>%
  ungroup() %>%
  arrange(start_date) %>%
  rename(Rahasto = fund, `Aloituspäivä` = start_date) %>%
  knitr::kable()

library(lubridate)

ret <- prices %>%
  mutate(month = 100*year(date) + month(date)) %>%
  group_by(fund, month) %>%
  slice(n()) %>%
  ungroup() %>%
  group_by(fund) %>%
  mutate(ret = prc/lag(prc) - 1) %>%
  ungroup() %>%
  filter(!is.na(ret),
         fund %in% c('suomi_exc', 'eurooppa_exc', 'global-brands_exc', 'euroobligaatio_exc'),
         month >= 199811,
         month <= 201902) %>%
  mutate(date = ceiling_date(date, 'month') - 1) %>%
  select(date, fund, ret)

library(tidyr)

ret_wide <- ret %>%
  spread(fund, ret)

wgt_iv <- matrix(0, nrow = nrow(ret_wide), ncol = ncol(ret_wide) - 1, dimnames = list(NULL, colnames(ret_wide)[-1]))
wgt_mv <- matrix(0, nrow = nrow(ret_wide), ncol = ncol(ret_wide) - 1, dimnames = list(NULL, colnames(ret_wide)[-1]))
port_ret <- matrix(0, nrow = nrow(ret_wide), ncol = 3, dimnames = list(NULL, c('EW', 'IV', 'MV')))

library(nloptr)
library(pracma)

minvar <- function(V) {
  N <- ncol(V)
  fn <- function(x, V) as.numeric(t(x) %*% V %*% x)
  gr <- function(x, V) grad(fn, x, V = V)
  eq <- function(x, V) sum(x) - 1
  jeq <- function(x, V) jacobian(eq, x, V = V)
  res <- nloptr(x0 = rep(1/N, N),
         eval_f = fn,
         eval_grad_f = gr,
         lb = rep(0, N),
         ub = rep(1, N),
         eval_g_eq = eq,
         eval_jac_g_eq = jeq,
         opts = list(algorithm = 'NLOPT_LD_SLSQP', xtol_rel = 1e-4),
         V = V)
  stopifnot(res$status >= 0)
  #cat(res$message, '\n')
  setNames(res$solution, colnames(V))
}

L <- 36

for (i in (L+1):nrow(ret_wide)) {
  ret <- ret_wide[(i-L):(i-1), -1]
  V <- cov(ret)
  sig <- sqrt(diag(V))
  wgt_iv[i, ] <- (1 / sig) / sum(1 / sig)
  wgt_mv[i, ] <- minvar(V)
  port_ret[i, 'EW'] <- apply(ret_wide[i, -1], 1, mean)
  port_ret[i, 'IV'] <- sum(wgt_iv[i, ] * ret_wide[i, -1])
  port_ret[i, 'MV'] <- sum(wgt_mv[i, ] * ret_wide[i, -1])
}

ggplot(cbind(select(ret_wide, date), wgt_iv) %>%
         slice(L:n()) %>%
         gather(fund, weight, -date),
       aes(x = date, y = weight, fill = fund)) +
    geom_area(position = 'stack') +
    labs(x = 'Aika', y = 'Paino', fill = 'Rahasto') +
    theme(legend.position = 'bottom') +
    scale_fill_brewer(palette = 'Set1')

ggplot(cbind(select(ret_wide, date), wgt_mv) %>%
         slice(L:n()) %>%
         gather(fund, weight, -date),
       aes(x = date, y = weight, fill = fund)) +
  geom_area(position = 'stack') +
  labs(x = 'Aika', y = 'Paino', fill = 'Rahasto') +
  theme(legend.position = 'bottom') +
  scale_fill_brewer(palette = 'Set1')

ggplot(cbind(ret_wide %>% select(date), port_ret) %>%
         slice(L:n()) %>%
         gather(port, ret, -date) %>%
         group_by(port) %>%
         mutate(cret = cumprod(1 + coalesce(ret, 0))) %>%
         ungroup(),
       aes(x = date, y = cret, color = port)) +
    geom_line() +
    labs(x = 'Aika', y = 'Kumulatiivinen tuotto', color = 'Strategia') +
    theme(legend.position = 'bottom') +
    scale_color_brewer(palette = 'Set1')

cbind(ret_wide %>% select(date), port_ret) %>%
  slice(L:n()) %>%
  gather(port, ret, -date) %>%
  group_by(port) %>%
  summarise(mean = 12*100*mean(ret),
            sd = sqrt(12)*100*sd(ret),
            cret = prod(1 + ret)) %>%
  ungroup() %>%
  rename(Strategia = port, Keskituotto = mean, Volatiliteetti = sd, `Arvo lopussa` = cret)

save(prices, L, ret_wide, wgt_iv, wgt_mv, port_ret, file = 'static/data/minimivarianssiportfolio.RData')
