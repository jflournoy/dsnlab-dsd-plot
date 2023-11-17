library(ggplot2)
library(data.table)
library(brms)

#dsd <- fread('disclosure_data_waves123_20210617.csv')

N_id = 100
N_trial = 100
N_wave = 3

fake_dsd <- data.table(sv = sample(-2:2, 2*N_id*N_trial*N_wave, replace = TRUE))
fake_dsd[, id := rep(rep(1:N_id, each = N_trial), 2*N_wave)]
fake_dsd[, wave := rep(1:N_wave, each = N_id*N_trial*2)]
fake_dsd[, intercept := rep(rep(c(-.25, -.75), each = N_trial*N_id), N_wave)]
fake_dsd[, slope := rep(rep(c(2, 1), each = N_trial*N_id), N_wave)]
fake_dsd[, pshare := plogis(intercept + slope * sv)]
fake_dsd[, shared := rbinom(.N, size = 1, prob = pshare)]
fake_dsd[, cond := rep(rep(c('sup', 'int'), each = N_id*N_trial), N_wave)]

m <- brm(shared ~ 1 + cond*sv*wave + (1 | id), data = fake_dsd, family = brms::bernoulli(),
         backend = 'cmdstan', file = 'fake_dsd', silent = 0, file_refit = 'always',
         chains = 4, cores = 4)

summary(m)

newdata <- data.table(expand.grid(cond = c('sup', 'int'), wave = 1:3, sv = seq(-2, 2, length.out = 100)))
newdata[, id := 1]
y_hat_se <- posterior_summary(posterior_epred(m, newdata = newdata))

newdata <- cbind(newdata, y_hat_se)

dot_data <- fake_dsd[, .(N = .N), by = c('cond', 'sv', 'shared', 'wave')]

ggplot(newdata, aes(x = sv, y = Estimate)) + 
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5, fill = cond, group = cond), alpha = .5) + 
  geom_line(aes(group = cond, color = cond)) + 
  geom_point(data = dot_data, aes(size = N, x = sv, y = shared, color = cond),
             position = position_dodge(width = .25)) +  
  scale_color_manual(breaks = c('sup', 'int'), values = c('#666666', '#000000'), aesthetics = c('color', 'fill')) + 
  facet_grid(~ wave, labeller = labeller(wave = c('1' = 'T1', '2' = 'T2', '3' = 'T3'))) + 
  theme_minimal() + 
  theme(panel.grid = element_blank(), strip.background = element_blank())
