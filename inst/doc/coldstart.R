## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
library(coldstart)
sim <- cs_simulate(n_train = 400, n_new = 150, seed = 3)
it <- sim$items
tr <- it$set == "train"
table(it$set)

## -----------------------------------------------------------------------------
pr <- cs_predictor(it$b_legacy[tr], sim$features[tr, ], it$family[tr], seed = 1)
pr
pred <- predict(pr, sim$features[!tr, ], it$family[!tr])
head(pred)

## -----------------------------------------------------------------------------
plan <- cs_plan(pred, target_sd = 0.3)
summary(plan[c("n_with_prior", "n_without_prior")])

## -----------------------------------------------------------------------------
resp <- cs_responses(sim, n_per_item = 25, seed = 4)
cal <- cs_calibrate(resp, pred)
truth <- it$b_true[match(cal$item, it$item)]
c(baseline = sqrt(mean((cal$base_mean - truth)^2)),
  with_prior = sqrt(mean((cal$post_mean - truth)^2)))

## -----------------------------------------------------------------------------
fam <- setNames(it$family[!tr], it$item[!tr])
chk <- cs_check(cs_calibrate(cs_responses(sim, 100, seed = 5), pred), fam)
chk
unique(it$family[it$rogue])

## -----------------------------------------------------------------------------
resp100 <- cs_responses(sim, 100, seed = 5)
final <- cs_calibrate(resp100, cs_distrust(pred, chk))
head(final[c("item", "n", "post_mean", "post_sd")])

