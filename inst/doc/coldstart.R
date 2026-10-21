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

## -----------------------------------------------------------------------------
unique(pred[c("family_seen", "sd", "sd_shared")])

## -----------------------------------------------------------------------------
sim2 <- cs_simulate(n_train = 400, n_new = 150, seed = 110)
it2 <- sim2$items; tr2 <- it2$set == "train"
pr2 <- cs_predictor(it2$b_legacy[tr2], sim2$features[tr2, ], it2$family[tr2], seed = 10)
pred2 <- predict(pr2, sim2$features[!tr2, ], it2$family[!tr2])
cal2 <- cs_calibrate(cs_responses(sim2, 100, seed = 10), pred2)
fam2 <- setNames(it2$family[!tr2], it2$item[!tr2])
c(unseen = unique(it2$family[it2$unseen_family]), drifted = unique(it2$family[it2$rogue]))

correlated <- cs_check(cal2, fam2)
independent <- cs_check(transform(cal2, prior_sd_shared = 0), fam2)
correlated[!correlated$trustworthy, c("family", "n_items", "mean_z", "p_value")]
independent[!independent$trustworthy, c("family", "n_items", "mean_z", "p_value")]

