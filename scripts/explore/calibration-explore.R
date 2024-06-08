
root <- rprojroot::find_root(".git/index")
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))
cfg <- get_config("features", config_dir())

# (2) fit the logistic model based on the score
dev_dat <- dat[source %in% c("aumc", "miiv") & split == "dev"]
dev_dat[, weight := ifelse(source == "aumc", 1 / sum(source == "aumc"),
                           1 / sum(source == "miiv"))]
log_reg <- glm(
  death ~ dose + source, family = "binomial",
  data = dev_dat,
  weights = dev_dat$weight
)
summary(log_reg)

cb_map <- as.data.frame(
  cbind(
    dev_dat[, list(prev = mean(death)), by = "source"],
    icept = c(coef(log_reg)["(Intercept)"], coef(log_reg)["(Intercept)"] +
                coef(log_reg)["sourcemiiv"])
  )
)

plot(cb_map$prev, cb_map$icept, pch = 19, xlim = c(0.1, 0.3), ylim = c(-3.5, -2.5))
abline(lm(icept ~ prev, cb_map))

# (3) a more sophisticated approach
icept_to_prev <- function(icept) {

  expit <- function(x) exp(x) / (1 + exp(x))
  mean(expit(dat[source == "sic"]$dose * coef(log_reg)["dose"] + icept))
}

bin_seg <- function(a, b, icept_to_prev, prev, eps = 10^(-3)) {


  mid <- (a + b) / 2
  if (b - a < eps) return(mid)

  prev_star <- icept_to_prev(mid)

  if (prev_star > prev) return(bin_seg(a, mid, icept_to_prev, prev))
  return(bin_seg(mid, b, icept_to_prev, prev))
}

new_prev <- mean(dat[source == "sic"]$death)
new_icept <- bin_seg(-3.5, -2.5, icept_to_prev, new_prev)
points(new_prev, new_icept, pch = 19, col = "red")

# get the predicted intercept just based on prevalence
pred_icept <- predict(lm(icept ~ prev, cb_map),
                      newdata = data.frame(prev = new_prev))

# (4) assess calibration on Salzburg
cb_oob <- dat[source == "sic"]
cb_oob[, proba := expit(dose * coef(log_reg)["dose"] + pred_icept)]
val.prob.ci.2(cb_oob$proba, cb_oob$death)

dose_proba <- dat[split == "dev", list(proba = mean(death), method = "DOSE"),
                  by = c("dose", "source")]
sofa_proba <- dat[split == "dev", list(proba = mean(death), method = "SOFA"),
                  by = c("sofa", "source")]

sofa_proba <- setnames(sofa_proba, "sofa", "score")
dose_proba <- setnames(dose_proba, "dose", "score")


# inspect binomial glm coefficients
ggplot(
  rbind(dose_proba, sofa_proba),
  aes(x = score, y = proba, color = method, linetype = source)
) + geom_point() + geom_line() + theme_bw() +
  theme(legend.position = "bottom") +
  # facet_grid(rows = vars(source)) +
  scale_y_continuous(labels = scales::percent) +
  ylab("Mortality") + xlab("Score Value")

# distribution of the SOFA by source
ggplot(dat, aes(x = dose, fill = source)) + geom_density(alpha = 0.4)




assess_calib(dat, "aumc", "miiv", "dose", model = "glm")

src_cmp <- data.frame(
  score = 0:28, miiv = assess_calib(dat, "miiv", method = "dose",
                                    type = "transform"),
  aumc = assess_calib(dat, "aumc", method = "dose", type = "transform")
)

ggplot(reshape2::melt(src_cmp, id.vars = "score"),
       aes(x = score, y = value, color = variable)) +
  geom_line() + geom_point() + theme_bw()

# fit a logistic model
summary(
  glm(death ~ dose, family = "binomial", data = dat[source == "aumc"])
)

summary(
  glm(death ~ dose, family = "binomial", data = dat[source == "sic"])
)

summary(
  glm(death ~ dose, family = "binomial", data = dat[source == "miiv"])
)

sources <- c("aumc", "sic", "miiv")
results <- data.frame(Source = character(), Coefficient = character(),
                      Estimate = numeric(), SE = numeric())

for (src in sources) {
  model <- glm(death ~ dose, family = "binomial", data = dat[source == src])
  coef_sum <- summary(model)$coefficients
  temp <- data.frame(
    Source = src,
    Coefficient = rownames(coef_sum),
    Estimate = coef_sum[, "Estimate"],
    SE = coef_sum[, "Std. Error"]
  )
  results <- rbind(results, temp)
}

results$Estimate <- results$Estimate + 2 * (results$Coefficient == "(Intercept)")

ggplot(results, aes(x = Coefficient, y = Estimate, fill = Source)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = Estimate - SE, ymax = Estimate + SE), width = 0.2,
                position = position_dodge(width = 0.8)) +
  theme_bw() +
  ggtitle("Calibration: Slope & Intercept comparison") +
  theme(legend.position = "bottom")

ggsave("~/Desktop/dose-calibration.png", width = 6, height = 5)
