cfg <- get_config("features", config_dir())

src <- c("miiv", "aumc", "hirid")

train_time <- hours(24L) # hours(seq.int(24, 120, 24))
train <- list()
for (i in seq_along(train_time)) {
  for (j in seq_along(src)) {
    train[[length(src) * (i-1) + j]] <-
      load_data(src[j], cfg, train_time[i] - 24L, train_time[i],
                cohort = config("cohort")[[src[j]]][["all"]], enc = FALSE)
  }
}

feats <- c("aptt_inr", "inr_pt", "plt", "plt_div_inr")
sgn <- c(1, 1, -1, -1)
full_names <- c("APTT x INR", "INR(PT)", "Platelets", "Platelets / INR(PT)")
names(sgn) <- names(full_names) <- feats
plt <- list()
for (feat in feats) {
  
  scr <- lapply(train, `[[`, feat)
  scr <- lapply(scr, function(x) x * sgn[feat])
  evl <- evalmod(
    scores = scr,
    labels = lapply(train, `[[`, "death"),
    dsids = 1:3,
    modnames = srcwrap(src)
  )
  print(round(auc_ci(evl)$mean[c(T, F)], 2))
  
  plt[[feat]] <- autoplot(evl, curvetype = "ROC") + 
    ggtitle(paste(full_names[feat], "ROCs")) +
    theme(legend.position = "bottom")
}

cowplot::plot_grid(plotlist = plt, ncol = 2L)
ggsave("coag_variables.png", width = 12, height = 5)


Reduce(
  function(x) {
    data.frame(val = x, src = src[i])
  }, 
)

vals <- lapply(train, `[[`, "aptt_inr")
plot(density(vals[[1]]), xlim = c(0, 200))
lines(density(vals[[2]]), col = "red")
lines(density(vals[[3]]), col = "blue")

ggplot(load_concepts("ptt", src), aes(x = ptt, fill = srcwrap(source))) +
  geom_density(alpha = 0.4) + theme_bw() + ggtitle("APTT density") +
  xlab("APTT (seconds)") + theme(legend.position = c(0.8, 0.8)) +
  scale_fill_discrete(name = "Dataset")