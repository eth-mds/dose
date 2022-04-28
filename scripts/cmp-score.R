
scores <- c("best-marg", "dose-manual", "dose-bestmarg-init", "dose-manual-init")

res <- c()
for (score in scores) {
  
  res <- cbind(res, sort(Reduce(c, unlist(config(score)))))
  
}

res <- as.data.frame(res)
names(res) <- c("marg", "manual", "init_marg", "init_manual")
res
