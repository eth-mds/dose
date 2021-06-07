library(ricu)
library(ggplot2)
library(precrec)
library(assertthat)
r_dir <- file.path(rprojroot::find_root(".git/index"), "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

cfg <- get_config("features", config_dir())

data_src1 <- "mimic"

cross_train <- F
set.seed(2020)

# train on MIMIC-III at 6 hours
{
  set.seed(2020)
  times <- hours(24L)
  
  mim <- load_data(data_src1, cfg, hours(-Inf), times)
  load("train_1.RData")
  mim <- mim[get(id_var(mim)) %in% train_ids]
  sofa_tbl <- replace_na(load_concepts("sofa", data_src1, keep_components = T), 0)
  sofa_tbl <- sofa_tbl[get(index_var(sofa_tbl)) == hours(24L)]
  mim <- merge(mim, sofa_tbl, all.x = TRUE)
}

systems <- c("cardio", "liver", "cns", "coag", "renal", "resp", "bone_marrow", "metabolic")
best <- lapply(systems, function(x) list(auc = 0.5))
names(best) <- systems

for (sys in systems) {
  
  components <- NULL
  for (i in 1:length(cfg)) 
    if(cfg[[i]][["category"]] == sys) components <- append(components, names(cfg)[i])
    
  for (cp in components) {
    
    print(cp)
    cp.idx <- grep(paste0("^", cp), names(mim))
    mat <- as.matrix(mim[, cp.idx, with=FALSE])
    cpt <- replicate(200, sample(1:ncol(mat), 4, F))
    
    for (k in 1:ncol(cpt)) {
      
      auc <- PRROC::roc.curve(weights.class0 = mim[["death"]], 
                              scores.class0 = rowSums(mat[, cpt[, k]]))$auc
      
      if (auc > best[[sys]][["auc"]]) {
        
        best[[sys]][["auc"]] <- auc
        best[[sys]][["feature"]] <- cp
        best[[sys]][["cols"]] <- sort(cpt[, k] + min(cp.idx) - 3)
        
        if (length(grep(paste0("^sofa.", sys), names(mim))) == 0) {
          sofa.cp <- rep(0, nrow(mim))
          sofa.name <- "Prev. no"
        } else {
          sofa.cp <- mim[[grep(paste0("^sofa.", sys), names(mim))]]
          sofa.name <- "SOFA"
        }
        
        eval <- evalmod(
          scores = list(rowSums(mat[, cpt[, k]]), replace_na(sofa.cp, 0)), 
          labels = list(mim[["death"]], mim[["death"]]),
          dsids = 1:2, modnames = paste(c("DOSE", sofa.name), sys)
        )
        
        best[[sys]][["plot"]]<- autoplot(eval, "ROC") + geom_line(size = 2) + 
                                  theme(legend.position = "bottom", 
                                        legend.text = element_text(size=15), 
                                        plot.title = element_blank())
        
      } 
      
    }
    
  }
    
}

sort(as.vector(sapply(best, `[[`, "feature")))
score.final <- sort(as.vector(sapply(best, `[[`, "cols")))
dput(score.final)

# domain_shift <- function(fts) {
#   
#   sapply(fts, function(f) {
#     vals <- load_concepts(f, "mimic")[[f]]
#     vals2 <- load_concepts(f, "aumc")[[f]]
#     (mean(vals, na.rm = T) - mean(vals2, na.rm = T)) / sd(vals, na.rm = T)
#   })
#   
# }
# 
# plot(domain_shift(sort(as.vector(sapply(best, `[[`, "feature")))))
# abline(h = 0)
