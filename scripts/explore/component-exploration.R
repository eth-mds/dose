library(ricu)
library(ggplot2)
library(precrec)
library(assertthat)
library(data.table)

r_dir <- file.path(rprojroot::find_root(".git/index"), "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

cfg <- get_config("features2", config_dir())
set.seed(2020)

times <- hours(24L)

component_plots <- function(dat, data_src, times) {
  
  direc <- function(x) ifelse(is.null(cfg[[x]]), 1L, 1 - 2*(cfg[[x]][["direction"]] == "decreasing"))
  
  grid <- list(
    #liver = c("alb", "ast", "alt", "bili", "alp", "bili_min_alb", "alb_div_bili", "ast_x_bili"),
    #cardio = c("map", "sbp", "dbp", "map50", "map100", "map200"),
    bone_marrow = c("basos", "eos", "lymph", "neut", "neut_div_lymph")#,
    #coag = c("inr_pt", "pt", "ptt", "plt", "plt_div_pt", "ptt_div_plt", "ptt_x_pt"),
    #renal = c("bun", "crea"),
    #cns = c("gcs", "gcs_raw")
  )
  
  translate <- list(alb = "Albumin", ast = "AST", alt = "ALT", bili = "Bilirubin", 
    alp = "Alkaline Phos.", alb_p_bili = "Albumin + Bili", alb_x_bili = "Albumin x Bili", 
    basos = "Basophils", eos = "Eosinophils", inr_pt = "INR", 
    pt = "Prothrombine Time", ptt = "Partial PT", map = "MAP", plt = "Platelets",
    sbp = "SBP", dbp = "DBP", map50 = "MAP - 50*norepi", map100 = "MAP - 100*norepi", 
    map200 = "MAP - 200*norepi", bili_min_alb = "Bilirubin - Albumin", 
    alb_div_bili = "Albumin / Bilirubin",
    ast_x_bili = "AST times Bili", bun = "Urea Nitrogen", crea = "Creatinine", 
    plt_div_pt = "Platelets / PT",
    ptt_div_plt = "APTT / Platelets", ptt_x_pt = "APTT times PT", gcs = "GCS (non-sedated)", 
    gcs_raw = "GCS (raw)", lymph = "Lymphocytes", neut = "Neutrophils", 
    neut_div_lymph = "Neutrophils / Lymphocytes")
  
  # dat[, bili_min_alb := (bili-alb)]
  # dat[, alb_div_bili := -(alb/bili)]
  # dat[, ast_x_bili := (ast*bili)]
  # dat[, plt_div_pt := -(plt/pt)]
  # dat[, ptt_div_plt := (ptt/plt)]
  # dat[, ptt_x_pt := (pt * ptt)]
  plots <- list()
  
  for (i in 1:length(grid)) {
    
    sys <- names(grid)[i] 
    
    if (length(grep(paste0("^sofa.", sys), names(dat))) == 0) {
      sofa.cp <- rep(0, nrow(dat))
      sofa.name <- paste("Prev. no", sys)
    } else {
      sofa.cp <- dat[[grep(paste0("^sofa.", sys), names(dat))]]
      sofa.name <- paste("SOFA", sys) 
    }
    
    scores <- lapply(grid[[i]], function(x) dat[[x]]*direc(x))
    scores <- append(scores, list(sofa.cp))
    labels <- lapply(1:length(scores), function(i) dat[["death"]])
    modnames <- c(sapply(grid[[i]], function(x) translate[[x]]), sofa.name)
   
    eval <- evalmod(
      scores = scores, 
      labels = labels,
      dsids = 1:length(scores), 
      modnames = modnames
    )
    
    plot.title <- stringr::str_to_upper(data_src)
    plots[[data_src]][[sys]] <- autoplot(eval, "ROC") + 
      geom_line(size = 0.5) + ggtitle(paste(plot.title, sys))
  }
  
  plots
  
}

src1 <- "mimic"
src2 <- "aumc"
dat1 <- load_cts(src1, cfg, times - hours(24L), times)
dat2 <- load_cts(src2, cfg, times - hours(24L), times)
plt <- component_plots(dat1, src1, hours(24L))
plt2 <- component_plots(dat2, src2, hours(24L))

sys_select <- "bone_marrow" #c("liver", "cardio", "bone_marrow", "coag", "renal", "cns")
final_plots <- lapply(sys_select, 
  function(s) cowplot::plot_grid(plt[[src1]][[s]], plt2[[src2]][[s]]))
names(final_plots) <- sys_select

for (sys in sys_select) {
  
  ggsave(paste0(sys, ".png"), plot = final_plots[[sys]], height = 6, width = 10)
  
}

# miss_summary <- function(cnc, src, patient_ids = si_cohort(src)) {
#   
#   tbl <- load_concepts(cnc, src, patient_ids = patient_ids)
#   
#   print(paste("Proportion with at least one measurement", 
#     round(100*length(unique(id_col(tbl)))/length(patient_ids), 2), "%"))
#   
#   tbl <- tbl[get(index_var(tbl)) < hours(24L)]
#   
#   print(paste("Proportion with a measurement before 24 hours", 
#     round(100*length(unique(id_col(tbl)))/length(patient_ids), 2),"%"))
#   
#   tbl <- tbl[get(index_var(tbl)) >= hours(0L)]
#   
#   print(paste("Proportion with a measurement between 0 and 24 hours", 
#     round(100*length(unique(id_col(tbl)))/length(patient_ids), 2),"%"))
#   
# }
# 
# miss_summary("basos", "mimic")
# miss_summary("basos", "aumc")
# 
# miss_summary("lymph", "mimic")
# miss_summary("lymph", "aumc")
