library(ricu)
library(ggplot2)
library(assertthat)
library(matrixStats)
library(magrittr)
library(cowplot)
library(officer)
library(gridExtra)
library(data.table)

root <- rprojroot::find_root(".git/index")
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))
Sys.setenv(RICU_CONFIG_PATH = file.path(root, "config", "custom-dict"))

# biomarkers
cfg <- get_config("features", config_dir())

src <- c("mimic", "miiv")
coh <- lapply(src, function(x) id_col(load_concepts("age", x)[age >= 18]))
names(coh) <- src
coh_dt <- data.table(source = src, size = vapply(coh, length, integer(1L)))
dg <- NULL

for (i in seq_along(cfg)) {
  
  tbl <- load_concepts(names(cfg)[i], src, patient_ids = coh)
  
  id_varr <- setdiff(id_vars(tbl), "source")
  
  stat <- merge(
    tbl[, list(num_ms = length(get(id_varr))), by = "source"],
    tbl[, list(num_pt = length(unique(get(id_varr)))), by = "source"],
    by = "source"
  )
  stat <- merge(stat, coh_dt, by = "source")
  
  stat[, num_ms := ceiling(num_ms / size)]
  stat[, num_pt := round(100 * num_pt / size)]
  
  ttl <- paste0(
    cfg[[i]][["full_name"]], ": ",
    paste0(stat$num_pt, "%", collapse = " to "), "; ",
    paste0(stat$num_ms, collapse = " to "), " measures"
  )
  
  dg[[i]] <- ggplot(tbl, aes_string(x = names(cfg)[i], fill = "source")) +
    geom_density(alpha = 0.3) + ggtitle(ttl) + theme_bw() +
    xlim(quantile(tbl[[names(cfg)[i]]], c(0.02, 0.98)))    
  
}

ggsave(
  filename = "plots.pdf", 
  plot = marrangeGrob(dg, nrow=1, ncol=1), 
  width = 15, height = 9
)

# suspected infection
si <- load_concepts("susp_inf", src, patient_ids = coh)
si[, length(unique(icustay_id)), by = "source"]$V1 / rev(coh_dt$size)

# mortality
table(load_concepts("death", src, patient_ids = coh)$source) / rev(coh_dt$size)

# SOFA score
sf <- load_concepts("sofa", src, patient_ids = coh, keep_components = TRUE,
                    explicit_wins = hours(24L))
sf <- replace_na(sf, 0L)

p_sf <- ggplot(melt(sf[, -c(2, 3, 10)], id.vars = "source"), 
       aes(x = value, fill = source)) +
  geom_histogram(aes(y=..ncount..), position = "dodge") + 
  theme_bw() +
  facet_grid(rows = vars(variable), scales = "free_x")


