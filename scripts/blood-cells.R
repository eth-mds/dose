library(ricu)
library(ggplot2)
library(precrec)
library(assertthat)
library(data.table)

r_dir <- file.path(rprojroot::find_root(".git/index"), "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

cfg <- get_config("features2", config_dir())

times <- hours(24L)

src1 <- "mimic"
src2 <- "aumc"
src3 <- "eicu"
dat1 <- load_cts(src1, cfg, times - hours(24L), times)
dat2 <- load_cts(src2, cfg, times - hours(24L), times)
dat3 <- load_cts(src3, cfg, times - hours(24L), times)


calibration_line <- function(dat, cnc, bins) {
  bin_labels <- function(breaks, unit = "%", right = T) {
    
    breaks <- c(-Inf, breaks, Inf)
    sapply(2:(length(breaks)), function(i) {
      paste0("(", breaks[i-1], ", ", breaks[i], "] ", unit)
    })
    
  }
  dat[, group := .bincode(get(cnc), c(-Inf, bins, Inf), right = T)]
  ggplot(dat[, mean(death), by = "group"], aes(x = group, y = V1)) +
    geom_point() + geom_smooth() + theme_bw() + xlab("Basophil group") + ylab("Mortality") +
    scale_x_continuous(labels=bin_labels(bins), breaks = c(1:(length(bins)+1))) +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, 
        size = 12, hjust = 1), axis.text.y = element_text(size = 12)
    )
  
}

bmc <- cowplot::plot_grid(
  calibration_line(dat1, "basos", seq(0, 1, 0.1)),
  calibration_line(dat2, "basos", seq(0, 1, 0.1)),
  labels = srcwrap(c(src1, src2)), label_x = 0.2, label_y = 0.9
)

ggsave("plots/basophil_mortality.png", plot = bmc, width = 12, height = 6)

correlation_heatmap <- function(src, concepts = c("basos", "lymph", "wbc")) {
  x <- load_concepts(concepts, src)
  x <- x[complete.cases(x)]
  cor_data <- cor(as.matrix(x[, concepts, with = F]))
  cor_data[upper.tri(cor_data)] <- NA
  ggplot(melt(cor_data, na.rm = T), aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(color = "white") + 
    geom_text(aes(Var1, Var2, label = round(value, 2)), color = "black", size = 6) +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
      midpoint = 0, limit = c(-1,1), space = "Lab", 
      name="Correlation") + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
      size = 12, hjust = 1), axis.text.y = element_text(size = 12))
}

p2 <- correlation_heatmap(src2)
p1 <- correlation_heatmap(src1, c("wbc", "neut", "basos", "lymph", "eos"))


crl <- cowplot::plot_grid(
  p1, p2,
  labels = srcwrap(c(src1, src2))
)

ggsave("plots/correlation_heatmap.png", plot = crl, width = 14, height = 6)
