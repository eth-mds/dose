library(ricu)
library(RColorBrewer)
library(VennDiagram)
library(magick)

root <- rprojroot::find_root(".git/index")
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

diff_summary <- function(tbl1, tbl2, plt.name, plot.main) {
  tbl1 <- data.table::setorderv(tbl1, cols = c(id_var(tbl1), index_var(tbl1)))
  tbl2 <- data.table::setorderv(tbl2, cols = c(id_var(tbl2), index_var(tbl2)))
  tbl1 <- tbl1[, head(.SD, n = 1L), by = eval(id_var(tbl1))]
  tbl2 <- tbl2[, head(.SD, n = 1L), by = eval(id_var(tbl1))]

  A <- unique(tbl1[[id_var(tbl1)]])
  B <- unique(tbl2[[id_var(tbl2)]])
  final <- merge(tbl1, tbl2, by = id_var(tbl1), all = T)
  final[id_col(final) %in% intersect(A, B)]
  C <- final[[index_var(tbl1)]] - final[[index_var(tbl2)]]


  myCol <- c("#B3E2CD", "#FDCDAC") # brewer.pal(2, "Pastel2")

  # Chart
  venn.diagram(x = list(A, B), fill = c("lightblue", "green"),
               category.names = c("fluid sampling + ABX", "multi-ABX"),
               alpha = c(0.5, 0.5), lwd=0.1,
               height = 1200,
               width = 1200,
               main.cex = 2,
               cat.pos = c(-20, 40),
               fontfamily = "Helvetica", ## helvetica does not exist :(
               main.fontfamily = "Helvetica",
               sub.fontfamily = "Helvetica",
               cat.fontfamily = "Helvetica",
               resolution = 300,
               main = plot.main,
               compression = "lzw",
               filename = plt.name)

  return(list(A, B, C))

}

si_cmp <- function(src, path, not_surg = FALSE) {

  patient_ids <- id_col(load_concepts("age", src)[age >= 18L])

  apc <- ""
  if (not_surg) {
    patient_ids <- intersect(patient_ids,
                             id_col(load_concepts("adm", src)[adm != "surg"]))
    apc <- " (non-surgical)"
  }

  orig <- load_concepts("susp_inf", src, patient_ids = patient_ids)
  orig <- orig[, meta_vars(orig), with = FALSE]

  abx <- ricu:::si_abx(load_concepts("abx", src, patient_ids = patient_ids),
                       hours(24L), 2L)

  abx <- abx[is_true(abx)]
  abx[, abx := NULL]
  abx <- abx[, meta_vars(abx), with = FALSE]

  orig <- orig[id_col(orig) %in% patient_ids]
  abx <- abx[id_col(abx) %in% patient_ids]

  diff <- diff_summary(orig, abx, path, paste0(srcwrap(src), apc))

}

si_cmp("miiv", file.path(root, "figures", "eFigure2_miiv.tiff"))
si_cmp("aumc", file.path(root, "figures", "eFigure2_aumc.tiff"))
si_cmp("aumc", file.path(root, "figures", "eFigure2_aumc_nonsurg.tiff"), TRUE)


files <- c("eFigure2_miiv.tiff", "eFigure2_aumc.tiff",
           "eFigure2_aumc_nonsurg.tiff")

tiff(file.path(root, "figures", "eFigure2.tiff"), units = "in", width = 21,
     height = 7, res = 300, type = "cairo", compression = "lzw")
par(mfrow = c(1, length(files)))

for (i in 1:length(files)) {
  img <- image_read(file.path(root, "figures", files[i]))
  plot(img)
}
dev.off()

# clean the folder of trash
logs <- grep("\\.log", list.files(file.path(root, "figures")), value = TRUE)
file.remove(file.path(root, "figures", c(files, logs)))
