
root <- rprojroot::find_root(".git/index")
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

# tie all the eFigures into a single file
system(paste(
  "cd scripts &&",
  "pdflatex eFigures.tex &&",
  "mv eFigures.pdf ../4files/supplementary/ &&",
  "rm eFigures.aux eFigures.log"
))

# ex_path <- file.path(root, "figures")
# efig <- list(
#   fig1 = list(
#     cap = paste(
#       "eFig.1: Comparison of sedation and imputation options for the central",
#       "nervous system component. In a post-hoc analysis, we inspected the predictive",
#       "performance of three imputation techniques: (i) setting the GCS score to the",
#       "maximal value whenever the patient is sedated; (ii) for the duration of the",
#       "sedation window, using the latest available GCS value prior to the sedation",
#       "window; (iii) ignoring the sedation information and using the raw GCS values",
#       "as recorded in the databases, and two approaches to sedation: (a) medication",
#       "based; (b) RASS based; (c) ignoring sedation. Our findings show that raw",
#       "GCS values with sedation status not taken into",
#       "account achieve the highest AUC for predicting mortality."
#     ),
#     ratio = 2 / 3
#   ),
#   fig2 = list(
#     cap = paste(
#       "eFig.2: Comparison of the original suspected infection definition with",
#       "the multiple antibiotic definition. Due to missingness of body fluid",
#       "sampling information, an alternative, multiple-antibiotic treatment",
#       "definition was used on the HiRID dataset. We compare the alternative",
#       "definition with the original definition (fluid sampling + antibiotics) on",
#       "the MIMIC-III and AUMC databases. Additionally, we compare the definitions",
#       "on the non-surgical part of the AUMC database.  The overlap of the two",
#       "definitions on the MIMIC-III and non-surgical AUMC cohorts is",
#       "satisfactory (Jaccard similarity 0.81 and 0.78, respectively) and this",
#       "justifies using the alternative definition on the HiRID dataset.",
#       "The overlap on the entire AUMC dataset (Jaccard similarity 0.42) is",
#       "somewhat smaller due to the fact that most of the cohort are surgical",
#       "admissions with antibiotic administrations for prophylaxis."
#     ),
#     ratio = 1 / 3
#   ),
#   fig3 = list(
#     cap = paste(
#       "eFig.3: Over-time performance of DOSE and SOFA scores, with metabolic",
#       "component removed. The two scores are evaluated in terms of area under",
#       "receiver operator characteristic (AUROC) and area under precision recall",
#       "(AUPRC) for predicting mortality,",
#       "during the first day of ICU stay, in time steps of 2 hours, with",
#       "the metabolic component of the DOSE score not included. The 95%",
#       "confidence intervals for the areas under the curve, obtained using",
#       "bootstrap, are plotted in every subplot. The DOSE score outperforms",
#       "SOFA in each metric, time point and dataset."
#     ),
#     ratio = 0.583
#   ),
#   fig4 = list(
#     cap = paste(
#       "eFig.4: Performance of DOSE and SOFA scores at 24 hours into ICU. Each",
#       "component of the DOSE score is compared to the corresponding component",
#       "of the SOFA score, by plotting the receiver operator characteristic",
#       "(ROC) curve at 24 hours into ICU stay. Each",
#       "row of the figure corresponds to an organ failure category",
#       "(cardiovascular, coagulation, hepatic, immunological, metabolic, renal,",
#       "respiratory) and each row corresponds to a dataset. The DOSE score",
#       "outperforms SOFA in almost every component and dataset."
#     ),
#     ratio = 1.424242
#   ),
#   fig5 = list(
#     cap = paste(
#       "eFig.5: Mortality barplots for each component and dataset. For each",
#       "organ failure component,",
#       "we divide the study cohort into groups of patients who had a DOSE",
#       "score of 0, 1, 2, 3, or 4 at 24 hours into ICU,",
#       "and calculated the mortality rate in each group. The same was repeated",
#       "for all the values and components of the SOFA score.",
#       "The mortality rate for each group, dataset and component are presented",
#       "as barplots, where",
#       "each row of the figure corresponds to an organ failure category",
#       "(cardiovascular, coagulation, hepatic, immunological, metabolic, renal,",
#       "respiratory) and each row corresponds to a dataset."
#     ),
#     ratio = 2.1
#   )
# )
#
# supp <- read_docx()
#
# for (i in seq_along(efig)) {
#
#   # add eFigx
#   supp <- body_add_img(supp,
#                        src = file.path(ex_path, paste0("eFigure", i, ".tiff")),
#                        height = min(6.5 * efig[[i]]$ratio, 9.7),
#                        width = 6.5 * min(1, 1.492 / efig[[i]]$ratio))
#   # add caption
#   supp <- body_add(supp, efig[[i]]$cap)
#   # add page break
#   if (i < length(efig)) supp <- body_add_break(supp)
# }
#
# print(supp, target = file.path(ex_path, "eFigures.docx"))
