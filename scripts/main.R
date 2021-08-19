
root <- rprojroot::find_root(rprojroot::has_file(".gitignore"))

# create folders if needed
if (dir.exists(file.path(root, "tables"))) dir.create(file.path(root, "tables"))
if (dir.exists(file.path(root, "figures"))) dir.create(file.path(root, "figures"))

# generate cohort
source(file.path(root, "scripts", "cohort-gen.R"))

# construct the score
# source(file.path(root, "scripts", "construct-score.R"))

# manuscript numbers
source(file.path(root, "scripts", "summary-stats", "manuscript-numbers.R"))

### generate tables

# patient table
source(file.path(root, "scripts", "tables", "patient-table.R"))

# variables table
source(file.path(root, "scripts", "tables", "variables-table.R"))

# score table
source(file.path(root, "scripts", "tables", "score-table.R"))

# AUROCs and AUPRCs
source(file.path(root, "scripts", "tables", "aucs.R"))

### generate figures

# over time performance
source(file.path(root, "scripts", "figures", "over-time.R"))

# ROC curves across different datasets and systems
source(file.path(root, "scripts", "figures", "roc-grid.R"))

# comparison of suspected infection (SI) definitions
source(file.path(root, "scripts", "figures", "si-comparison.R"))

# mortality barplots in each domain
source(file.path(root, "scripts", "figures", "mortality-barplots.R"))

# performance of scores between days 3 and 7
source(file.path(root, "scripts", "figures", "3-to-7.R"))

# comparison of scores when lactate is removed
source(file.path(root, "scripts", "figures", "without-lactate.R"))
