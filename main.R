
root <- rprojroot::find_root(rprojroot::has_file(".gitignore"))

# create folders if needed
if (!dir.exists(file.path(root, "tables")))
  dir.create(file.path(root, "tables"))
if (!dir.exists(file.path(root, "figures")))
  dir.create(file.path(root, "figures"))

# generate cohort
source(file.path(root, "scripts", "cohort-gen.R"))

# construct the score
source(file.path(root, "scripts", "construct-score.R"))

# manuscript numbers
source(file.path(root, "scripts", "summary-stats", "manuscript-numbers.R"))

###' * Tables *

# patient table
source(file.path(root, "scripts", "tables", "patient-table.R"))

# score table
source(file.path(root, "scripts", "tables", "score-table.R"))

###' * eTables *

# variables table
source(file.path(root, "scripts", "tables", "variables-table.R"))

# AUROCs and AUPRCs
source(file.path(root, "scripts", "tables", "aucs.R"))

###' * Figures *

# over time performance
source(file.path(root, "scripts", "figures", "over-time.R"))

###' * eFigures *

# comparison of several GCS options
source(file.path(root, "scripts", "figures", "gcs-options.R"))

# comparison of scores when lactate is removed
source(file.path(root, "scripts", "figures", "over-time-add.R"))

# ROC curves across different datasets and systems
source(file.path(root, "scripts", "figures", "roc-grid.R"))

# mortality barplots in each domain
source(file.path(root, "scripts", "figures", "mortality-barplots.R")) # new

# calibration of SOFA, SOFA 2.0
source(file.path(root, "scripts", "figures", "calibration.R")) # new

### knit supplementary figures to a single file
source(file.path(root, "scripts", "knit-supplement.R")) # new
