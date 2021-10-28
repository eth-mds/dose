
x <- concept_availability()

colnames(x)



grep("vent|trach|mech", names(get_config("concept-dict")),
     value = TRUE)

dput(
  as.numeric(get_config("concept-dict")[["vent_start"]][["sources"]][["mimic"]][[1]][["ids"]])
)

src <- c("aumc", "hirid", "miiv")

mm <- dput(
  get_config("concept-dict")[["vent_start"]][["sources"]]["mimic"][[1]][[1]][["ids"]]
)

subset(miiv$d_items, itemid %in% mm)[, c("itemid", "label"), with=FALSE]


trach <- subset(aumc$processitems, grepl("beademen", item, ignore.case = TRUE))
trach[, dur := (stop - start)/(1000 * 60)]
ggplot(trach, aes(x = as.numeric(dur), fill = factor(itemid))) + 
  geom_density() + theme_bw() + xlim(c(0, 10000))

# HiRID analysis

tbl <- subset(hirid$observations, variableid == 15001552)
tbl <- tbl[, c("datetime", "patientid", "value"), with = FALSE]
tbl <- setorderv(tbl, cols = "datetime")

tbl[, endtime := shift(datetime, n = -1L), by = "patientid"]
tbl[, mode_end := shift(value, n = -1L), by = "patientid"]

ggplot(
  tbl[, mean(endtime - datetime, na.rm = TRUE), by = c("value", "mode_end")],
  aes(x = factor(mode_end), y = factor(value), fill = as.numeric(V1) / 3600)
) + geom_tile() + theme_bw() + scale_fill_viridis_c(name = "Duration") +
  ggtitle("Time between consecutive entries") +
  scale_y_discrete(labels = hirid$ordinal[67:72, ]$stringvalue) +
  scale_x_discrete(labels = c(hirid$ordinal[67:72, ]$stringvalue, "none")) +
  geom_text(
    data = tbl[, .N, by = c("value", "mode_end")],
    aes(label = N, fill = NULL), color = "red"
  ) + ylab("Entry") + xlab("Follow-up entry")


# MIMIC-IV P(land in interval)

tbl <- load_concepts(c("mech_vent", "alistart"), "miiv")

tbl <- tbl[!is.na(alistart)]

100 * table(!is.na(tbl$mech_vent)) / sum(table(!is.na(tbl$mech_vent)))

# MIMIC-III
subset(
  mimic$d_items, 
  itemid %in% get_config("concept-dict")[["vent_start"]][["sources"]]["mimic"][[1]][[1]][["ids"]]
)

subset(
  mimic$d_items,
  grepl("ventil", label, ignore.case = TRUE) & itemid < 200000)
)

vnt <- load_concepts("alistart", "mimic")
vnt[, end := shift(get(index_var(vnt)), n = -1L), by = c(id_var(vnt))]
vnt[, dur := end - get(index_var(vnt))]

vnt <- load_concepts(c("alistart", "vent_mode"), "mimic")

vnt[!is.na(vent_mode), avl := as.numeric(charttime)]

vnt[, avl_f := nafill(avl, type = "nocb"), by = "icustay_id"]

quantile(vnt[!is.na(alistart), avl_f - as.numeric(charttime)],
         na.rm = TRUE, probs = seq(0.01, 0.99, 0.01))

vmod <- subset(mimic$chartevents, itemid == 720)
table(vmod$value) # nothing non-invasive here...

# eICU
vnt <- eicu$respiratorycare[, 
                            c("patientunitstayid", "ventstartoffset", 
                              "ventendoffset", "priorventstartoffset",
                              "priorventendoffset", )]

vnt[, lapply(.SD, function(x) 100 * mean(x != 0))]

tbl <- subset(eicu$respiratorycharting, respcharttypecat %in% 
         get_config("concept-dict")[["vent_start"]][["sources"]]["eicu"][[1]][[3]][[1]])


# alistart vs. prior windows

rcare <- load_concepts("mech_vent", "eicu")
rcare <- rcare[dur_var > 0]

rcare <- merge(expand(rcare), load_concepts("alistart", "eicu"), all = TRUE)

table(is.na(rcare$alistart), is.na(rcare$mech_vent))

vnt <- load_concepts("alistart", "eicu")
vnt[, end := shift(get(index_var(vnt)), n = -1L), by = c(id_var(vnt))]
vnt[, dur := end - get(index_var(vnt))]

quantile(as.numeric(vnt$dur), na.rm = TRUE, probs = seq(0.8, 0.99, 0.01))
