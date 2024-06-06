---
output:
  pdf_document: default
  html_document: default
---

## SICdb SOFA Specification

  1. Respiratory
    - PaO2 IDs 689 (compatible with other DBs)
    - FiO2 IDs 2283 (684 additionally?)
    - Mechanical Ventilation -> current best guess FiO2 > 21%
  2. Liver
    - Bilirubin ID 333
  3. Kidney
    - Creatinine ID 367
    - Urine ID 725
  4. Cardio
    - MAP IDs 703, 706 (mixing non-invasive and invasive)
    - Norepinephrine ID 1562
    - Epinephrine ID 1502
    - Dopamine ID 1618
    - Dobutamine ID 1559
    - for weight normalization, use weight from cases table
  5. Coagulation
    - Platelets ID 314
  6. Central Nervous System
    - Glasgow Coma Scale score info missing?
    
### Suspected Infection

  1. Antibiotics: IDs 1406, 1408, 1410, 1418, 1422, 1428, 1431, 1433, 1436, 1449, 1454, 1457, 1458, 1459, 1460, 1461, 1603, 1795, 1913, 1927 using a broad regex

  2. Body fluid sampling: information not present?

for the sedatives:
sic_search("propofol|midazolam|fentanyl|morphine|diazepam|lorazepam|methadone|pentothal|phenobarbiturate|oxazepam|zolpidem|clonazepam|dexmed|cloni|anaconda", ignore.case = TRUE)