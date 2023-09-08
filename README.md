# Analysis of crimes related to domestic violence and rape committed in the municipality of Aguascalientes

This project was done in conjuction with the Aguascalientes State Attorney General's Office (Fiscalíaa General del Estado de Aguascalientes) using the R programming language. The data files are not available because they are property of the  Aguascalientes State Attorney General's Office.

## Context

In the municipality of Aguascalientes, there has been a high level of impunity for two types of crimes: Rape and Domestic violence. According to the Mesa Ciudadana de Seguridad del Estado, impunity in these cases is measured by the number of open criminal investigation files (CIFs) that have not been linked to the legal process or have initiated it but there is no verdict.

## Objective 

Calculate the number of CIFs opened during the 2020-2022 period, those linked to the legal process and given a verdict, and quantify the time it takes for CIFs to be linked to the legal process.

## Data 

The State Attorney General's Office provided a dataset consisting of 6,314 records related to rape offenses and domestic violence crimes that occurred between 2020 and 2022. The original variables and their corresponding names are as follows:

- Alphanumeric ID for the CFI (carpeta)
- Type of crime (delito)
- Date when the CFI was opened (fecha)
- Brief description of the crime (sintesis)
- Court name (juzgado)
- Date when the CFI was linked to the legal process (fecha_proceso)
- Date when the CFI received a verdict (fecha_sentencia)

### Cleaning

The data cleaning consisted on:

1. Checked for the presence of missing values (NAs).
2. Extracted the numeric ID from the alphanumeric ID (carpeta).
3. Identified the municipality where the crime occurred.
4. Removed the year from the court record ID (exp_jud).
5. Transformed the date variables from character type to date type.
6. Calculated the time difference (in days) between the date when the CFI was opened and the date when the CFI was linked to the legal process.
7. Calculated the time difference (in days) between the date when the CFI was linked to the process and the date when the CFI received a verdict.
8. Calculated the time difference (in days) between the date when the CFI was opened and the date when the CFI received a verdict.
9. Ordered the dataset by numeric CFI ID.
