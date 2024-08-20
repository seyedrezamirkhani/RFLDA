# RFLDA

Original data and code of RFLDA algorithm is available in `code & data.zip`

The data and code have been re-organised. The data resides in `input_data` folder. The code has been moved to src folder.

The following changes have been made to the original code:

1 - The original code fails to write and read back the LDA object. This is resolved by converting LDA to a data.frame before saving it to disk. Perhaps the code may have worked with the openxlsx before and subsequent changes to this package stopped supporting of writing the matrix to excel?

2 - The openxlsx is very slow at writing xlsx files. Additionally, the file lncRNA-disease-ALL.xlsx cannot be opened with LibreOffice Calc. To resolve this issue, writexl is used instead.

3 - The original code converted LDA into a matrix which is a bug as this dataframe contains two columns of text.

4 - Changed generation of labels for LDExcl0 to use sqldf instead of nested loops changing the time taken from approx. 10 hours to 1 minute.

5 - Switched from RandomForest library to ranger as it supports usage of multiple processor cores.

6 - Added support for using parquet files which are compact and fast to read from.

## R Package info

The full list of R-Packages that were installed on the development machine can be found in **R_package_versions.txt**.

The main R-Packages are:

| Package      | Version |
|--------------|---------|
| arrow        | 16.1.0  |
| diffdf       | 1.0.4   |
| openxlsx     | 4.2.5.2 |
| randomForest | 4.7-1.1 |
| ranger       | 0.16.0  |
| sqldf        | 0.4-11  |
| writexl      | 1.5.0   |

## Dev info

The code was written and tested on Ubuntu 24.04 LTS using R version 4.4.1
