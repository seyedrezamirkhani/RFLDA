# RFLDA
Original data and code of RFLDA algorithm is available in `code & data.zip`

The data and code have been re-organised. The data resides in `input_data` folder.
The code has been moved to src folder.

The following changes have been made to the original code:

1 - The original code fails to write and read back the LDA object. This is resolved by converting LDA to a 
data.frame before saving it to disk. Perhaps the code may have worked with the openxlsx before and subsequent
changes to this package stopped supporting of writing the matrix to excel?

2 - The openxlsx is very slow at writing xlsx files. Additionally, the file
lncRNA-disease-ALL.xlsx cannot be opened with LibreOffice Calc. To resolve
this issue, writexl is used instead.

3 - The original code converted LDA into a matrix which is a bug as this dataframe contains two columns of text. 

4 - Changed generation of labels for LDExcl0 to use sqldf instead of nested loops resulting in x60 improvement in speed.

5 - Switched from RandomForest library to ranger as it support usage of multiple processor cores.

6 - Added support for using parquet files which are compact and fast to read from.
