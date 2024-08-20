write.table(installed.packages()[, c("Package", "Version")], file = "../R_package_versions.txt", row.names = FALSE, quote = FALSE, sep = "\t")
