# List of specific packages to filter
required_packages <- c("ranger", "randomForest", "arrow", "openxlsx", "writexl", "sqldf", "diffdf")

# Get the list of all installed packages
installed_packages <- installed.packages()

# Filter for the required packages
filtered_packages <- installed_packages[installed_packages[, "Package"] %in% required_packages, ]

# If any of the required packages are not installed, we handle it by creating an empty data frame
if (nrow(filtered_packages) == 0) {
  packages_df <- data.frame(Package = character(0), Version = character(0))
} else {
  # Extract package names and versions
  package_names <- filtered_packages[, "Package"]
  package_versions <- filtered_packages[, "Version"]

  # Combine names and versions into a data frame
  packages_df <- data.frame(Package = package_names, Version = package_versions)
}

# Specify the output file name
output_file <- "../main_R_package_versions.txt"

# Write the data frame to the text file
write.table(packages_df, file = output_file, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)

# Print a message to confirm the file has been saved
cat("Filtered package names and versions have been saved to", output_file, "\n")
