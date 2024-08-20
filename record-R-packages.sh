#!/bin/bash

cd src
if Rscript save_R_package_versions.R; then
    echo "R script ran successfully"
else
    echo "R script failed" >&2
    exit 1
fi

