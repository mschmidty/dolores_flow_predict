#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cd "$DIR"

/usr/local/bin/R < R/run_analysis.R --no-save

git add predictions/predictions.csv
git commit -m "updated prediction"
git push origin master