DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cd "$DIR"

Rscript R/run_analysis.R

git add predictions/predictions.csv
git commit -m "updated prediction"
git push origin master