# testthat helper
# box.path : racine du projet pour la resolution des modules box (app/...).
# Convention : les tests sont lancer depuis la racine du projet
# (ex. Rscript -e 'testthat::test_dir("tests/testthat")' depuis la racine).
if (is.null(getOption("box.path"))) {
  options(box.path = getwd())
}
