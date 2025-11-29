# https://stackoverflow.com/questions/54634056/how-to-include-an-html-vignette-in-a-binary-r-package



#pkgdown
devtools::document()
pkgdown::build_site()
#pkgdown::build_home()
R.utils::copyDirectory("./docs/", "../DRAT_/html/Ephys4/", overwrite=T)

SOURCE <-devtools::build()
BINARY <- devtools::build(SOURCE, binary = TRUE)
drat::insertPackage(SOURCE, repodir = "../DRAT_/")
drat::insertPackage(BINARY, repodir = "../DRAT_/")
available.packages(repos="file:///C:/Users/danker/Documents/R_Projekte/PACKAGES/DRAT_/")[,1:3]



drat_repo <- git2r::repository("../DRAT_/")
git2r::status(drat_repo)
git2r::add(drat_repo, "*")
git2r::status(drat_repo)
git2r::commit(drat_repo,  stringr::str_remove(basename(BINARY), ".zip") ) 
# git2r::commit(drat_repo,  "update" ) 



# if connected to NMI-intranet:
git2r::pull(drat_repo)
git2r::push(drat_repo)

R_REPO <- git2r::repository("I://PharmaBiotech/1033_Ephys/R_PACKAGES/")
git2r::pull(R_REPO)
git2r::push(R_REPO)

available.packages(repos="file:///I:/PharmaBiotech/1033_Ephys/R_PACKAGES/")[,1:3]

