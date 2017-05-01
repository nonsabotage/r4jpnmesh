REPOS <- "http://cran.ism.ac.jp"
LIBS  <- c ("readr", "readxl", "purrr", "sf", "dplyr")

for (lib in LIBS) {
    if(!require(lib, character.only=TRUE)) {
        install.packages(lib, repos=REPOS, depend=TRUE)
        require(lib, character.only=TRUE)
    }}

