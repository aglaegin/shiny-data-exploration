# Packages ----
list_of_packages <-
  c(
    'checkmate',
    'datasets',
    'DiscriMiner',
    'shinycssloaders',
    'shinydashboard',
    'shinyjs',
    'ggplot2',
    'lsr'
  )
for (package in list_of_packages) {
  if (!(require(package = package, character.only = T))) {
    install.packages(pkgs = package)
    require(package = package, character.only = T)
  }
}
rm(list_of_packages)

# R scripts

currentdir <- getwd()
dir_not_2_source <- c(currentdir, 'old')
file_not_2_source <- c('app', 'source', 'profvis')
file_not_2_source <-
  file.path(currentdir, paste0(file_not_2_source, '.R'))
for (dir in list.dirs(path = getwd())) {
  if (!dir %in% dir_not_2_source) {
    for (file in list.files(dir, full.names = T)) {
      if (substr(x = file,
                 start = nchar(file) - 1,
                 stop = nchar(file))
          == ".R") {
        if (!file %in% file_not_2_source) {
          source(file = file, encoding = 'UTF-8')
        }
      }
    }
  }
}
if (1 == 2) {
  for (file in currentdir) {
    print(file)
    if (substr(x = file,
               start = length(file) - 2,
               stop = length(file))
        == '.R') {
      if (!file %in% file_not_2_source) {
        source(file = file, encoding = 'UTF-8')
      }
    }
  }
}
rm(currentdir, dir, dir_not_2_source, file, file_not_2_source)
gc()