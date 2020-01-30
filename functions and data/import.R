df <- read.csv("Titanic.csv",
               header = T,
               stringsAsFactors = F)
df <- df[, colnames(df) != "index"]
col_num <- c()
col_cat <- c()
for (col in colnames(df)) {
  if (!(all(is.na(as.numeric(df[, col]))))) {
    df[, col] <- as.numeric(df[, col])
    col_num <- c(col_num, col)
  } else {
    col_cat <- c(col_cat, col)
  }
}
target <- colnames(df)[length(colnames(df))]

nbin <- 10