wd <- getwd()
setwd(wd)

maxfunction <- function(df) {
  y <- max(df)
  x <- name[df==y]
  cat("Max: ", y, "- Projeto:", x, "\n")
}

minfunction <- function(df) {
  y <- min(df)
  x <- name[df==y]
  cat("Min: ", y, "- Projeto:", x, "\n")
}

meanfunction <- function(df) {
  y <- mean(df)
  cat("Mean:", y, "\n")
}

best <- function(){
  x <- rowSums(values[,-1])
  y <- name[x==max(x)]
  cat("THE BEST PROJECT WAS", y,"\n")
}

worst <- function(){
  x <- rowSums(values[,-1])
  y <- name[x==min(x)]
  cat("THE WORST PROJECT WAS", y,"\n")
}




args <- commandArgs(trailingOnly = TRUE)

if (is.na(args[1]) || is.na(args[2])){
  cat("Give me some arguments!")
} else{

  values <- read.csv(args[1])
  attach(values)

  png(file=paste(args[2],"/bugs.png", sep=""))
  hist(bugs, xlab="Quantity", main="Bugs")
  invisible(dev.off())

  png(file=paste(args[2],"/code_smells.png", sep=""))
  hist(code_smells, xlab="Quantity", main="Code Smells")
  invisible(dev.off())

  png(file=paste(args[2],"/technical_debt.png", sep=""))
  hist(sqale_index, xlab="Work days", main="Technical Debt")
  invisible(dev.off())

  png(file=paste(args[2],"/duplicated_blocks.png", sep=""))
  hist(duplicated_blocks, xlab="Quantity", main="Duplicated Blocks")
  invisible(dev.off())

  png(file=paste(args[2],"/duplicated_lines_density.png", sep=""))
  hist(duplicated_lines_density, xlab="Quantity", main="Duplicated lines of code")
  invisible(dev.off())

  png(file=paste(args[2],"/complexity.png", sep=""))
  hist(complexity, xlab="Quantity", main="Cyclomatic complexity")
  invisible(dev.off())

  cat("BUGS\n")
  maxfunction(bugs)
  minfunction(bugs)
  meanfunction(bugs)
  cat("\n")

  cat("CODE SMELLS\n")
  maxfunction(code_smells)
  minfunction(code_smells)
  meanfunction(code_smells)
  cat("\n")

  cat("TECHNICAL DEBT\n")
  maxfunction(sqale_index)
  minfunction(sqale_index)
  meanfunction(sqale_index)
  cat("\n")

  cat("DUPLICATED BLOCKS\n")
  maxfunction(duplicated_blocks)
  minfunction(duplicated_blocks)
  meanfunction(duplicated_blocks)
  cat("\n")

  cat("DUPLICATED LINES OF CODE\n")
  maxfunction(duplicated_lines_density)
  minfunction(duplicated_lines_density)
  meanfunction(duplicated_lines_density)
  cat("\n")

  cat("CYCLOMATIC COMPLEXITY\n")
  maxfunction(complexity)
  minfunction(complexity)
  meanfunction(complexity)
  cat("\n")

  best()
  cat("\n")

  worst()
  cat("\n")
}
