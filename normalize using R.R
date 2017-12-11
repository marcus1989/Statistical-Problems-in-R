# RMA or MAS5 normalization for R/Bioconductor.
param <- commandArgs(TRUE)
DATA.DIR <- as.character(param[1])

# Set to the name of the file to be created.  This will be saved in
# the current directory (type 'getwd()' at the R prompt to see what it
# is) unless you specify a full path name.
OUTFILE <- as.character(param[2])

# Should be "RMA" or "MAS5".
ALGORITHM <- as.character(param[3])

# Are the files compressed (ends with .gz)?  TRUE or FALSE.
COMPRESSED <- as.numeric(param[4]) == 1



###############################

print(sprintf("Running from: %s", getwd()))

if(!file.exists(DATA.DIR))
  stop("I cannot find DATA.DIR.  Try setting it to a full path name.")

library("affy")

files <- list.files(DATA.DIR)
if(!length(files)) stop("No files found")
x <- unlist(strsplit(files, "\\."))
filestems <- x[seq(1, length(x), by=2)]
fullpaths <- sapply(files, function(x) { paste(DATA.DIR, "/", x, sep="") })

print(sprintf("Normalizing %d files", length(fullpaths)))


if(ALGORITHM == "RMA") {
  eset <- justRMA(filenames=fullpaths, compress=COMPRESSED)
  write.exprs(eset, file=OUTFILE)
} else if(ALGORITHM == "MAS5") {
  NUM.SAMPLES <- 25
  data.all <- c()
  for(i1 in seq(1, length(fullpaths), NUM.SAMPLES)) {
    i2 <- min(i1+NUM.SAMPLES-1, length(fullpaths))
    print(c(i1, i2))
    pd <- new("AnnotatedDataFrame", data=data.frame(x=1:length(fullpaths[i1:i2])))
    varLabels(pd) <- list(x="arbitrary number")
    x <- read.affybatch(filenames=(fullpaths[i1:i2]), phenoData=pd)
    x.mas5 <- mas5(x, normalize=FALSE)
    data.all <- cbind(data.all, exprs(x.mas5))
  }
  colnames(data.all) <- files

  x <- matrix(runif(ncol(data.all)*10), ncol(data.all), 10)
  x <- as.data.frame(x)
  rownames(x) <- colnames(data.all)
  pdata <- new("AnnotatedDataFrame", data=x)
  varLabels(pdata) <- as.list(colnames(x))

  data.all <- new('ExpressionSet', exprs=data.all, phenoData=pdata)
  data.out <- affy.scalevalue.exprSet(data.all)
  write.exprs(data.out, file=OUTFILE)
}
