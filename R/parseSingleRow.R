## Parse a single row from a JSON list
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
##############################################################################

.parseSingleRow <- 
  function(row)
{
  ## constants
  kNaValue <- NA
  kMultiValueDelimiterString <- ', '
  ## end constants
  
  ## iterate through rownames and add each element to the data frame
  isNull <- sapply(row,is.null) 
  row[isNull] <- kNaValue 
  row[!isNull] <- lapply(row[!isNull], paste, collapse=kMultiValueDelimiterString)
  data.frame(row, stringsAsFactors=FALSE)
}
