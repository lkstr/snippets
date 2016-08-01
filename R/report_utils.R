
renderDataFrame <- function(pData) {
  isColNumeric <- sapply(pData, is.numeric)
  
  xtbl <- xtable(pData)
  
  alignStr <- paste0(ifelse(isColNumeric, "r", "l"), collapse = "")
  alignStr <- paste0("r", alignStr, collapse = "")
  align(xtbl)  <- alignStr
  
  fnDefaultDigitsTest <- function(x) is.numeric(x) && max(abs(x)) < 1e5
  useDefaultDigits <- sapply(pData, fnDefaultDigitsTest)
  colDigits <- ifelse(useDefaultDigits, 2, 0)
  colDigits <- c(0, colDigits)
  digits(xtbl) <- colDigits
  
  print(xtbl, include.rownames = F, format.args = list(big.mark = ","))
}