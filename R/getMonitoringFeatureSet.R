
getMonitoringFeatureSet = function(log) {
  #assertDataFrame(log, any.missing = FALSE, all.missing = FALSE,
  #  min.rows = 2L, ncols = 3L)

  res = .Call("getMonitoringFeatureSetC", as.numeric(log$iter), as.numeric(log$incumbant), PACKAGE = "salesperson")
  return(res)
}
