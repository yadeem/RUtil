#' nodoc
#' @export
find.near = function(v1, v2, threshold = 0.2) {
    return(RcppFindNear(v1, v2, threshold))
}

# match two data by the time, the match rule is based on the timestamp of a should
# less than or euqal to the timestamp of b & the by(default is OrderId) column should be euqal
#' @param a data for match
#' @param b data for match
#' @param a.time.col the timestamp column of a data to match
#' @param b.time.col the timestamp column of b data to match
#' @param threshold the timestamp of a data should not less than the timestamp of b beyond the threshold
#' @param by column that two data have the same value
#' @return data.table(a.index, b.index) the row index that matched in a and b data
#' @export
find.match = function(a, b, a.time.col = "Timestamp", b.time.col = "Timestamp", threshold = 0.2, by = "OrderId") {
    ret = RcppFindMatch(a, b, a.time.col, b.time.col, threshold, by)
    setDT(ret)
    return(ret)
}