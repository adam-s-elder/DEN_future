# order_to_df simply replaces the a vector of locations to a dataframe
# of relationships.  "Wallingford", "Seattle", "Washington" gets converted to
#        child     parent
# 1 Wallinford    Seattle
# 2 Wallinford Washington
# 3    Seattle Washington
#

order_to_df <- function(vector) {
  vec_l <- length(vector)
  if (vec_l > 1) {
    df_l <- list()
    for (idx in 2:vec_l) {
      df_l[[idx - 1]] <-
        data.frame("child" = vector[1:(idx - 1)],
                   "parent" = vector[idx])
    }
    if (vec_l > 2) {
      return(do.call(what = bind_rows, df_l))
    } else {
      return(df_l[[1]])
    }
  } else {
    return(NULL)
  }
}
