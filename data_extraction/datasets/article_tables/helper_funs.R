perc_to_prop <- function(x) {
  look <- x |> map(.f = function(y) {
    z <- y |> str_replace("\\%", "* 0.01")
    if (!grepl("<", z)) {
      bb <- eval(parse(text = z))
    } else {
      bb <- z
    }
    return(bb)
  })
  look <- look |> do.call(what = c)
}
