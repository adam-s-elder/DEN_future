# Invert the order of an axis for a date object.
library(scales)

c_trans <- function(a, b, breaks = b$breaks, format = b$format, domain = b$domain) {
  a <- as.trans(a)
  b <- as.trans(b)
  name <- paste(a$name, b$name, sep = "-")
  trans <- function(x) a$trans(b$trans(x))
  inv <- function(x) b$inverse(a$inverse(x))
  trans_new(name, trans, inv, breaks = breaks, format = format, domain = domain)
}

rev_date <- c_trans("reverse", "time")

theme_altair <- function() {
  theme_minimal(base_family = "Arial", base_size = 12) +
    theme(
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      legend.position = "right",
      plot.title = element_text(size = 16, hjust = 0, face = "plain"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.border = element_blank()
    )
}
# This also needs to be added for the altair theme:
# scale_y_continuous(expand = c(0, 0))
#
# periods_to_label <- c("A" = "Symptoms to Test",
#                       "B" = "Test to Result",
#                       "C" = "Result to Case Notified",
#                       "D" = "Contact Named to Reached")

periods_to_label <- c("Symptoms to Test" = "A",
                      "Test to Result" = "B",
                      "Result to Case Notified" = "C",
                      "Contact Named to Reached" = "D")


