## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=FALSE--------------------------------------------------------
suppressPackageStartupMessages({
  library(ggarrow)
  library(grid)
})

## ----illustrate_head, echo = FALSE--------------------------------------------
arrow_grob <- grob_arrow(
  x = unit(c(0.2, 0.8), "npc"),
  y = unit(c(0.5, 0.5), "npc"),
  arrow_head = arrow_head_wings(),
  arrow_fins = arrow_fins_feather(),
  length_head = unit(10, "mm"),
  length_fins = unit(14.4, "mm"),
  shaft_width = unit(2, "mm")
)

grid.newpage()
grid.draw(arrow_grob)
grid.lines(
  x = unit(0.8, "npc") - unit(c(0, 0, 1.44, 1.44), "cm"),
  y = unit(0.5, "npc") + unit(1, "cm") + unit(c(-0.2, 0, 0, -0.2), "cm")
)
grid.text(
  c("start", "end", "arrow_head"),
  x = unit(c(0.1, 0.9, 0.8), "npc") - unit(c(0, 0, 0.72), "cm"),
  y = unit(c(0.5, 0.5, 0.5), "npc") + unit(c(0, 0, 1.5), "cm")
)

## ----illustrate_head_wings, echo = FALSE--------------------------------------

arr <- arrow_head_wings() * 40
arr[, "x"] <- arr[, "x"] - 11.2
arrow_grob <- grob_arrow(
  x = unit(0.5, "npc") + unit(c(-28.8, 28.8), "mm"),
  y = unit(c(0.5, 0.5), "npc"),
  length_head = unit(40, "mm"),
  shaft_width = unit(5, "mm"),
  gp = gpar(fill = NA)
)

grid.newpage()
grid.points(
  x = unit(0.5, "npc") + unit(c(-11.2, 28.8), "mm"),
  y = unit(c(0.5, 0.5), "npc"), pch = 16, gp = gpar(col = alpha(2, 0.5))
)
grid.lines(
  x = unit(0.5, "npc") + unit(c(-11.2, -11.2, 28.8, 28.8), "mm"),
  y = unit(0.5, "npc") + unit(c(0, -2, -2, 0), "cm"),
  gp = gpar(col = alpha(2, 0.5))
)
grid.text(
  x = unit(0.5, "npc") + unit(8.8, "mm"),
  y = unit(0.5, "npc") - unit(2, "cm"),
  label = "length_head", gp = gpar(col = 2),
  vjust = 2
)
grid.lines(
  x = unit(0.5, "npc") + unit(c(0, 28.8), "mm"),
  y = unit(c(0.5, 0.5), "npc"),
  gp = gpar(col = 3)
)
arc <- seq(pi, pi - 20 * pi / 180, length.out = 30)
grid.polyline(
  x = unit(0.5, "npc") + unit(28.8, "mm") + unit(cos(arc) * 16, "mm"),
  y = unit(0.5, "npc") + unit(sin(arc) * 16, "mm"),
  gp = gpar(col = 3)
)
grid.text(
  x = unit(0.5, "npc") + unit(9.4, "mm"),
  y = unit(0.5, "npc"), label = "offset", gp = gpar(col = 3),
  vjust = -0.5, hjust = 1
)
arc <- seq(2 * pi - 20 * pi / 180, 2 * pi - 50 * pi / 180, length.out = 30)
grid.polyline(
  x = unit(0.5, "npc") + unit(cos(arc) * 10, "mm") + unit(arr[2, "x"], "mm"),
  y = unit(0.5, "npc") + unit(sin(arc) * 10, "mm") + unit(arr[2, "y"], "mm"),
  gp = gpar(col = 4)
)
grid.text(
  x = unit(0.5, "npc") + unit(arr[2, "x"], "mm"),
  y = unit(0.5, "npc") + unit(arr[2, "y"], "mm"),
  label = "inset", gp = gpar(col = 4), hjust = -0.5, vjust = -0.5
)
grid.draw(arrow_grob)

## ----echo=FALSE---------------------------------------------------------------
offset <- c(20, 30, 90, 45, 135)
inset  <- c(30, 60, 60, 90, 30)
y      <- 4:0
descr  <- c(" (default)", "", "", "", "")

example_plot <- ggplot(mapping = aes(x = c(0, 1))) +
  coord_cartesian(clip = "off") +
  scale_y_continuous(limits = c(-0.5, 4.5)) +
  scale_x_continuous(limits = c(-0.1, 1.1)) +
  theme_void()

example_plot +
  geom_vline(xintercept = 1, colour = "tomato") +
  annotation_custom(
    grid::segmentsGrob(
      x0 = unit(0, "npc") - unit(10, "mm"),
      x1 = unit(0, "npc") - unit(10, "mm"),
      gp = grid::gpar(col = "dodgerblue")
    ),
    xmin = 1, xmax = 1
  ) +
  lapply(seq_along(y), function(i) {
    geom_arrow(
      aes(y = c(y[i], y[i])), linewidth = 2,
      arrow_head = arrow_head_wings(offset = offset[i], inset = inset[i]),
      length_head = unit(10, "mm")
    )
  }) +
  annotate(
    "text", x = 0.5, y = y + 0.5, label = paste0(
      "offset = ", offset, ", inset = ", inset, descr
    )
  )

## ----illustrate_head_line, echo = FALSE---------------------------------------
arr <- arrow_head_line()
arr <- arr(40, 5)
arr[, "x"] <- arr[, "x"] + 18.8
arrow_grob <- grob_arrow(
  x = unit(0.5, "npc") + unit(c(-28.8, 28.8), "mm"),
  y = unit(c(0.5, 0.5), "npc"),
  length_head = unit(40, "mm"),
  shaft_width = unit(5, "mm"),
  arrow_head = arrow_head_line(),
  gp = gpar(fill = NA)
)
arrow1 <- arrow2 <- arrow3 <- arrow_grob
arrow1$arrow_head <- arrow_head_line(lineend = "square")
arrow2$arrow_head <- arrow_head_line(lineend = "parallel")
arrow3$arrow_head <- arrow_head_line(lineend = "round")
arrow2$gp <- arrow3$gp <- arrow1$gp <- gpar(lwd = 0.5, col = 3, fill = NA)

grid.newpage()

grid.points(
  x = unit(0.5, "npc") + unit(arr[1:2, "x"], "mm"),
  y = unit(0.5, "npc") + unit(arr[1:2, "y"], "mm"),
  gp = gpar(col = 2, alpha = 0.5), pch = 16
)

angle <- 30 * pi / 180 - 0.5 * pi
grid.lines(
  x = unit(0.5, "npc") + unit(arr[c(1, 1, 2, 2), "x"], "mm") +
    unit(cos(angle) * c(0, 1, 1, 0) * 0.5, "cm"),
  y = unit(0.5, "npc") + unit(arr[c(1, 1, 2, 2), "y"], "mm") +
    unit(sin(angle) * c(0, 1, 1, 0) * 0.5, "cm"),
  gp = gpar(col = 2, alpha = 0.5)
)

grid.draw(arrow1)
grid.draw(arrow2)
grid.draw(arrow3)

grid.text(
  x = unit(0.5, "npc") + unit(arr[6, "x"], "mm"),
  y = unit(0.5, "npc") + unit(arr[6, "y"], "mm"),
  label = "lineend", gp = gpar(col = 3), vjust = -0.5
)
grid.text(
  x = unit(0.5, "npc") + unit(mean(arr[1:2, "x"]), "mm") +
    unit(cos(angle) * 0.5, "cm"),
  y = unit(0.5, "npc") + unit(mean(arr[1:2, "y"]), "mm") +
    unit(sin(angle) * 0.5, "cm"),
  label = "length_head", rot = 30, gp = gpar(col = 2), vjust = 1.5
)
angle <- pi - 30 * pi / 180
grid.lines(
  x = unit(0.5, "npc") + unit(cos(c(angle, angle, pi)) * c(30, 0, 30), "mm") +
    unit(28.8 - 2.5 / sin(angle), "mm"),
  y = unit(0.5, "npc") + unit(sin(c(angle, angle, pi)) * c(30, 0, 30), "mm"),
  gp = gpar(col = 4)
)
angle <- seq(angle, pi, length.out = 30)
grid.lines(
  x = unit(0.5, "npc") + unit(cos(angle) * 20, "mm") +
    unit(28.8 - 2.5 / sin(angle[1]), "mm"),
  y = unit(0.5, "npc") + unit(sin(angle) * 20, "mm"),
  gp = gpar(col = 4)
)
grid.text(
  x = unit(0.5, "npc") - unit(10, "mm"),
  y = unit(0.5, "npc"), label = "angle", gp = gpar(col = 4),
  vjust = -1.5, hjust = 0
)

grid.draw(arrow_grob)

## ----echo=FALSE---------------------------------------------------------------
angle   <- c(30, 30, 90, 30, 135)
lineend <- c("butt", "round", "square", "parallel", "butt")
y      <- 4:0
descr  <- c(" (default)", "", "", "", "")

example_plot +
  geom_vline(xintercept = 1, colour = "tomato") +
  lapply(seq_along(y), function(i) {
    geom_arrow(
      aes(y = c(y[i], y[i])), linewidth = 2,
      arrow_head = arrow_head_line(angle = angle[i], lineend = lineend[i]),
      length_head = unit(10, "mm")
    )
  }) +
  annotate(
    "text", x = 0.5, y = y + 0.5, label = paste0(
      "angle = ", angle, ", lineend = ", lineend, descr
    )
  )

## ----illustrate_head_minimal, echo = FALSE------------------------------------
arrow_grob <- grob_arrow(
  x = unit(0.5, "npc") + unit(c(-28.8, 28.8), "mm"),
  y = unit(c(0.5, 0.5), "npc"),
  arrow_head = arrow_head_minimal(),
  length_head = unit(40, "mm"),
  shaft_width = unit(10, "mm"),
  gp = gpar(fill = NA)
)

grid.newpage()

angle <- pi - 45 / 180 * pi
grid.lines(
  x = unit(0.5, "npc") + unit(28.8, "mm") +
    unit(cos(c(angle, angle, pi)) * c(30, 0, 30), "mm"),
  y = unit(0.5, "npc") + unit(sin(c(angle, angle, pi)) * c(30, 0, 30), "mm"),
  gp = gpar(col = alpha(2, 0.5))
)
angle <- seq(angle, pi, length.out = 30)
grid.lines(
  x = unit(0.5, "npc") + unit(cos(angle) * 20, "mm") + unit(28.8, "mm"),
  y = unit(0.5, "npc") + unit(sin(angle) * 20, "mm"),
  gp = gpar(col = alpha(2, 0.5))
)
grid.text(
  x = unit(0.5, "npc") - unit(10, "mm"),
  y = unit(0.5, "npc"), label = "angle", gp = gpar(col = 2),
  vjust = -3, hjust = -0.5
)

grid.draw(arrow_grob)

## ----echo=FALSE---------------------------------------------------------------
angle   <- c(45, 45, 90, 135, 5)
width   <- c(5, 2, 5, 5, 5)
y      <- 4:0
descr  <- c(" (default)", "", "", "", "")

example_plot +
  geom_vline(xintercept = 1, colour = "tomato") +
  lapply(seq_along(y), function(i) {
    geom_arrow(
      aes(y = c(y[i], y[i])), linewidth = width[i],
      arrow_head = arrow_head_minimal(angle = angle[i]),
      length_head = unit(0, "mm")
    )
  }) +
  annotate(
    "text", x =  0.5, y = y + 0.5, label = paste0(
      "angle = ", angle, ", linewidth = ", width, descr
    )
  )

## ----illustrate_fins, echo = FALSE--------------------------------------------
arrow_grob <- grob_arrow(
  x = unit(c(0.2, 0.8), "npc"),
  y = unit(c(0.5, 0.5), "npc"),
  arrow_head = arrow_head_wings(),
  arrow_fins = arrow_fins_feather(),
  length_head = unit(10, "mm"),
  length_fins = unit(14.4, "mm"),
  shaft_width = unit(2, "mm")
)

grid.newpage()
grid.draw(arrow_grob)
grid.lines(
  x = unit(0.2, "npc") + unit(c(0, 0, 1.44, 1.44), "cm"),
  y = unit(0.5, "npc") + unit(1, "cm") + unit(c(-0.2, 0, 0, -0.2), "cm")
)
grid.text(
  c("start", "end", "arrow_fins"),
  x = unit(c(0.1, 0.9, 0.2), "npc") + unit(c(0, 0, 0.72), "cm"),
  y = unit(c(0.5, 0.5, 0.5), "npc") + unit(c(0, 0, 1.5), "cm")
)

## ----illustrate_fins_feather, echo = FALSE------------------------------------
arr <- arrow_fins_feather()
arr <- arr * 50
arr[, "x"] <- -1 * arr[, "x"] + 21.2
arrow_grob <- grob_arrow(
  x = unit(0.5, "npc") + unit(c(-28.8, 28.8), "mm"),
  y = unit(c(0.5, 0.5), "npc"),
  arrow_head = NULL, arrow_fins = arrow_fins_feather(),
  length_fins = unit(50, "mm"),
  shaft_width = unit(5, "mm"),
  gp = gpar(fill = NA)
)
mc <- makeContent(arrow_grob)

grid.newpage()

grid.points(
  x = unit(0.5, "npc") + unit(arr[4:5, "x"], "mm"),
  y = unit(0.5, "npc") + unit(arr[4:5, "y"], "mm"),
  gp = gpar(col = 3, alpha = 0.5), pch = 16
)
grid.lines(
  x = unit(0.5, "npc") + unit(arr[c(4, 4, 5, 5), "x"], "mm"),
  y = unit(0.5, "npc") + unit(c(arr[4, "y"], c(-20, -20), arr[5, "y"]), "mm"),
  gp = gpar(col = 3, alpha = 0.5)
)
grid.text(
  x = unit(0.5, "npc") + unit(mean(arr[4:5, "x"]), "mm"),
  y = unit(0.5, "npc") + unit(-20, "mm"),
  label = "outdent", gp = gpar(col = 3), vjust = 1.5
)


grid.points(
  x = unit(0.5, "npc") + unit(arr[c(1, 6), "x"], "mm"),
  y = unit(0.5, "npc") + unit(arr[c(1, 6), "y"], "mm"),
  gp = gpar(col = 4, alpha = 0.5), pch = 16
)
grid.lines(
  x = unit(0.5, "npc") + unit(arr[c(1, 1, 6, 6), "x"], "mm"),
  y = unit(0.5, "npc") + unit(c(arr[1, "y"], c(-20, -20), arr[6, "y"]), "mm"),
  gp = gpar(col = 4, alpha = 0.5)
)
grid.text(
  x = unit(0.5, "npc") + unit(mean(arr[c(1, 6), "x"]), "mm"),
  y = unit(0.5, "npc") + unit(-20, "mm"),
  label = "indent", gp = gpar(col = 4), vjust = 1.5
)

grid.points(
  x = unit(0.5, "npc") + unit(arr[c(2, 4), "x"], "mm"),
  y = unit(0.5, "npc") + unit(arr[c(2, 4), "y"], "mm"),
  gp = gpar(col = 2, alpha = 0.5), pch = 16
)
grid.lines(
  x = unit(0.5, "npc") + unit(arr[c(2, 2, 4, 4), "x"], "mm"),
  y = unit(0.5, "npc") + unit(c(arr[2, "y"], c(20, 20), arr[4, "y"]), "mm"),
  gp = gpar(col = 2, alpha = 0.5)
)
grid.text(
  x = unit(0.5, "npc") + unit(mean(arr[c(2, 4), "x"]), "mm"),
  y = unit(0.5, "npc") + unit(20, "mm"),
  label = "length_fins", gp = gpar(col = 2), vjust = -0.5
)

grid.points(
  x = unit(0.5, "npc") + unit(arr[1:2, "x"], "mm"),
  y = unit(0.5, "npc") + unit(arr[1:2, "y"], "mm"),
  gp = gpar(col = 6, alpha = 0.5), pch = 16
)
grid.lines(
  x = unit(0.5, "npc") + unit(c(arr[1, "x"], -35, -35, arr[2, "x"]), "mm"),
  y = unit(0.5, "npc") + unit(arr[c(1, 1, 2, 2), "y"], "mm"),
  gp = gpar(col = 6, alpha = 0.5)
)
grid.text(
  x = unit(0.5, "npc") - unit(35, "mm"),
  y = unit(0.5, "npc") + unit(mean(arr[1:2, "y"]), "mm"), rot = 90,
  label = "height", gp = gpar(col = 6), vjust = -0.5
)


grid.draw(arrow_grob)

## ----example_fins_feather, echo=FALSE-----------------------------------------
indent  <- c(0.3, 0, 0.3, 0.3, -0.3)
outdent <- c(0.3, 0.3, 0, 0.3, -0.3)
height  <- c(0.5, 0.5, 0.5, 1, 0.5)
y      <- 4:0
descr  <- c(" (default)", "", "", "", "")

example_plot +
  geom_vline(xintercept = 0, colour = "tomato") +
  annotation_custom(
    grid::segmentsGrob(
      x0 = unit(0, "npc") + unit(10, "mm"),
      x1 = unit(0, "npc") + unit(10, "mm"),
      gp = grid::gpar(col = "dodgerblue")
    ),
    xmin = 0, xmax = 0
  ) +
  lapply(seq_along(y), function(i) {
    geom_arrow(
      aes(y = c(y[i], y[i])), linewidth = 2, arrow_head = NULL,
      arrow_fins = arrow_fins_feather(
        indent = indent[i], outdent = outdent[i], height = height[i]
      ),
      length_fins = unit(10, "mm")
    )
  }) +
  annotate(
    "text", x = 0.5, y = y + 0.5, label = paste0(
      "indent = ", indent, ", outdent = ", outdent, ", height = ", height, descr
    )
  )

## ----illustrate_fins_line, echo = FALSE---------------------------------------
arr <- arrow_fins_line()
arr <- arr(40, 5)
# arr <- arr * 50
arrow_grob <- grob_arrow(
  x = unit(c(0.5, 0.7), "npc"),
  y = unit(c(0.5, 0.5), "npc"),
  arrow_head = NULL, arrow_fins = arrow_fins_line(),
  length_fins = unit(40, "mm"),
  shaft_width = unit(5, "mm"),
  justify = 1,
  gp = gpar(fill = NA)
)
arrow1 <- arrow2 <- arrow3 <- arrow_grob
arrow1$arrow_fins <- arrow_fins_line(lineend = "square")
arrow2$arrow_fins <- arrow_fins_line(lineend = "parallel")
arrow3$arrow_fins <- arrow_fins_line(lineend = "round")
arrow2$x[1] <- arrow2$x[1] - unit(10, "mm")
arrow2$gp <- arrow3$gp <- arrow1$gp <- gpar(lwd = 0.5, col = 3, fill = NA)

grid.newpage()

grid.points(
  x = unit(0.5, "npc") - unit(arr[3:4, "x"], "mm"),
  y = unit(0.5, "npc") - unit(arr[3:4, "y"], "mm"),
  gp = gpar(col = 2, alpha = 0.5), pch = 16
)

angle <- 30 * pi / 180 

grid.lines(
  x = unit(0.5, "npc")- unit(cos(c(angle, angle, pi)) * c(30, 0, -30), "mm") -
    unit(5, "mm"),
  # x = unit(0.5, "npc") + unit(cos(c(angle, angle, pi)) * c(30, 0, 30), "mm") +
    # unit(28.8 - 2.5 / sin(angle), "mm"),
  y = unit(0.5, "npc") + unit(sin(c(angle, angle, pi)) * c(30, 0, 30), "mm"),
  gp = gpar(col = 4)
)

grid.draw(arrow1)
grid.draw(arrow2)
grid.draw(arrow3)

grid.text(
  x = unit(0.5, "npc") - unit(arr[6, "x"], "mm"),
  y = unit(0.5, "npc") - unit(arr[6, "y"], "mm"),
  label = "lineend", gp = gpar(col = 3), vjust = -1
)

angle <- seq(pi - angle, pi, length.out = 30)
grid.lines(
  x = unit(0.5, "npc") + unit((cos(angle) * 20) - 5, "mm"),
  y = unit(0.5, "npc") + unit(sin(angle) * 20, "mm"),
  gp = gpar(col = 4)
)
grid.text(
  x = unit(0.5, "npc") - unit(33, "mm"),
  y = unit(0.5, "npc"), label = "angle", gp = gpar(col = 4),
  vjust = -1.5
)
.halfpi <- 0.5 * pi
angle <- 30 * pi / 180 
grid.lines(
  x = unit(0.5, "npc") - unit(arr[c(3,3,4,4), "x"], "mm") +
    unit(c(0, cos(.halfpi - angle[c(1, 1)]), 0), "cm"),
  y = unit(0.5, "npc") - unit(arr[c(3,3,4,4), "y"], "mm") -
    unit(c(0, sin(.halfpi - angle[c(1, 1)]), 0), "cm"),
  gp = gpar(col = 2, alpha = 0.5)
)

grid.text(
  x = unit(0.5, "npc") - unit(mean(arr[3:4, "x"]), "mm") + 
    unit(cos(.halfpi - angle) * 1, "cm"),
  y = unit(0.5, "npc") - unit(mean(arr[3:4, "y"]), "mm") -
    unit(sin(.halfpi - angle) * 1, "cm"),
  label = "length_fins", rot = 30,
  gp = gpar(col = 2), vjust = 1.5
)

grid.draw(arrow_grob)

## ----echo=FALSE---------------------------------------------------------------
angle   <- c(30, 30, 30, 30, 135)
lineend <- c("butt", "round", "square", "parallel", "butt")
y      <- 4:0
descr  <- c(" (default)", "", "", "", "")

example_plot +
  geom_vline(xintercept = 0, colour = "tomato") +
  lapply(seq_along(y), function(i) {
    geom_arrow(
      aes(y = c(y[i], y[i])), linewidth = 2, arrow_head = NULL,
      arrow_fins = arrow_fins_line(angle = angle[i], lineend = lineend[i]),
      length_fins = unit(10, "mm")
    )
  }) +
  annotate(
    "text", x = 0.5, y = y + 0.5, label = paste0(
      "angle = ", angle, ", lineend = ", lineend, descr
    )
  )

## ----illustrate_fins_minimal, echo = FALSE------------------------------------
angle <- 45
arrow_grob <- grob_arrow(
  x = unit(0.5, "npc") + unit(c(-0.1, 0.1), "npc"),
  y = unit(c(0.5, 0.5), "npc"),
  arrow_fins = arrow_fins_minimal(angle),
  arrow_head = NULL,
  length_fins = unit(40, "mm"),
  shaft_width = unit(10, "mm"),
  gp = gpar(fill = NA)
)

grid.newpage()
grid.draw(arrow_grob)

angle <- .halfpi + angle / 180 * pi
grid.lines(
  x = unit(0.4, "npc") + unit(c(0, 0, cos(angle)) * 30, "mm"),
  y = unit(0.5, "npc") + unit(c(1, 0, sin(angle)) * 30, "mm"),
  # x = unit(0.5, "npc") + unit(cos(c(angle, angle, pi)) * c(30, 0, 30), "mm"),
  # y = unit(0.5, "npc") + unit(sin(c(angle, angle, pi)) * c(30, 0, 30), "mm"),
  gp = gpar(col = alpha(2, 0.5))
)
angle <- seq(angle, .halfpi, length.out = 30)
grid.lines(
  x = unit(0.4, "npc") + unit(cos(angle) * 20, "mm"),
  y = unit(0.5, "npc") + unit(sin(angle) * 20, "mm"),
  gp = gpar(col = alpha(2, 0.5))
)
grid.text(
  x = unit(0.4, "npc") - unit(10, "mm"),
  y = unit(0.5, "npc") + unit(25, "mm"),
  label = "angle", gp = gpar(col = 2)
)


## ----echo=FALSE---------------------------------------------------------------
ggplot(mapping = aes(x = c(0, 1))) +
  geom_vline(xintercept = 0, colour = "tomato") +
  geom_arrow(
    aes(y = c(4, 4)), linewidth = 5, arrow_head = NULL,
    arrow_fins = arrow_fins_minimal(angle = 45)
  ) +
  annotate(
    "text", x = 0.5, y = 4.5, label = "angle = 45 (default)"
  ) +
  geom_arrow(
    aes(y = c(3, 3)), linewidth = 2, arrow_head = NULL,
    arrow_fins = arrow_fins_minimal(angle = 45)
  ) +
  annotate(
    "text", x = 0.5, y = 3.5, label = "angle = 45, linewidth = 2"
  ) +
  geom_arrow(
    aes(y = c(2, 2)), linewidth = 5, arrow_head = NULL,
    arrow_fins = arrow_fins_minimal(angle = 0)
  ) +
  annotate(
    "text", x = 0.5, y = 2.5, label = "angle = 0"
  ) +
  geom_arrow(
    aes(y = c(1, 1)), linewidth = 5, arrow_head = NULL,
    arrow_fins = arrow_fins_minimal(angle = 135)
  ) +
  annotate(
    "text", x = 0.5, y = 1.5, label = "angle = 135"
  ) +
  geom_arrow(
    aes(y = c(0, 0)), linewidth = 5, arrow_head = NULL,
    arrow_fins = arrow_fins_minimal(angle = 70)
  ) +
  annotate(
    "text", x = 0.5, y = 0.5, label = "angle = 70"
  ) +
  coord_cartesian(clip = "off") +
  scale_y_continuous(limits = c(-0.5, 4.5)) +
  scale_x_continuous(limits = c(-0.1, 1.1)) +
  theme_void()

## ----illustrate_cups, echo = FALSE--------------------------------------------
arr <- arrow_cup(angle = 90)
arr <- arr(1, 4, 10)
arrow_grob <- grob_arrow(
  x = unit(c(0.1, 0.4), "npc"),
  y = unit(c(0.5, 0.5), "npc"),
  arrow_head = arrow_cup(angle = 90),
  length_head = unit(30, "mm"),
  resect_head = unit(20, "mm"),
  shaft_width = unit(10, "mm"),
  gp = gpar(fill = NA)
)
arrow1 <- arrow_grob
arrow1$x <- unit(c(0.9, 0.6), "npc")
arrow1$arrow_head <- arrow_cup(lineend = "butt")
arrow2 <- arrow1
arrow2$arrow_head <- arrow_cup(lineend = "round")
arrow2$gp <- gpar(col = 3, fill = NA)

grid.newpage()

grid.points(
  x = unit(c(0.4, 0.4, 0.6, 0.6), "npc") - unit(c(0, 20, 0, -20), "mm"),
  y = unit(c(0.5, 0.5, 0.5, 0.5), "npc"),
  gp = gpar(col = 2, alpha = 0.5), pch = 16
)
grid.polyline(
  x = unit(c(0.4, 0.4, 0.6, 0.6), "npc") - unit(c(0, 20, 0, -20), "mm"),
  y = unit(c(0.5, 0.5, 0.5, 0.5), "npc"), id.lengths = c(2,2),
  gp = gpar(col = 2, alpha = 0.5)
)
grid.text(
  x = unit(c(0.4, 0.6), "npc") - unit(c(10, -10), "mm"),
  y = unit(c(0.5, 0.5), "npc"),
  label = "resect",
  vjust = -0.5,
  gp = gpar(col = 2)
)

angle <- 90 / 180 * pi
grid.lines(
  x = unit(0.4, "npc") + unit(c(cos(0.5 * angle + pi) * 40, 0, 
                                cos(-0.5 * angle + pi) * 40), "mm"),
  y = unit(0.5, "npc") + unit(c(sin(0.5 * angle + pi) * 40, 0, 
                                sin(-0.5 * angle + pi) * 40), "mm"),
  gp = gpar(col = 4, alpha = 0.5)
)
angle <- seq(-0.5, 0.5, length.out = 20) * angle + pi
grid.lines(
  x = unit(0.4, "npc") + unit(cos(angle) * 35, "mm"),
  y = unit(0.5, "npc") + unit(sin(angle) * 35, "mm"),
  gp = gpar(col = 4, alpha = 0.5)
)
grid.text(
  x = unit(0.4, "npc") + unit(cos(90 / 180 * pi * 0.25 + pi) * 40 - 5, "mm"),
  y = unit(0.5, "npc") + unit(sin(90 / 180 * pi * 0.25 + pi) * 40, "mm"),
  label = "angle", gp = gpar(col = 4)
)

angle <- 30 / 20
angle <- seq(-0.5, 0.5, length.out = 20) * angle + pi
grid.lines(
  x = unit(0.6, "npc") - unit(cos(angle) * 25, "mm"),
  y = unit(0.5, "npc") + unit(sin(angle) * 25, "mm"),
  gp = gpar(col = 4, alpha = 0.5)
)
grid.text(
  x = unit(0.6, "npc") + unit(25, "mm"),
  y = unit(0.5, "npc") + unit(10, "mm"),
  label = "length_head", gp = gpar(col = 4),
  hjust = 0
)

grid.text(
  x = unit(0.6, "npc") + unit(5, "mm"),
  y = unit(0.3, "npc"),
  label = "lineend",
  gp = gpar(col = 3)
)

grid.draw(arrow2)
grid.draw(arrow1)
grid.draw(arrow_grob)

## ----echo = FALSE-------------------------------------------------------------
ggplot(mapping = aes(x = c(0, 1))) +
  geom_vline(xintercept = 1, colour = "tomato") +
  annotate(
    "point", x = 1, y = 0:4, colour = "tomato", size = 5, shape = 16
  ) +
  geom_arrow(
    aes(y = c(4, 4)), linewidth = 3,
    arrow_head = arrow_cup(), resect_head = 5,
    length_head = 5
  ) +
  annotate(
    "text", x = 0.5, y = 4.5, label = "length_head = 5, resect_head = 5"
  ) +
  geom_arrow(
    aes(y = c(3, 3)), linewidth = 3,
    arrow_head = arrow_cup(), resect_head = 10,
    length_head = 5
  ) +
  annotate(
    "text", x = 0.5, y = 3.5, label = "length_head = 5, resect_head = 10"
  ) +
  geom_arrow(
    aes(y = c(2, 2)), linewidth = 3,
    arrow_head = arrow_cup(), resect_head = 2.5,
    length_head = 5
  ) +
  annotate(
    "text", x = 0.5, y = 2.5, label = "length_head = 5, resect_head = 2.5"
  ) +
  geom_arrow(
    aes(y = c(1, 1)), linewidth = 3,
    arrow_head = arrow_cup(angle = 90, lineend = "butt"), resect_head = 5,
    length_head = 5
  ) +
  annotate(
    "text", x = 0.5, y = 1.5, 
    label = "angle = 90, resect_head = 2.5, lineend = 'butt'"
  ) +
  geom_arrow(
    aes(y = c(0, 0)), linewidth = 3,
    arrow_head = arrow_cup(angle = 360), resect_head = 2.5,
    length_head = 5
  ) +
  annotate(
    "text", x = 0.5, y = 0.5, 
    label = "angle = 360, resect_head = 2.5"
  ) +
  coord_cartesian(clip = "off") +
  scale_y_continuous(limits = c(-0.5, 4.5)) +
  scale_x_continuous(limits = c(-0.1, 1.1)) +
  theme_void()

## -----------------------------------------------------------------------------
ggplot(mtcars, aes(disp, mpg)) +
  geom_point(aes(colour = factor(cyl))) +
  geom_arrow_curve(
    data = ~ subset(.x, rownames(.x) == "Hornet 4 Drive"),
    aes(xend = 300, yend = 25),
    arrow_fins = arrow_cup(angle = 360), resect_fins = 1.5
  ) +
  annotate(
    "text", x = 300, y = 25,
    label = "Hornet 4 Drive",
    vjust = -1
  )

