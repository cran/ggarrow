## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ggarrow)

p <- ggplot(data.frame(x = c(0, 1), y = 0), aes(x, y)) +
  geom_point(colour = c(2, 4), size = 3) +
  geom_text(
    aes(label = c("start\n'fins'", "end\n'head'")),
    colour = c(2, 4), vjust = -1
  )

## ----vary_linewidth-----------------------------------------------------------
p + geom_arrow(linewidth = c(0, 3))

## ----complicated linewidth----------------------------------------------------
ggplot(faithful, aes(waiting)) +
  geom_arrow(
    stat = "density",
    aes(y = after_stat(0),
        linewidth = after_scale(ndensity * 20))
  )

## ----strokes------------------------------------------------------------------
p + geom_arrow(stroke_colour = "black", stroke_width = 1,
               colour = "white", linewidth = 5)

## ----arrow_parts--------------------------------------------------------------
p + geom_arrow(
  arrow_head = arrow_head_wings(),
  arrow_mid  = arrow_head_wings(),
  arrow_fins = arrow_head_wings()
)

## ----arrow_sizes--------------------------------------------------------------
p + geom_arrow(
  arrow_head = arrow_head_wings(),
  arrow_mid  = arrow_head_wings(),
  arrow_fins = arrow_head_wings(),
  length_head = 10,
  length_fins = 4,
  length_mid  = unit(10, "mm")
)

## ----mid_place----------------------------------------------------------------
p + geom_arrow(
  arrow_head = NULL,
  arrow_mid  = arrow_head_wings(),
  mid_place  = c(0.33, 0.66)
)

## ----negative_mid_place-------------------------------------------------------
p + geom_arrow(
  arrow_head = NULL,
  arrow_mid  = arrow_head_wings(),
  mid_place  = c(0.16, -0.33, 0.66, -0.82)
)

## -----------------------------------------------------------------------------
p + geom_arrow(
  arrow_head = NULL,
  arrow_mid  = arrow_head_wings(),
  mid_place  = unit(-1, "cm")
)

## -----------------------------------------------------------------------------
p + geom_arrow(resect = 5)

## -----------------------------------------------------------------------------
p + geom_arrow(resect_head = 10, resect_fins = 3)

## -----------------------------------------------------------------------------
p + geom_arrow_chain(size = 3, resect = 0)

## -----------------------------------------------------------------------------
p + geom_arrow(justify = 1, linewidth = 2)

## -----------------------------------------------------------------------------
p + geom_arrow(resect = 100, force_arrow = TRUE)

