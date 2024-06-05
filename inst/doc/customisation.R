## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo = FALSE-------------------------------------------------------------

plot_ornament <- function(ornament) {
  # Plot ornament
  par(pty = "s")
  plot(ornament, type = "n", xlim = c(-1, 1), ylim = c(-1, 1))
  polygon(ornament[, "x"], ornament[, "y"])
  
  # Incoming line
  lines(ggarrow:::debug_notching(orn, 0.2), col = 2)
  # Length measurement
  lines(x = c(0, 1), y = c(0, 0), col = 4)
  text(x = 0.5, y = 0.1, labels = "Length", col = 4)
  text(x = -0.5, y = 0, labels = "Incoming line", col = 2)
}

## -----------------------------------------------------------------------------
my_ornament <- function(n = 5) {
  t <- seq(0, 2 * pi, length.out = n * 2 + 1)[-(n * 2 + 1)]
  l <- rep(c(1, 0.4), length.out = length(t))

  cbind(
    x = cos(t) * l,
    y = sin(t) * l
  )
}

## -----------------------------------------------------------------------------
orn <- my_ornament(5)

plot_ornament(orn)

## -----------------------------------------------------------------------------
library(ggarrow)

ggplot(data = data.frame(x = c(0, 1)), aes(x = x)) +
  geom_arrow(aes(y = c(1, 3)), arrow_head = orn, resect = unit(2, "cm")) +
  geom_arrow(aes(y = c(2, 2)), arrow_fins = orn, length_fins = unit(1, "cm")) +
  geom_arrow(aes(y = c(3, 1)), arrow_mid  = orn, mid_place = c(0.33, 0.66), 
             linewidth = 2)

## -----------------------------------------------------------------------------
half_star <- orn[orn[, "y"] >= 0, ]

ggplot(data.frame(x = c(0, 1), y = c(1, 1)), aes(x, y)) +
  geom_arrow(arrow_head = half_star, linewidth = 3)

## -----------------------------------------------------------------------------
magic_number <- 0.7528125
half_star[, "y"] <- half_star[, "y"] - (1.5/12) * magic_number

ggplot(data.frame(x = c(0, 1), y = c(1, 1)), aes(x, y)) +
  geom_arrow(arrow_head = half_star, linewidth = 3)

## -----------------------------------------------------------------------------
half_star <- function(n = 5) {
  ornament <- my_ornament(n)
  function(...) {
    half <- ornament[ornament[, "y"] >= 0, ]
    half
  }
}

ggplot(data.frame(x = c(0, 1), y = c(1, 1)), aes(x, y)) +
  geom_arrow(arrow_head = half_star(5), linewidth = 3)

## -----------------------------------------------------------------------------
half_star <- function(n = 5) {
  ornament <- my_ornament(n)
  function(length, ...) {
    half <- ornament[ornament[, "y"] >= 0, ]
    half * length
  }
}

ggplot(data.frame(x = c(0, 1), y = c(1, 1)), aes(x, y)) +
  geom_arrow(arrow_head = half_star(5), linewidth = 3)

## -----------------------------------------------------------------------------
half_star <- function(n = 5) {
  ornament <- my_ornament(n)
  function(length, ...) {
    half <- ornament[ornament[, "y"] >= 0, ]
    half <- half * length
    attr(half, "resect") <- length
    half
  }
}

ggplot(data.frame(x = c(0, 1), y = c(1, 1)), aes(x, y)) +
  geom_arrow(arrow_head = half_star(5), linewidth = 3)

## -----------------------------------------------------------------------------
half_star <- function(n = 5) {
  ornament <- my_ornament(n)
  function(length, width, ...) {
    half <- ornament[ornament[, "y"] >= 0, ]
    half <- half * length
    half[, "y"] <- half[, "y"] - 0.5 * width
    attr(half, "resect") <- length
    half
  }
}

df <- expand.grid(x = c(0, 1), width = 1:4)

ggplot(df, aes(x, width, linewidth = I(width), group = width)) +
  geom_arrow(arrow_head = half_star(5)) +
  ylim(0, 5)

## -----------------------------------------------------------------------------
p <- ggplot(whirlpool(5), aes(x, y, group = group)) +
  coord_equal()

p + geom_arrow(aes(arrow_head = group), resect = 5) +
  scale_arrow_head_discrete(
    values = list("head_wings", orn, "fins_feather", orn, "cup"),
  )

## -----------------------------------------------------------------------------
arrow_star <- function(n = 5) {
  my_ornament(round(n))
}

p + geom_arrow(aes(arrow_head = group), resect = 1) +
  scale_arrow_head_discrete(
    values = c("head_wings", "star", "fins_feather", "star", "cup"),
  )

## -----------------------------------------------------------------------------
p + geom_arrow(aes(arrow_head = as.integer(group)), resect = 5) +
  scale_arrow_head_continuous(
    generator = arrow_star, map_arg = "n",
    range = c(3, 7)
  )

