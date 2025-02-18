
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggarrow <img src="man/figures/logo.png" align="right" height="138" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
![CRAN status](https://www.r-pkg.org/badges/version/ggarrow)
[![R-CMD-check](https://github.com/teunbrand/ggarrow/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/teunbrand/ggarrow/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/teunbrand/ggarrow/branch/main/graph/badge.svg)](https://app.codecov.io/gh/teunbrand/ggarrow?branch=main)
<!-- badges: end -->

The goal of ggarrow is to draw arrows in
[{ggplot2}](https://ggplot2.tidyverse.org/). It is a [{ggplot2}
extension](https://exts.ggplot2.tidyverse.org/gallery/) package that
focusses on specialised geometry layers to expand the toolkit of arrows.
While you’re reading this, I’ll take this opportunity to inform you that
while this package probably works, it is not very polished.

## Installation

You can install the development version of ggarrow from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("teunbrand/ggarrow")
```

## Arrows

They’re made for pointing at things. The workhorse functionality is in
the `geom_arrow()` function that, unsurprisingly, draws arrows.

### Basic arrows

``` r
library(ggarrow)
#> Loading required package: ggplot2

p <- ggplot(whirlpool(5), aes(x, y, colour = group)) +
  coord_equal() +
  guides(colour = "none")
p + geom_arrow()
```

<img src="man/figures/README-basic_example-1.png" width="80%" style="display: block; margin: auto;" />

### Variable width

Arrows, in contrast to vanilla lines, can have variable widths.

``` r
p + geom_arrow(aes(linewidth = I(arc))) # Identity scale for linewidth
```

<img src="man/figures/README-variable_width-1.png" width="80%" style="display: block; margin: auto;" />

### Inner arrows

Besides varying linewidths, there is also an option to place arrows
along the path. You could draw arbitrarily many of these, but I doubt
that will look pretty.

``` r
p + geom_arrow(arrow_mid = "head_wings", mid_place = c(0.25, 0.5, 0.75))
```

<img src="man/figures/README-middle_arrows-1.png" width="80%" style="display: block; margin: auto;" />

### Ornament styles

You can also tweak what the arrows should look like. The example below
is a bit verbose, but gives an impression of the available options by
combining different styles of arrow heads and what are termed ‘arrow
fins’.

``` r
p + geom_arrow(aes(arrow_head = group, arrow_fins = group), linewidth = 2) +
  scale_arrow_head_discrete(values = list(
    "head_wings",
    arrow_head_wings(offset = 20, inset = 70),
    arrow_head_line(lineend = "parallel"),
    arrow_head_line(45, lineend = "round"),
    "head_minimal"
  ), guide = "none") +
  scale_arrow_fins_discrete(values = list(
    "fins_feather",
    arrow_fins_feather(indent = 0, outdent = 0, height = 1),
    "fins_line",
    arrow_fins_line(90),
    "fins_minimal"
  ), guide = "none")
```

<img src="man/figures/README-show_ornaments-1.png" width="80%" style="display: block; margin: auto;" />

There are some other geoms that mimic bread-and-butter ggplot2 layers,
such as `geom_arrow_segment()` and `geom_arrow_curve()`, that add the
same arrow functionality on top of the `geom_segment()` and
`geom_curve()` layers.

### Chains

Aside from these, there is also `geom_arrow_chain()`, which has no
equivalent in vanilla ggplot2. It adds arrows in between points, and
dodges the endpoints a bit so that they don’t seem to touch. In the
example below, we can see that we can dodge points of different sizes.

``` r
t <- seq(0, 2 * pi, length.out = 11)
l <- rep(c(1, 0.4), length.out = 11)

df <- data.frame(
  x = cos(t) * l,
  y = sin(t) * l,
  size = l
)

ggplot(df, aes(x, y, size = size)) +
  geom_point(colour = 2) +
  geom_arrow_chain() +
  coord_equal()
```

<img src="man/figures/README-arrow_chain-1.png" width="80%" style="display: block; margin: auto;" />

### Theme elements

Because arrows are almost drop-in replacements for lines, I also
included `element_arrow()` as a theme element. With function, you can
set any line element in the theme to an arrow, with similar
customisation options as the layers.

``` r
p + geom_arrow() +
  theme(
    axis.line.x = element_arrow(
      arrow_head = "head_wings", linewidth_head = 1.5, linewidth_fins = 0
    ),
    axis.line.y = element_arrow(arrow_head = "head_line"),
    axis.ticks.length = unit(0.4, "cm"),
    axis.ticks.x = element_arrow(linewidth_fins = 0, linewidth_head = 2),
    axis.ticks.y = element_arrow(arrow_fins = "head_line"),
    panel.grid.major = element_arrow(
      linewidth_head = 5, linewidth_fins = 0,
      resect_head = 5, resect_fins = 5, lineend = "round"
    ),
    panel.grid.minor = element_arrow(
      linewidth_head = 0, linewidth_fins = 5
    )
  )
```

<img src="man/figures/README-theme_elements-1.png" width="80%" style="display: block; margin: auto;" />

## Limitations

The current limitation is that variable width paths don’t lend
themselves well to jagged paths with short segments. This is because I
had to implement linejoins for variable width paths and I barely have
high-school level understanding of trigonometry. Consequently, the
linejoins look bad with short jagged segments.

``` r
ggplot(economics, aes(date, unemploy)) +
  geom_arrow(aes(linewidth = date))
```

<img src="man/figures/README-jagged-1.png" width="80%" style="display: block; margin: auto;" />

The best advice I can give for the jagged linejoins is to smooth the
data beforehand.

``` r
ggplot(economics, aes(date, unemploy)) +
  geom_arrow(
    stat = "smooth", formula = y ~ x, method = "loess", span = 0.05,
    aes(linewidth = after_stat(x))
  )
```

<img src="man/figures/README-smoothed-1.png" width="80%" style="display: block; margin: auto;" />

With regards to the legend-keys, that’ll require a change in ggplot2’s
code.

## Dependency statement

The {ggarrow} package largely takes on the same dependencies as
{ggplot2} to keep it on the lightweight side. However, this package
wouldn’t work at all without the {polyclip} dependency, which is the
only one outside {ggplot2}’s imports.

## Related work

Of course, the {grid} package, on which {ggplot2} is build upon, offers
some options for arrows. The
[{arrowheadr}](https://github.com/wjschne/arrowheadr) package provides
some great extensions for arrowheads. The
[{vwlines}](https://cran.r-project.org/package=vwline) package that
handles variable widths lines much more graciously than this package.
Both the [{gggenes}](https://wilkox.org/gggenes/) and
[{gggenomes}](https://thackl.github.io/gggenomes/) packages use arrows
in a domain-specific context. For vector field visualisation, there is
the [{ggquiver}](http://pkg.mitchelloharawild.com/ggquiver/) package.
The [{ggarchery}](https://github.com/mdhall272/ggarchery) package also
provides extended options for the `geom_segment()` parametrisation of
lines.
