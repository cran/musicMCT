## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(musicMCT)

## -----------------------------------------------------------------------------
tc(c(0, 2, 4, 5), c(0, 7))

## -----------------------------------------------------------------------------
scale_from_genus <- function(genus) {
 sd2 <- genus[1]
 sd3 <- genus[2]
 if (sd2 > sd3) {
   return(NA)
 }
 tc(c(0, sd2, sd3, 5), c(0, 7))
}

## ----out.width = '75%', echo = FALSE------------------------------------------
knitr::include_graphics("img/vhd_plane_embedding.png")

## ----fig.width=5, fig.height=5, fig.fullwidth=TRUE----------------------------
ionian <- scale_from_genus(c(2, 4)) #1
dorian <- scale_from_genus(c(2, 3)) #2
phrygian <- scale_from_genus(c(1, 3)) #3
double_harmonic <- scale_from_genus(c(1, 4)) #4
enharmonic <- scale_from_genus(c(.5, 1)) #5

demo_scales <- cbind(ionian, dorian, phrygian, double_harmonic, enharmonic)

## ----fig.width=5, fig.height=5, fig.fullwidth=TRUE----------------------------
tetra_plot <- function(scales, title, ...) {
  oldpar <- par(bg='aliceblue')
  on.exit(par(oldpar))

  plot(scales[2, ], 
       scales[3, ], 
       xlab="Height of scale degree 2", xlim=c(-.05, 5.05),
       ylab="Height of scale degree 3", ylim=c(-.01, 5.01),
       ...)
  grid(col="gray35")
  mtext(side=3, title, font=2, line=1)
}
tetra_plot(demo_scales, 
           "Location of 5 Reference Scales in the tc() Plane",
           pch=sapply(1:5, toString)) 

## ----fig.width=5, fig.height=5, fig.fullwidth=TRUE----------------------------
# Invert the original enharmonic scale:
inverted_enharmonic <- tni(enharmonic, 0)

# Define a new enharmonic scale based on where we expect to plot it:
new_enharmonic <- scale_from_genus(c(4, 4.5))

# The two are the same:
rbind(inverted_enharmonic, new_enharmonic)

# Let's plot them:
demo_scales <- cbind(demo_scales, inverted_enharmonic)

tetra_plot(demo_scales, 
           "Location of 6 Reference Scales in the tc() Plane",
           pch=sapply(1:6, toString)) 

## -----------------------------------------------------------------------------
show_landmarks <- function() {
  points(demo_scales[2, ], demo_scales[3, ], pch=19, cex=2.5, col="white")
  points(demo_scales[2, ], demo_scales[3, ], pch=sapply(1:6, toString), font=2)
}

## -----------------------------------------------------------------------------
num_points <- 4000
parhypatai <- runif(num_points, 0, 5)
lichanoi <- runif(num_points, 0, 5)
inputs <- rbind(parhypatai, lichanoi) |> apply(MARGIN=2, FUN=sort)
random_scales <- apply(inputs, 2, scale_from_genus)
all_signvectors <- apply(random_scales, 2, signvector)
unique_signvectors <- all_signvectors |> unique(MARGIN=2) |> apply(MARGIN=2, FUN=toString)
unique_signvectors <- sort(unique_signvectors)
length(unique_signvectors)

## ----echo=FALSE---------------------------------------------------------------
new_order <- c(1, 15, 17, 21, 3, 19, 25, 11, 9, 13, 5, 23, 7, 2, 16, 18, 22, 4, 20, 26, 12, 10, 6, 24, 8, 14)
unique_signvectors <- unique_signvectors[new_order]

## ----fig.width=5, fig.height=5, fig.fullwidth=TRUE----------------------------
match_sv <- function(sv) {
  res <- which(unique_signvectors == toString(sv))
  if (length(res)==0) {
    return(0)
  }
  res
}

scalar_colors <- apply(all_signvectors, 2, match_sv)
display_colors <- palette.colors(26, palette="Polychrome 36")[scalar_colors]

tetra_plot(random_scales, 
           "26 Scalar Colors in the tc() Plane",
           col=display_colors,
           pch=20)
show_landmarks()

## -----------------------------------------------------------------------------
howfree(double_harmonic)

## -----------------------------------------------------------------------------
howfree(ionian)
howfree(dorian)
howfree(phrygian)

## ----fig.width=5, fig.height=5, fig.fullwidth=TRUE----------------------------
projected_scales <- apply(random_scales, 2, match_flat, target_scale=double_harmonic)
colors_for_projected_scales <- rep("black", num_points)
scales_for_fig5 <- cbind(random_scales, projected_scales)
colors_for_fig5 <- c(display_colors, colors_for_projected_scales)

tetra_plot(scales_for_fig5,
           "Double Harmonic's Flat as a Line of Black Points",
           col=colors_for_fig5,
           pch=20)
show_landmarks()

## -----------------------------------------------------------------------------
test_for_pwf <- apply(projected_scales, 2, isgwf)
table(test_for_pwf)

## ----fig.width=5, fig.height=5, fig.fullwidth=TRUE----------------------------
evenness_values <- apply(projected_scales, 2, evenness)
sizes_for_fig6 <- c(rep(1, num_points), max(evenness_values)-evenness_values)

tetra_plot(scales_for_fig5,
           "Line Thickness Represents Scale Evenness",
           col=colors_for_fig5,
           pch=20,
           cex=sizes_for_fig6)
show_landmarks()

## ----fig.width=5, fig.height=5, fig.fullwidth=TRUE----------------------------
zoomed_tetra_plot <- function(scales, title, ...) {
  oldpar <- par(bg='aliceblue')
  on.exit(par(oldpar))
  plot(scales[2,], 
       scales[3, ], 
       xlab="Height of scale degree 2", xlim=c(0.95, 2.05),
       ylab="Height of scale degree 3", ylim=c(2.99, 4.01),
       ...)
  grid(col="gray35")
  mtext(side=3, title, font=2, line=1)
}

zoomed_parhypatai <- runif(num_points, 1, 2)
zoomed_lichanoi <- runif(num_points, 3, 4)
zoomed_inputs <- rbind(zoomed_parhypatai, zoomed_lichanoi)
zoomed_sets <- apply(zoomed_inputs, 2, scale_from_genus)
zoomed_signvectors <- apply(zoomed_sets, 2, signvector)
zoomed_scalar_colors <- apply(zoomed_signvectors, 2, match_sv)
zoomed_display_colors <- palette.colors(26, palette="Polychrome 36")[zoomed_scalar_colors]

points_for_fig7 <- cbind(zoomed_sets, projected_scales)
colors_for_fig7 <- c(zoomed_display_colors, colors_for_projected_scales)
sizes_for_fig7 <-c(rep(1, num_points), 3^(evenness(double_harmonic)-evenness_values))

zoomed_tetra_plot(points_for_fig7, 
                  "Scalar Colors in the tc() Plane's Most Interesting Zone",
                  col=colors_for_fig7,
                  pch=20,
                  cex=sizes_for_fig7)
show_landmarks()

## -----------------------------------------------------------------------------
which_most_even <- which.min(evenness_values)
projected_scales[, which_most_even]

## -----------------------------------------------------------------------------
naive_guess <- scale_from_genus(c(1+(2/3), 3+(1/3)))
actual_optimum <- scale_from_genus(c(23/14, 47/14))

evenness(naive_guess)
evenness(actual_optimum)

## ----fig.width=5, fig.height=5, fig.fullwidth=TRUE----------------------------
zoomed_tetra_plot(points_for_fig7, 
                  "Most Even Scale at the Point Labeled #7",
                  col=colors_for_fig7,
                  pch=20,
                  cex=sizes_for_fig7)
show_landmarks()
points(23/14, 47/14, pch=19, cex=2.5, col="white")
points(23/14, 47/14, pch="7", font=2)

## -----------------------------------------------------------------------------
howfree(naive_guess)
iswellformed(naive_guess)

## ----fig.width=5, fig.height=5, fig.fullwidth=TRUE----------------------------
# Define a grid that covers the tc() plane evenly.
# x and y both range from 0 to 5 but with slight offsets so that we only see 3-D scales:
grid_subdivisions <- 100
x <- seq(0.001, 4.999, length.out=grid_subdivisions) 
y <- seq(0, 4.98, length.out=grid_subdivisions)

# Calculate the scale structures. (Cf. the definition of `scalar_colors` for Figure 4.)
color_from_point <- function(x, y) scale_from_genus(c(x, y)) |> signvector() |> match_sv()
scalar_colors <- outer(x, y, Vectorize(color_from_point))

# Create visuals
color_palette <- c("aliceblue", palette.colors(26, palette="Polychrome 36"))
oldpar <- par(bg="aliceblue")

image(x, y, z=scalar_colors, col=color_palette,
      xlab="Height of Scale Degree 2", ylab="Height of Scale Degree 3")
mtext(side=3, "26 Scalar Colors in the tc() Plane", font=2, line=1)
show_landmarks()

## ----echo=FALSE---------------------------------------------------------------
par(oldpar)

## ----fig.width=7, fig.height=5, fig.fullwidth=TRUE----------------------------
# Position plot and legend
oldpar <- par(no.readonly=TRUE)
layout(matrix(c(1, 2), ncol=2), widths=c(5, 2))

# Plot the evenness values
measure_evenness <- function(x, y) scale_from_genus(c(x, y)) |> evenness()
evenness_values <- outer(x, y, Vectorize(measure_evenness))
fig10_palette <- hcl.colors(100, palette="Green-Brown")

par(bg="aliceblue")
image(x, y, z=evenness_values, col=fig10_palette,
      xlab="Height of Scale Degree 2", ylab="Height of Scale Degree 3")
points(23/14, 47/14, pch=4, col="white", cex=1.5)
mtext(side=3, "Evenness of Scales in the tc() Plane", font=2, line=1)

# Make the legend
color_legend <- as.raster(matrix(rev(fig10_palette), ncol=1))
ymin <- min(evenness_values[!is.na(evenness_values)])
ymax <- max(evenness_values[!is.na(evenness_values)])
plot(c(0, 1), c(ymin, ymax), type="n", axes=FALSE, xlab="", ylab="Evenness")

rasterImage(color_legend, 0, ymin, 1, ymax)
axis(2, at = round(seq(ymin, ymax, length.out=5), 2))

## ----echo=FALSE---------------------------------------------------------------
par(oldpar)

## ----fig.width=7, fig.height=5, fig.fullwidth=TRUE----------------------------
oldpar <- par(no.readonly=TRUE)
layout(matrix(c(1, 2), ncol=2), widths=c(5, 2))

# Plot the brightness ratio values
measure_ratio <- function(x, y) scale_from_genus(c(x, y)) |> ratio()
ratio_values <- outer(x, y, Vectorize(measure_ratio))
fig11_palette <- hcl.colors(100, palette="Green-Brown")

par(bg="aliceblue")
image(x, y, z=ratio_values, col=fig11_palette,
      xlab="Height of Scale Degree 2", ylab="Height of Scale Degree 3")
mtext(side=3, '"Brightness Ratio" Values in the tc() Plane', font=2, line=1)

# Make the legend
color_legend <- as.raster(matrix(rev(fig11_palette), ncol=1))
ymin <- min(ratio_values[!is.na(ratio_values)])
ymax <- max(ratio_values[!is.na(ratio_values)])
plot(c(0, 1), c(ymin, ymax), type="n", axes=FALSE, xlab="", ylab="Brightness Ratio")

rasterImage(color_legend, 0, ymin, 1, ymax)
axis(2, at = round(seq(ymin, ymax, length.out=5), 2))

## ----echo=FALSE---------------------------------------------------------------
par(oldpar)

## ----fig.width=7, fig.height=5, fig.fullwidth=TRUE----------------------------
oldpar <- par(no.readonly=TRUE)
layout(matrix(c(1, 2), ncol=2), widths=c(5, 2))

# Plot the brightness ratio values
measure_ratio <- function(x, y) scale_from_genus(c(x, y)) |> ratio()
ratio_values <- outer(x, y, Vectorize(measure_ratio))
fig11_palette <- hcl.colors(100, palette="Green-Brown")

par(bg="aliceblue")
image(x, y, z=ratio_values, col=fig11_palette,
      xlab="Height of Scale Degree 2", ylab="Height of Scale Degree 3")
mtext(side=3, 'Contour Plot of "Brightness Ratio" in the tc() Plane', font=2, line=1)

# Add the contour plot
contour(x, y, z=ratio_values, add=TRUE)

# Make the legend
color_legend <- as.raster(matrix(rev(fig11_palette), ncol=1))
ymin <- min(ratio_values[!is.na(ratio_values)])
ymax <- max(ratio_values[!is.na(ratio_values)])
plot(c(0, 1), c(ymin, ymax), type="n", axes=FALSE, xlab="", ylab="Brightness Ratio")
rasterImage(color_legend, 0, ymin, 1, ymax)
axis(2, at = round(seq(ymin, ymax, length.out=5), 2))

## ----echo=FALSE---------------------------------------------------------------
par(oldpar)

## ----fig.width=7, fig.height=5, fig.fullwidth=TRUE----------------------------
oldpar <- par(no.readonly=TRUE)
layout(matrix(c(1, 2), ncol=2), widths=c(5, 2))

grid_subdivisions <- 100
u <- seq(1.001, 1.999, length.out=grid_subdivisions)
v <- seq(3, 3.98, length.out=grid_subdivisions)

ratio_values2 <- outer(u, v, Vectorize(measure_ratio))
fig11_palette <- hcl.colors(100, palette="Green-Brown")

par(bg="aliceblue")
image(u, v, z=ratio_values2, col=fig11_palette,
      xlab="Height of Scale Degree 2", ylab="Height of Scale Degree 3")
contour(u, v, z=ratio_values2, add=TRUE)
mtext(side=3, "\"Brightness Ratio\" at Center of tc() Plane", font=2, line=1)

# Make the legend
color_legend <- as.raster(matrix(rev(fig11_palette), ncol=1))
ymin <- min(ratio_values2[!is.na(ratio_values)])
ymax <- max(ratio_values2[!is.na(ratio_values)])
plot(c(0, 1), c(ymin, ymax), type="n", axes=FALSE, xlab="", ylab="Brightness Ratio")

rasterImage(color_legend, 0, ymin, 1, ymax)
axis(2, at = round(seq(ymin, ymax, length.out=5), 2))

## ----echo=FALSE---------------------------------------------------------------
par(oldpar)

## -----------------------------------------------------------------------------
ratio(scale_from_genus(c(1.5, 3.5)))

