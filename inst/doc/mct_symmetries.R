## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(musicMCT)

## -----------------------------------------------------------------------------
blues <- c(0, 3, 5, 6, 7, 10)

## -----------------------------------------------------------------------------
double_harmonic <- realize_stepword(c(1, 3, 1, 2, 1, 3, 1))
blackkey_pentatonic <- maxeven(5, 12)
all_interval_tetrachord <- sc(4, 29)

print(double_harmonic)
print(blackkey_pentatonic)
print(all_interval_tetrachord)

## -----------------------------------------------------------------------------
blackkey_pentatonic[3]

## -----------------------------------------------------------------------------
c_major <- c(0, 2, 4, 5, 7, 9, 11)
coord_to_edo(c_major)

coord_to_edo(blues)

## -----------------------------------------------------------------------------
cis_diminished <- c(1, 0, -1)
coord_from_edo(cis_diminished)

## -----------------------------------------------------------------------------
augmented_triad <- edoo(3)
equiheptatonic_scale <- edoo(7)

print(augmented_triad)
print(equiheptatonic_scale)

# edoo(n) does look like the origin after applying coord_to_edo():
coord_to_edo(equiheptatonic_scale)

## -----------------------------------------------------------------------------
g <- 2
i <- 4
sim(blues)[g+1, i]

## -----------------------------------------------------------------------------
tetrachord_arrangement <- getineqmat(4)
print(tetrachord_arrangement)

## -----------------------------------------------------------------------------
dom7 <- c(0, 4, 7, 10)
x <- coord_to_edo(dom7)

hyperplane_v <- tetrachord_arrangement[7, ]
print(hyperplane_v)

# Let's leave off the last column:
hyperplane_v <- hyperplane_v[-5]
print(hyperplane_v)

# Now we can take the dot product of v and x as in the example:
hyperplane_v %*% x

## -----------------------------------------------------------------------------
signvector(dom7)

## -----------------------------------------------------------------------------
signvector(c_major)[23]

## -----------------------------------------------------------------------------
# P_theta:
ineqsym(a=2, b=0, card=5)

# P_psi:
ineqsym(a=2, b=1, card=5)

## -----------------------------------------------------------------------------
# Using the transformation Theta:
ineqsym(set=blackkey_pentatonic, a=2, b=0)

# Using the transformation Psi:
ineqsym(set=blackkey_pentatonic, a=2, b=1)

## -----------------------------------------------------------------------------
image_of_blues <- ineqsym(blues, 5, 0)

print(image_of_blues)
fortenum(image_of_blues)

## -----------------------------------------------------------------------------
spectrumcount(blackkey_pentatonic)
spectrumcount(c_major)

## -----------------------------------------------------------------------------
spectrumcount(blackkey_pentatonic)
spectrumcount(ineqsym(blackkey_pentatonic, 2, 0))

spectrumcount(blues)
spectrumcount(image_of_blues)

## -----------------------------------------------------------------------------
melodic_minor <- c(0, 2, 3, 5, 7, 9, 11)
melmin_image <- ineqsym(melodic_minor, 2, 0)

spectrumcount(melodic_minor)
spectrumcount(melmin_image)

## -----------------------------------------------------------------------------
k <- 2
rows_with_skips <- get_relevant_rows(k, tetrachord_arrangement)
print(rows_with_skips)

skip_subarrangement <- tetrachord_arrangement[rows_with_skips, ]
print(skip_subarrangement)

## -----------------------------------------------------------------------------
ineqsym(card=5, a=1, b=0, involution=TRUE)

## -----------------------------------------------------------------------------
blues_involution <- ineqsym(blues, involution=TRUE)
print(blues_involution)

fortenum(blues_involution)

normal_form(blues_involution, optic="optc")
normal_form(image_of_blues, optic="optc")

## ----fig.width=5, fig.height=5, fig.fullwidth=TRUE----------------------------
tetrachord_F <- c(0, 2, 4, 7)
tetrachord_G <- c(0, 1, 4, 8)

brightnessgraph(tetrachord_F)

## ----fig.width=5, fig.height=5, fig.fullwidth=TRUE----------------------------
brightnessgraph(tetrachord_G)

## ----fig.width=5, fig.height=5, fig.fullwidth=TRUE----------------------------
brightnessgraph(tetrachord_F, show_sums=FALSE, show_pitches=FALSE)

## ----fig.width=5, fig.height=5, fig.fullwidth=TRUE----------------------------
brightnessgraph(tetrachord_G, show_sums=FALSE, show_pitches=FALSE)

## -----------------------------------------------------------------------------
vlsig(tetrachord_F, index=2)
vlsig(tetrachord_G, index=1)

## -----------------------------------------------------------------------------
iswellformed(blackkey_pentatonic)
iswellformed(ineqsym(set=blackkey_pentatonic, a=2, b=0))
iswellformed(ineqsym(set=blackkey_pentatonic, a=2, b=1))

## -----------------------------------------------------------------------------
blackkey_pentatonic |> ineqsym(a=2, b=0) |> iswellformed()

## -----------------------------------------------------------------------------
just_diatonic <- j(dia) # Defined in footnote 7 of "Symmetries"

iswellformed(just_diatonic)

isgwf(just_diatonic)
howfree(just_diatonic)

# Thus it is pairwise well-formed.
# Like all PWF scales, it is trivalent:

spectrumcount(just_diatonic)

## ----fig.width=5, fig.height=5, fig.fullwidth=TRUE----------------------------
pentachord_F <- c(0, 1, 4, 6, 9)
pentachord_G <- c(0, 2, 3, 5, 7)

brightnessgraph(pentachord_F, show_sums=FALSE, show_pitches=FALSE)

## ----fig.width=5, fig.height=5, fig.fullwidth=TRUE----------------------------
brightnessgraph(pentachord_G, show_sums=FALSE, show_pitches=FALSE)

## ----fig.width=5, fig.height=5, fig.fullwidth=TRUE----------------------------
brightnessgraph(blues)

## ----fig.width=5, fig.height=5, fig.fullwidth=TRUE----------------------------
brightnessgraph(image_of_blues)

## ----fig.width=5, fig.height=5, fig.fullwidth=TRUE----------------------------
brightnessgraph(blues_involution)

## -----------------------------------------------------------------------------
pentatonic_palette <- scale_palette(blackkey_pentatonic)
print(pentatonic_palette)

apply(pentatonic_palette, 2, iswellformed)

## -----------------------------------------------------------------------------
apply(pentatonic_palette, 2, evenness) |> round(digits=5)

## -----------------------------------------------------------------------------
blues_palette <- scale_palette(blues, include_involution=FALSE)
print(blues_palette)

## -----------------------------------------------------------------------------
apply(blues_palette, 2, normal_form, optic="optc")

## -----------------------------------------------------------------------------
more_blues <- scale_palette(blues, include_involution=TRUE)
print(more_blues)

## -----------------------------------------------------------------------------
golomb_ruler <- c(0, 1, 4, 9, 11)
golomb_scale <- convert(golomb_ruler, 15, 12)

coord_to_edo(golomb_scale) |> round(digits=3)
golomb_scale
ineqsym(golomb_scale, a=4, b=3) |> round(digits=3)

# This computation could also be done in 15-tone equal temperament,
# working directly with golomb_ruler and using the "edo" parameter:

coord_to_edo(golomb_ruler, edo=15)
ineqsym(golomb_ruler, a=4, b=3, edo=15)

## -----------------------------------------------------------------------------
P <- ineqsym(card=5, a=4, b=3)
print(P)

eigen(P)

## -----------------------------------------------------------------------------
eigenvector_y <- c(0, -1, 1, 0, 0)
eigenvector_z <- c(1, 0, 0, -1, 0)

eigenscale <- coord_from_edo(eigenvector_y + eigenvector_z)
eigenscale_involution <- coord_from_edo(-1 * (eigenvector_y + eigenvector_z))

print(eigenscale)
print(eigenscale_involution)

ineqsym(eigenscale, a=4, b=3)

## -----------------------------------------------------------------------------
eigenscale_involution

saturate(-1, eigenscale)

## -----------------------------------------------------------------------------
dorian <- c(0, 2, 3, 5, 7, 9, 10)

sigma_6_0 <- ineqsym(card=7, a=6, b=0)
inversion <- -1 * sigma_6_0

inversion %*% coord_to_edo(dorian) |> coord_from_edo() |> t() |> as.numeric()

## -----------------------------------------------------------------------------
phrygian <- c(0, 1, 3, 5, 7, 8, 10)

inversion %*% coord_to_edo(phrygian) |> coord_from_edo() |> t() |> as.numeric()

## ----fig.width=5, fig.height=5, fig.fullwidth=TRUE----------------------------
inverted_blues <- ineqsym(blues, a=5, b=0) |> saturate(r=-1)
print(inverted_blues)

clockface(blues)

## ----fig.width=5, fig.height=5, fig.fullwidth=TRUE----------------------------
clockface(inverted_blues)

## -----------------------------------------------------------------------------
tni(blues, 0)

## -----------------------------------------------------------------------------
forza <- c(0, 2, 3, 7)
destino <- ineqsym(forza, a=1, b=2, involution=TRUE)

## -----------------------------------------------------------------------------
print(destino)

tn(forza, 3)

## -----------------------------------------------------------------------------
ineqsym(forza, a=3, b=0, involution=FALSE)
tni(forza, 3)

## -----------------------------------------------------------------------------
ait <- c(0, 1, 4, 6)

scale_palette(ait, include_involution=TRUE)

## -----------------------------------------------------------------------------
scale_palette(forza, include_involution=TRUE)

## -----------------------------------------------------------------------------
schbeg <- sc(6, 44)
scale_palette(schbeg) # Only 12 entries

isym(schbeg) # Not inversionally symmetrical
ineqsym(schbeg, a=5, b=0) # Is symmetrical under this transformation

## -----------------------------------------------------------------------------
set <- c(0, 1, 4, 8)
fortenum(set)

(set * 5) %% 12 |> fortenum()
(set * 7) %% 12 |> fortenum()

set_image <- ineqsym(set, a=3, b=0)
print(set_image)
fortenum(set_image)

## -----------------------------------------------------------------------------
just_chromatic_scale <- j(1, m2, 2, m3, 3, 4, jtt, 5, m6, 6, m7, 7)
print(just_chromatic_scale)

## -----------------------------------------------------------------------------
adjusted_chromatic_scale <- ineqsym(just_chromatic_scale, a=5, b=0)
print(adjusted_chromatic_scale)

## -----------------------------------------------------------------------------
primeform(just_chromatic_scale)
primeform(adjusted_chromatic_scale)

## -----------------------------------------------------------------------------
spectrumcount(just_chromatic_scale)
spectrumcount(adjusted_chromatic_scale)

