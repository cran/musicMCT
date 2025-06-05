## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(musicMCT)

## -----------------------------------------------------------------------------
ivec(sc(7,35))

## -----------------------------------------------------------------------------
melodic_minor <- c(0, 2, 3, 5, 7, 9, 11)
sim(melodic_minor)

## -----------------------------------------------------------------------------
j(dia)

## -----------------------------------------------------------------------------
sim(melodic_minor)[, 4]

## -----------------------------------------------------------------------------
c_major <- c(0, 2, 4, 5, 7, 9, 11)
minimize_vl(c_major, sim(melodic_minor)[, 4])

## -----------------------------------------------------------------------------
overtones <- 7:13
frequency_ratios <- overtones / 7
semitone_values <- 12 * log2(frequency_ratios)
overtone_scale <- sim(semitone_values)[, 2]
print(overtone_scale)

## -----------------------------------------------------------------------------
acoustic_scale <- sim(melodic_minor)[, 4]
minimize_vl(overtone_scale, acoustic_scale)

## -----------------------------------------------------------------------------
round(overtone_scale, digits=0)

## -----------------------------------------------------------------------------
round(overtone_scale, digits=0)
round(tn(overtone_scale, 1), digits=0)

## -----------------------------------------------------------------------------
amounts_to_transpose <- (0:99)/100
transposed_scales <- sapply(amounts_to_transpose, tn, set=overtone_scale)
quantized_scales <- apply(transposed_scales, 2, round, digits=0)
unique_quantizations <- unique(quantized_scales, MARGIN=2)
print(unique_quantizations)

## -----------------------------------------------------------------------------
unique_quantizations_from_0 <- apply(unique_quantizations, 2, startzero)
final_quantizations <- unique(unique_quantizations_from_0, MARGIN=2)
colnames(final_quantizations) <- apply(final_quantizations, 2, fortenum)
print(final_quantizations)

## ----echo=FALSE---------------------------------------------------------------
quantization_labels <- apply(final_quantizations, 2, fortenum)
quantization_weights <- diff(which(duplicated(quantized_scales, MARGIN=2)==FALSE))
quantization_weights[1] <- quantization_weights[1] + 1
names(quantization_weights) <- quantization_labels
print(quantization_weights)

## -----------------------------------------------------------------------------
getineqmat(4)

## -----------------------------------------------------------------------------
signvector(sc(4, 6))
signvector(sc(4, 24))

## -----------------------------------------------------------------------------
signvector(acoustic_scale)
signvector(overtone_scale)

## -----------------------------------------------------------------------------
whichsvzeroes(overtone_scale)

## -----------------------------------------------------------------------------
getineqmat(7)[whichsvzeroes(overtone_scale),]

## -----------------------------------------------------------------------------
signed_interval_class(overtone_scale[5]-overtone_scale[1])
signed_interval_class(overtone_scale[5]-overtone_scale[2])

## -----------------------------------------------------------------------------
signvector(acoustic_scale)[38]

## -----------------------------------------------------------------------------
all_signvectors <- apply(final_quantizations, 2, signvector)
all_signvectors[38, ]

## -----------------------------------------------------------------------------
signvectors_with_overtone_scale <- rbind(signvector(overtone_scale), t(all_signvectors))
rownames(signvectors_with_overtone_scale)[1] <- "o.s."
dist(signvectors_with_overtone_scale)

## -----------------------------------------------------------------------------
howfree(overtone_scale)
apply(final_quantizations, 2, howfree)

## -----------------------------------------------------------------------------
all_12edo_heptachords <- sapply(1:38, sc, card=7)
heptachord_freedoms <- apply(all_12edo_heptachords, 2, howfree)
table(heptachord_freedoms)

## -----------------------------------------------------------------------------
quantized_overtone_color <- quantize_color(overtone_scale)
print(quantized_overtone_color)

## -----------------------------------------------------------------------------
round(overtone_scale, digits=2)
convert(quantized_overtone_color$set, 30, 12)
isTRUE(all.equal(signvector(overtone_scale), signvector(quantized_overtone_color$set, edo=30)))

## -----------------------------------------------------------------------------
overtone_sv <- signvector(overtone_scale)
apply(all_signvectors, 2, comparesignvecs, signvecY=overtone_sv)

## ----eval=FALSE---------------------------------------------------------------
# representative_scales <- readRDS("representative_scales.rds")
# representative_signvectors <- readRDS("representative_signvectords.rds")
# color_adjacencies <- readRDS("color_adjacencies.rds")

## -----------------------------------------------------------------------------
colornum(overtone_scale)

## ----eval=exists("color_adjacencies") && exists("representative_scales") && exists("representative_signvectors")----
# os_adjacent_colors <- color_adjacencies[[7]][[colornum(overtone_scale)]]
# os_adjacent_scales <- representative_scales[[7]][, os_adjacent_colors]
# regularity <- apply(os_adjacent_scales, 2, countsvzeroes)
# most_regular_approximation <- os_adjacent_scales[, which.max(regularity)]
# quantize_color(most_regular_approximation)

## -----------------------------------------------------------------------------
asword(overtone_scale)
best_simple_approximation <- convert(c(0, 2, 4, 6, 8, 10, 11), 14, 12)
asword(best_simple_approximation)

## -----------------------------------------------------------------------------
signvector(overtone_scale)[10]
signvector(best_simple_approximation)[10]
signvector(acoustic_scale)[10]

## -----------------------------------------------------------------------------
round(minimize_vl(overtone_scale, acoustic_scale), 3)
round(minimize_vl(overtone_scale, best_simple_approximation), 3)

## ----fig.width=5, fig.height=5, fig.fullwidth=TRUE----------------------------
brightnessgraph(acoustic_scale)

## ----fig.width=5, fig.height=5, fig.fullwidth=TRUE----------------------------
brightnessgraph(overtone_scale, numdigits=1, show_sums=FALSE)

## ----fig.width=5, fig.height=5, fig.fullwidth=TRUE----------------------------
brightnessgraph(quantized_overtone_color$set, edo=30, show_sums=FALSE)

## ----fig.width=5, fig.height=5, fig.fullwidth=TRUE----------------------------
brightnessgraph(final_quantizations[, 4])

## ----fig.width=5, fig.height=5, fig.fullwidth=TRUE----------------------------
brightnessgraph(best_simple_approximation, show_pitches=FALSE, show_sums=FALSE)

