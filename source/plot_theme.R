library(ggplot2)

# HESS submission constants
HESS_SINGLE_COL_CM <- 8
HESS_DOUBLE_COL_CM <- 17.4
HESS_DPI <- 300

cm_to_in <- function(cm) cm / 2.54

# Shared ggplot2 theme for RSFME figures (HESS submission)
# Sans-serif font, clean styling, consistent sizing
theme_rsfme <- function(base_size = 10, base_family = "Helvetica") {
    theme_classic(base_size = base_size, base_family = base_family) %+replace%
        theme(
            text = element_text(size = base_size),
            axis.title = element_text(size = base_size),
            axis.text = element_text(size = base_size - 1),
            strip.text = element_text(size = base_size, face = "bold"),
            strip.background = element_blank(),
            legend.title = element_text(size = base_size),
            legend.text = element_text(size = base_size - 1),
            panel.spacing = unit(0.5, "lines"),
            plot.title = element_text(size = base_size + 1, face = "bold", hjust = 0),
            plot.margin = margin(5, 5, 5, 5)
        )
}

# Colorblind-friendly method palette (Paul Tol's qualitative scheme)
method_colors <- c(
    "li"        = "#4477AA",
    "beale"     = "#EE6677",
    "rating"    = "#228833",
    "composite" = "#CCBB44",
    "truth"     = "#000000"
)

method_labels <- c(
    "li"        = "Linear Interpolation",
    "beale"     = "Beale",
    "rating"    = "Rating",
    "composite" = "Composite",
    "truth"     = "Truth"
)

scale_color_methods <- function(...) {
    scale_color_manual(values = method_colors, labels = method_labels, ...)
}

scale_fill_methods <- function(...) {
    scale_fill_manual(values = method_colors, labels = method_labels, ...)
}

# Error band colors
error_band_colors <- c(
    "band_5pct"  = "#228833",
    "band_20pct" = "#CCBB44"
)

error_band_labels <- c(
    "band_5pct"  = "±5%",
    "band_20pct" = "±20%"
)

scale_fill_error_bands <- function(...) {
    scale_fill_manual(
        name = "Error",
        values = error_band_colors,
        labels = error_band_labels,
        ...
    )
}

# Solute palette
solute_colors <- c(
    "Ca"    = "#EE6677",
    "NO3"   = "#4477AA",
    "GN_Ca" = "#EE6677",
    "GN_NO3_N" = "#4477AA"
)

# Sampling frequency palette
freq_colors <- c(
    "Weekly"    = "#EE6677",
    "Biweekly"  = "#999999",
    "Monthly"   = "#4477AA"
)

# Season palette
season_colors <- c(
    "Fall"   = "#AA3377",
    "Spring" = "#228833",
    "Summer" = "#CCBB44",
    "Winter" = "#4477AA"
)

# Helper to save HESS-formatted figures
ggsave_hess <- function(filename, plot = last_plot(),
                        width = HESS_DOUBLE_COL_CM,
                        height = NULL,
                        dpi = HESS_DPI,
                        device = "png",
                        ...) {
    if (is.null(height)) height <- width * 0.618
    ggsave(
        filename = filename,
        plot = plot,
        width = cm_to_in(width),
        height = cm_to_in(height),
        dpi = dpi,
        device = device,
        ...
    )
}
