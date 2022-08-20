# Source custom functions ------------------------------------------------------

source(here::here("R/functions/density_grid.R"))
source(here::here("R/functions/density_plot.R"))
source(here::here("R/functions/gridded_plots.R"))

# Modifiable parameters --------------------------------------------------------

# Modifiable params
iteration_id <- "lamination_0059"
initial_seed <- 5787059
grid_width <- 10
grid_height <- 1
plot_bg <- "#131313"
plot_res <- 100

# Gradient colours
base_colours <- rev(c("#C9DBDF", "#93B7BE", "#588B8B", "#2D3047"))

gradient_colours1 <- c(base_colours, "#FFD5C2")
set.seed(initial_seed)
gradient_source1 <- sample(
  gradient_colours1, size = length(gradient_colours1), replace = FALSE)

gradient_colours2 <- c(base_colours, "#F28F3B")
set.seed(initial_seed)
gradient_source2 <- sample(
  gradient_colours2, size = length(gradient_colours2), replace = FALSE)

gradient_colours3 <- c(base_colours, "#C8553D")
set.seed(initial_seed)
gradient_source3 <- sample(
  gradient_colours3, size = length(gradient_colours3), replace = FALSE)

# Derived parameters -----------------------------------------------------------

# Set custom colour gradient
gradient_colours1 <- (grDevices::colorRampPalette(gradient_source1))(10)
gradient_colours2 <- (grDevices::colorRampPalette(gradient_source2))(10)
gradient_colours3 <- (grDevices::colorRampPalette(gradient_source3))(10)

palette_list <- tibble::tibble(
  palette_num = 1:3,
  colours = list(gradient_colours1, gradient_colours2, gradient_colours3))

# Seed vector for multiple dust runs
set.seed(initial_seed)
seed_vec <- sample(seq(1, 1000000, by = 1), 2, replace = FALSE)

# Create data ------------------------------------------------------------------

data <- density_grid(
  seed = initial_seed, n_cols = grid_width, n_rows = grid_height)

# Build plots ------------------------------------------------------------------

# Colourful 2D density plots
plots_in_grid <- gridded_plots(
  seed = initial_seed, data = data, palette_list = palette_list,
  n_cols = grid_width, n_rows = grid_height, resolution = plot_res,
  bg_colour = plot_bg, visible_grid = FALSE)

# Export to file ---------------------------------------------------------------

# Colourful 2D density plots
ggplot2::ggsave(
  here::here(glue::glue("img/ingredients/{`iteration_id`}_base.png")),
  plots_in_grid, width = 1000, height = 100, units = "px",
  dpi = 600, device = ragg::agg_png)

# Shift to {magick} workflow ---------------------------------------------------

# Read in images
img <- magick::image_read(
  here::here(glue::glue("img/ingredients/{`iteration_id`}_base.png")))

img_rescaled <- img |>
  magick::image_scale("2000x2000!") |>
  magick::image_morphology(method = "Dilate", iterations = 1, kernel = "Disk") |>
  magick::image_morphology(method = "Smooth", iterations = 10, kernel = "Disk") |>
  magick::image_noise(noisetype = "gaussian") |>
  magick::image_scale("4500x3000!") |>
  magick::image_noise(noisetype = "gaussian")

# Export
magick::image_write(
  img_rescaled,
  path = here::here(glue::glue("img/{`iteration_id`}.png")),
  format = "png")
