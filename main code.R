library(raster)
library(rasterVis)
library(rworldxtra)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(dplyr)
library(patchwork)
library(extrafont)
library(RColorBrewer)
library(stringr)

# ------------------------------------------------------------------------------ 
# 1) Read NC files
# ------------------------------------------------------------------------------ 
nc_folder <- "F:\\30DayMapChallenge\\Day29\\rain"
nc_files  <- list.files(nc_folder, pattern = "\\.nc$", full.names = TRUE)

if (length(nc_files) != 12) stop("Error: must contain 12 nc files.")

# -------------------------------
# (A) sort files by month order
# -------------------------------
month_order <- c(
  "January","February","March","April","May","June",
  "July","August","September","October","November","December"
)

extract_month <- function(x){
  # assume: PREmm-April.nc → April
  str_extract(basename(x), paste(month_order, collapse="|"))
}

months_in_files <- extract_month(nc_files)

# reorder file list
nc_files <- nc_files[match(month_order, months_in_files)]
panel_titles <- paste0("PREmm-", month_order)

# ------------------------------------------------------------------------------ 
# 2) Robinson projection
# ------------------------------------------------------------------------------ 
target_crs_robinson <- "ESRI:54030"

# ------------------------------------------------------------------------------ 
# 3) Basemap
# ------------------------------------------------------------------------------ 
world_countries <- ne_countries(scale = "medium", returnclass = "sf")
world_oceans    <- ne_download(scale = "medium", type = "ocean",
                               category = "physical", returnclass = "sf")
world_lakes     <- ne_download(scale = "medium", type = "lakes",
                               category = "physical", returnclass = "sf")

world_countries_robinson <- st_transform(world_countries, crs = target_crs_robinson)
world_oceans_robinson    <- st_transform(world_oceans,    crs = target_crs_robinson)
world_lakes_robinson     <- st_transform(world_lakes,     crs = target_crs_robinson)

# ------------------------------------------------------------------------------ 
# 4) Compute global min/max for unified color scale (min = 4)
# ------------------------------------------------------------------------------ 
all_values <- c()

for (fi in nc_files) {
  r <- raster(fi)
  r[r < 4] <- NA  # ignore values < 4
  all_values <- c(all_values, values(r))
}

global_min <- 4  # start legend from 4
global_max <- max(all_values, na.rm = TRUE)

# color scale breaks
breaks_9 <- seq(global_min, global_max, length.out = 10)

# ------------------------------------------------------------------------------ 
# 5) Function to create individual precipitation map
# ------------------------------------------------------------------------------ 
make_map <- function(nc_path, panel_title) {
  
  r <- raster(nc_path)
  
  # remove values < 4 mm
  r[r < 4] <- NA
  
  r_proj <- projectRaster(r, crs = target_crs_robinson, method = "ngb")
  
  df <- as.data.frame(r_proj, xy = TRUE, na.rm = TRUE)
  names(df) <- c("x","y","val")
  
  ggplot() +
    geom_raster(data = df, aes(x = x, y = y, fill = val)) +
    geom_sf(data = world_oceans_robinson, fill = "#AFEEEE", color = NA, alpha = 0.3) +
    geom_sf(data = world_lakes_robinson,  fill = "#AFEEEE", color = NA, alpha = 0.7) +
    geom_sf(data = world_countries_robinson, fill = NA, color = "black", linewidth = 0.25) +
    
    scale_fill_distiller(
      palette = "RdBu",
      name = "(mm/day)",
      direction = 1,
      na.value = "transparent",
      limits = c(global_min, global_max),
      breaks = breaks_9,
      labels = sprintf("%.2f", breaks_9)
    ) +
    ggtitle(panel_title) +
    theme_minimal() +
    theme(
      text = element_text(family = "Times New Roman", face = "bold"),
      plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      legend.position = "bottom"
    )
}

# ------------------------------------------------------------------------------ 
# 6) Generate 12 monthly plots
# ------------------------------------------------------------------------------ 
plots <- mapply(make_map, nc_files, panel_titles, SIMPLIFY = FALSE)

# ------------------------------------------------------------------------------ 
# 7) Combine panels (4×3) with ONE shared legend
# ------------------------------------------------------------------------------ 
final_plot <- (
  (plots[[1]] | plots[[2]] | plots[[3]]) /
    (plots[[4]] | plots[[5]] | plots[[6]]) /
    (plots[[7]] | plots[[8]] | plots[[9]]) /
    (plots[[10]] | plots[[11]] | plots[[12]])
) +
  plot_layout(guides = "collect") & 
  theme(
    legend.position = "bottom",
    legend.key.width = unit(2.5, "cm"),
    legend.key.height = unit(0.8, "cm"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold")
  )

# ------------------------------------------------------------------------------ 
# 8) Save final figure
# ------------------------------------------------------------------------------ 
ggsave("Precip_12panel_Robinson.png",
       final_plot, width = 15, height = 10, dpi = 300, bg = "white")

ggsave("Precip_12panel_Robinson.pdf",
       final_plot, width = 15, height = 10, device = cairo_pdf)
