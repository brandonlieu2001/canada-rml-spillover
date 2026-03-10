library(sf)
library(dplyr)
library(tigris)
library(geosphere) # for distHaversine()

options(tigris_use_cache = TRUE)

# (1a) Distance from Canadian border, haversine w/ crs = 4326 (archived) ================================================
# 0) Restrict to border states
border_statefp <- c(
  # "02", # AK, omitting Alaska from main analysis
  "53", # WA
  "16", # ID
  "30", # MT
  "38", # ND
  "27", # MN
  "26", # MI
  # "55", # WI
  "36", # NY
  "50", # VT
  "33", # NH
  "23", # ME
  "39", # OH
  "42"  # PA
)

# 1) Download counties and keep only border states; extract internal-point lat/lon
counties <- counties(cb = FALSE, year = 2020, class = "sf") %>%
  filter(STATEFP %in% border_statefp) %>% 
  mutate(
    lat = as.numeric(INTPTLAT),
    lon = as.numeric(INTPTLON)
  )

# Turn internal-point lat/lon into sf POINT geometry (WGS84 lat/lon)
county_pts <- st_as_sf(
  counties,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)

# Explicitly transform to 4326 to avoid mismatch in st_nearest_feature
county_pts <- st_transform(county_pts, 4326)

# 2) Read actual US international boundary line and keep Canada segments
canada_border <- st_read(
  "data/data_distance/tl_2023_us_internationalboundary/tl_2023_us_internationalboundary.shp",
  quiet = TRUE
)

canada_border <- canada_border %>%
  filter(IBTYPE %in% c("C")) %>%
  st_transform(4326)

# 3) For each county internal point, find nearest border segment
nearest_line_idx <- st_nearest_feature(county_pts, canada_border)

# Build the shortest line from each county point to its nearest border segment
nearest_lines <- st_nearest_points(
  county_pts,
  canada_border[nearest_line_idx, ],
  pairwise = TRUE
)

# Extract the border endpoint (the 2nd point of each 2-point LINESTRING)
border_pts <- st_sfc(
  lapply(st_geometry(nearest_lines), function(line) {
    coords <- st_coordinates(line)
    st_point(coords[nrow(coords), 1:2])  # last row = end point (border)
  }),
  crs = st_crs(nearest_lines)
)

# Get numeric lon/lat of that border point
border_coords <- st_coordinates(border_pts)
border_lon <- border_coords[,1]
border_lat <- border_coords[,2]

# 4) Great-circle distance (Haversine) between county internal point and nearest border point
# Calculate distances using distHaversine() from `geosphere`
counties$dist_haversine_m <- mapply(
  function(lon1, lat1, lon2, lat2) {
    distHaversine(c(lon1, lat1), c(lon2, lat2))
  },
  counties$lon,
  counties$lat,
  border_lon,
  border_lat
)

# Set in terms of km, then convert to miles
counties <- counties %>% 
  mutate(dist_haversine_km = dist_haversine_m / 1000) %>% # Convert to km
  mutate(dist_haversine_mi = dist_haversine_km * 0.621371) %>% # Convert to miles
  mutate(dist_haversine_mi_100 = dist_haversine_mi / 100) # Set to per 100 miles
  

# 5) Output file for merging into your panel
output <- counties %>%
  st_drop_geometry() %>%
  transmute(
    geoid = GEOID,
    dist_haversine_mi_100 = dist_haversine_mi_100
  ) %>%
  arrange(dist_haversine_mi_100)

write.csv(output, "data/data_distance/county_to_canada_distance.csv", row.names = FALSE)
# (1b) Distance from Canadian border, EPSG:5070, st_nearest_feature (archived; tigris file includes Great Lakes, causing many OH and PA to have 0 distance) ===== 
# 0) Restrict to border states
border_statefp <- c(
  # "02", # AK, omitting Alaska from main analysis
  "53", # WA
  "16", # ID
  "30", # MT
  "38", # ND
  "27", # MN
  "26", # MI
  # "55", # WI
  "36", # NY
  "50", # VT
  "33", # NH
  "23", # ME
  "39", # OH
  "42"  # PA
)

# 1) Download counties and keep only border states
counties_sf <- counties(cb = FALSE, year = 2020, class = "sf") %>%
  filter(STATEFP %in% border_statefp)

# Make internal-point coordinates numeric (still lon/lat degrees, but stored as columns)
counties_sf <- counties_sf %>%
  mutate(
    lat = as.numeric(INTPTLAT),
    lon = as.numeric(INTPTLON)
  )

# Turn internal-point lon/lat into sf POINT geometry in 4326, then project to 5070
county_pts_5070 <- st_as_sf(
  counties_sf,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
) %>%
  st_transform(5070)

# 2) Read actual US international boundary line, keep Canada segments, project to 5070
canada_border_5070 <- st_read(
  "data/data_distance/tl_2023_us_internationalboundary/tl_2023_us_internationalboundary.shp",
  quiet = TRUE
) %>%
  filter(IBTYPE == "C") %>%
  st_transform(5070)

# 3) For each county internal point, find nearest border segment (in meters)
# nearest border segment index for each county point
nearest_line_idx <- st_nearest_feature(county_pts_5070, canada_border_5070)

# sanity: should be no NA
stopifnot(!anyNA(nearest_line_idx))

# distance in meters to that nearest segment (vector length = n counties)
dist_m <- st_distance(
  county_pts_5070,
  canada_border_5070[nearest_line_idx, ],
  by_element = TRUE
)

dist_m <- as.numeric(dist_m)

# 4) Convert meters -> km -> miles, then per 100 miles
counties_sf_dist <- counties_sf %>%
  mutate(
    dist_to_border_m  = dist_m,
    dist_to_border_km = dist_to_border_m / 1000,
    dist_to_border_mi = dist_to_border_km * 0.621371,
    dist_to_border_mi_100 = dist_to_border_mi / 100,
  )

# 5) Output file for merging into your panel
output <- counties_sf_dist %>%
  st_drop_geometry() %>%
  transmute(
    geoid = GEOID,
    dist_to_border_mi_100 = dist_to_border_mi_100
  ) %>%
  arrange(dist_to_border_mi_100)

write.csv(output, "data/data_distance/county_to_canada_distance.csv", row.names = FALSE)