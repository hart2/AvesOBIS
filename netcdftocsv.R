library(ncdf4)
library(lubridate)

# load netcdf files
nc_fname <- "C:/Users/savan/OneDrive - University of South Florida/cmems_mod_glo_phy_my_0.083_P1M-m_1679430596101.nc"
nc_ds <- nc_open(nc_fname)

# extract coordinates
dim_lon <- ncvar_get(nc_ds, "longitude")
dim_lat <- ncvar_get(nc_ds, "latitude")
dim_depth <- ncvar_get(nc_ds, "depth")
dim_time <- ncvar_get(nc_ds, "time")

# time conversion
t_units <- nc_ds[["var"]][["thetao"]][["dim"]][[4]][["vals"]]
date <- as.Date(t_units/24, origin="1950-01-01")
date

# coordinate matrix
coords <- as.matrix(expand.grid(dim_lon, dim_lat, dim_depth, date))

# extract variables
thetao <- ncvar_get(nc_ds, "thetao", collapse_degen=FALSE)


# export csv
nc_df <- data.frame(cbind(coords, thetao))
names(nc_df) <- c("lon", "lat", "depth", "time", "thetao")
# head(na.omit(nc_df), 5)  # Display some non-NaN values for a visual check
# csv_fname <- "netcdf_nigripes19932018.csv"
# write.table(nc_df, csv_fname, row.names=FALSE, sep=";")

# OR select lat and lon out of localities
# remove SST values that are NAs
nc_df2 <- na.omit(nc_df)
