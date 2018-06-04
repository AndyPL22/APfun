SHPextensions <- c("shp", "shx", "dbf", "prj", "sbn", "sbx", "fbn", "fbx", "ain", "aih",
                "ixs", "mxs", "atx", "shp.xml", "cpg", "qix")

# Save data to package
devtools::use_data(SHPextensions)


rasterExtensions <- list(tif = c("tif", "tif.aux.xml", "ovr", "tfw"))

devtools::use_data(rasterExtensions)
