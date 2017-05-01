if (!exists("MESH_MODULE")) {
    stop ("MESH_MODULEを事前に定義してください.\n")}
require(purrr)
require(dplyr)
require(sf)
meshcode2geometry <- function (mesh_code, mesh_level=5) {
    MM <- MESH_MODULE$new()
    bl <- NULL
    if (mesh_level <= 6) {
        bl <- MM$mesh2latlng (mesh_code, mesh_level)}
    else if (mesh_level == 50) {
        bl <- MM$mesh2latlng__50 (mesh_code)}
    else if (mesh_level == 100) {
        bl <- MM$mesh2latlng__100 (mesh_code)}
    else {
        stop("想定外のメッシュレベルです")}
    rectangle_coord <-
        with (bl, map2(lat, log, function(lt, lg, ml) {
            MM$meshsize_rectangle_coord(lt, lg, ml)
        }, ml=mesh_level))
    mesh_geometry   <-
        rectangle_coord %>%
        map(list) %>%
        map(st_multipolygon) %>%
        st_sfc(crs=4612)
    mesh_geometry}

