# ----------------------------------------------- #
# r4mesh
#    標準地域メッシュコードを持つcsvフォーマットのテーブルを
#    シェープファイルへ変換する.
#    プログラムの設定は, parameter.xlsxに記載する.
# ----------------------------------------------- #
rm(list=ls(all=TRUE))
source("./L/libs.r", encoding="UTF-8")
source("./L/MESH_MODULE.r", encoding="UTF-8")
source("./L/meshcode2geometry.r", encoding="UTF-8")

main <- function () {
    PARAMETERS <-
        read_excel ("./parameter.xlsx", sheet="p") %>%
        dplyr::filter (flag==1)
    if (nrow(PARAMETERS) == 0) {
        cat(sprintf("変換対象がないようです"))
        return ()}
    for (prm_i in 1:nrow(PARAMETERS)) {
        # パラメータの読み込み
        ip <- PARAMETERS$ipath[prm_i]
        op <- PARAMETERS$opath[prm_i]
        ml <- PARAMETERS$mesh_level[prm_i]
        mc <- PARAMETERS$mesh_column[prm_i]

        # インプットファイルの確認
        if (!file.exists(ip)) {
            cat (sprintf("%s が見つかりません.\n", ip))
            next}

        # アウトプットファイルの確認
        if (file.exists(op)) {
            cat (sprintf("%s は既に存在しています.\n", op))
            next}

        # ターゲットテーブルの確認
        target_tbl <- read_csv (ip)
        if (nrow(target_tbl) == 0) {
            cat (sprintf("%sのレコード数がゼロです.", ip))
            next}
        if (all(colnames(target_tbl) != mc)) {
            cat (sprintf("%sにカラム%sがありません.", ip, mc))
            next}

        # メッシュコードの変換と, シェープファイルへの出力
        try({
            target_tbl[[mc]] <- format(target_tbl[[mc]], scientific=FALSE)
            gems             <- meshcode2geometry (target_tbl[[mc]], ml)
            st_geometry(target_tbl) <- gems
            st_write(target_tbl, op, driver='ESRI Shapefile', layer_options="ENCODING=UTF-8")
        })}

    cat ("\n変換が終わりました.\n")
}

main()
