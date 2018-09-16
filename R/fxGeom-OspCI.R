#' @importClassesFrom tectr fxGeom fxGeomContinuous fxGeomContinuousCI
#' @importMethodsFrom tectr fxe_layer_scale

NULL

#' fxGeom_class "OspCI"
#'
#' This class is intended for the osp variables. While not all of these may have
#' confidence intervals, there is no difference between a variable of class
#' "ContinuousCI" without the associated variables and "Continuous". In the name
#' of sparsity, we therefore only define one class.

.fxGeomOspCI <- setClass("fxGeomOspCI", contains = "fxGeomContinuousCI")

#' @describeIn fxGeomOspCI-class This class adds a second axis with the
#' corresponding ordinal scale. This depends on the ord aesthetics in
#' fxGeom_assoc_vars
#'
#' @export

setMethod("fxe_layer_scale",
          signature = c(fx_geom = "fxGeomOspCI", aes_name = "xAesName"),
          function(fx_geom, aes_name, data,
                   fxGeom_assoc_vars = NULL) {
            nxt <- callNextMethod()
            ord <- fxGeom_assoc_vars[["ord"]]
            if(is.null(ord)) return(nxt)
            ord <-
              ord %>%
              rlang::quo_get_expr() %>%
              as.character()
            if(!(ord %in% metaframe(data)$name)) return(nxt)
            mf_ord <-
              mf_ord %>%
              dplyr::filter(name == ord)
            fxGeom_breaks <- mf_ord$fxGeom_breaks[[1]]
            if(is.null(fxGeom_breaks)) fxGeom_breaks = waiver()
            nxt$secondary.axis <-
              sec_axis(
                name = mf_ord$fxInfo_name,
                breaks = fxGeom_breaks
              )
          })
