GeomLineCol <- ggplot2::ggproto(NULL, ggplot2::GeomSegment,
                                required_aes = c("x", "y"),
                                setup_data = function(data, params){
                                  transform(data, xend = x, yend = 0)
                                },
                                draw_panel = function(self, data, panel_params, coord){
                                  ggplot2::ggproto_parent(ggplot2::GeomSegment, self)$
                                    draw_panel(data, panel_params, coord)
                                })

#' @importFrom ggplot2 layer
geom_linecol <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity",
                          ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)
{
  layer(data = data, mapping = mapping, stat = stat, geom = GeomLineCol,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...))
}
