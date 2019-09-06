#' Bits and pieces copied from ggplot2 sources and
#' https://github.com/wjschne/ggnormalviolin
#'
#' StatStudent
#'
#' @keywords internal
#' @usage NULL
#' @export
StatStudent <- ggplot2::ggproto(
  "StatStudent",
  ggplot2::Stat,
  required_aes = c("x", "mean", "se", "df", "level"),
  
  setup_params = function(data, params) {
    
    params$scale <- match.arg(params$scale, c("area", "width"))
    params$type <- match.arg(params$type, c("density", "box"))
    
    if (params$scale == "area"){
      params$maxwidth <- max(dt(0, data$df) / data$se)
    } else {
      params$maxwidth <- 1
    }
    params
  },
  setup_data = function(data, params) {
    if (is.null(data$width)) {
      data$width <- params$width
    }
    data$maxwidth <- params$maxwidth
    data$type <- params$type
    
    if (!is.factor(data$level)) {
      stop("'level' aes should be a factor .")
    }
    if (!identical(as.numeric(levels(data$level)),
                   sort(as.numeric(levels(data$level)), decreasing = TRUE))) {
      stop("Levels of 'level' aes should be decreasing, e.g. (0.99, 0.95, 0.9).")
    }
    data
  },
  compute_group = function(self, data, scales, width, maxwidth, scale, type) {
    
    tdist <- function(data) {
      level <- as.numeric(levels(data$level))[data$level] 
      # compute y values (x in terms of pdf)
      limit <- qt((1 + level) / 2, data$df)
      y <- data$mean + data$se * c(seq(-limit, limit, 0.01),
                                   seq(limit, -limit, -0.01))
      # mirror
      side <- c(rep(c(1, -1), each = length(y) / 2))
      
      # compute x values (y in terms of pdf)
      xwidth <- side * data$width
      if (data$type == "box") {
        x <- xwidth * dt(0, data$df) / (data$maxwidth * data$se) + data$x
      } else {
        x <- xwidth * dt((y - data$mean) / data$se, data$df) / (data$maxwidth * data$se) + data$x
      }
      
      # Make data.frame
      data.frame(
        x = x,
        y = y,
        group = data$group,
        mean = data$mean,
        se = data$se,
        df = data$df,
        dx = data$x)
    }
    data %>% group_by(level) %>% do(tdist(.)) %>% ungroup()
  }
)

#' GeomStudent
#'
#' @keywords internal
#' @usage NULL
#' @export
GeomStudent <- ggplot2::ggproto(
  `_class` = "GeomStudent",
  `_inherit` = ggplot2::Geom,
  required_aes = c("x", "mean", "se", "df", "level"),
  default_aes = ggplot2::aes(
    shape = 19,
    colour = NA,
    fill = "grey70",
    size = 0.5,
    linetype = 1,
    alpha = 1,
    stroke = 0.5
  ),
  draw_key = ggplot2::draw_key_polygon,
  draw_panel = function(data, panel_scales, coord, draw_lines, 
                        draw_mean, overflow = 0.25) {
    
    # Parameters for interval
    d_param <- data %>% 
      dplyr::group_by(group, level) %>%
      dplyr::summarise_all(.funs = list(dplyr::first)) %>%
      dplyr::ungroup()
    
    
    # Transform to grid coordinates
    d_points <- coord$transform(data, panel_scales)
    #d_points$level <- factor(d_points$level, levels = factor_orders)
    
    g1 <- grid::polygonGrob(
      default.units = "native",
      x = d_points$x,
      y = d_points$y,
      id = d_points$group,
      gp = grid::gpar(col = d_param$colour,
                      fill = scales::alpha(d_param$fill,
                                           d_param$alpha),
                      lty = d_param$linetype,
                      lwd = d_param$size* .pt)
    )
    if (draw_mean || draw_lines) {
      if (is.null(draw_lines)) {
        levs <- data$level[1]
      } else {
        levs <- factor(draw_lines)
      }
      
      ldf <- left_join(
        d_points %>%
          filter(level %in% levs) %>% 
          group_by(dx, level) %>%
          summarise(
            ylwr = min(y),
            yupr = max(y),
            ymean = mean(y),
            xleft = min(x[y==max(y[x < mean(x)])]),
            xright = max(x[y==max(y[x > mean(x)])]),
            mxleft = min(x),
            mxright = max(x),
            xcenter = mean(x)),
        d_param %>%
          filter(level %in% levs) %>% 
          group_by(dx, level) %>%
          summarise_all(list(first)),
        by = c("dx", "level"))
      
      ldf$mxleft <- ldf$mxleft - overflow * (ldf$xright[1] - ldf$xcenter[1])
      ldf$mxright <- ldf$mxright + overflow * (ldf$xright[1] - ldf$xcenter[1])
      ldf$xleft <- ldf$xleft - overflow * (ldf$xright[1] - ldf$xcenter[1])
      ldf$xright <- ldf$xright + overflow * (ldf$xright[1] - ldf$xcenter[1])
      
    }
    
    if (draw_mean) {
      g2 <- grid::segmentsGrob(
        x0 = ldf$mxleft,
        y0 = ldf$ymean,
        x1 = ldf$mxright,
        y1 = ldf$ymean,
        default.units = "native",
        name = "mean",
        gp = grid::gpar(col = "black",
                        lty = d_param$linetype,
                        lwd = d_param$size * .pt)
      )
    } else g2 <- NULL
    if (!is.null(draw_lines)) {
      g3 <- grid::segmentsGrob(
        x0 = rep(ldf$xleft, 2),
        y0 = c(ldf$yupr, ldf$ylwr),
        x1 = rep(ldf$xright, 2),
        y1 = c(ldf$yupr, ldf$ylwr),
        default.units = "native",
        name = "lines",
        gp = grid::gpar(col = "black",
                        lty = d_param$linetype,
                        lwd = d_param$size * .pt)
      )
    } else g3 <- NULL
    grid::grobTree(g1, g2, g3)
  }
)


#' Student plot
#'
#' A Student plot is a mirrored density plot similar to violin plot
#' but instead of kernel density estimate it is based on the $t$-density.
#' It can be though of as a continuous "confidence interval density", which 
#' could reduce the dichotomous interpretations due to a fixed confidence level.
#'
#' @eval rd_aesthetics("geom", "student")
#' @inheritParams layer
#' @param draw_lines If `not(NULL)` (default), draw horizontal lines
#'   at the given quantiles of the density estimate.
#' @param draw_mean If `TRUE` (default), draw horizontal line at mean.
#' @export
#' @examples
#' ci_levels <- c(0.999, 0.95, 0.9, 0.8, 0.5)
#' n <- length(ci_levels)
#' ci_levels <- factor(ci_levels, levels = ci_levels)
#' PlantGrowth %>% dplyr::group_by(group) %>% 
#'   dplyr::summarise(
#'     mean = mean(weight), 
#'     df = dplyr::n() - 1, 
#'     se = sd(weight)/sqrt(df + 1)) %>%
#'  dplyr::full_join( 
#'    data.frame(group = 
#'      rep(levels(PlantGrowth$group), each = n),
#'      level = ci_levels), by = "group") -> d
#' 
#' p <- ggplot(data = d, aes(group)) + 
#'  geom_student(aes(mean = mean, se = se, df = df, 
#'    level = level, fill = level), draw_lines = c(0.95, 0.5))
#' p
#' g <- scales::seq_gradient_pal("#e5f5f9", "#2ca25f")
#' p + scale_fill_manual(values=g(seq(0,1,length = n))) + theme_bw()
#' rci <- rev(scales::rescale(as.numeric(as.character(ci_levels))))
#' p + scale_fill_manual(values = g(rci)) + theme_bw()
#'  
geom_student <- function(
  mapping = NULL,
  data = NULL,
  position = "identity",
  width = 0.25,
  type = "density",
  scale = "area",
  draw_lines = NULL,
  draw_mean = TRUE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
) {
  
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatStudent,
    geom = GeomStudent,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      type = type,
      scale = scale,
      width = width,
      draw_lines = draw_lines,
      draw_mean = draw_mean,
      ...
    )
  )
}