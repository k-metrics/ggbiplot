# 
#  ggbiplot.r
#  
#  Copyright 2019 Sampo Suzuki
#  Original Source's Copyright 2011 Vincent Q. Vu.
# 
#  This program is free software; you can redistribute it and/or
#  modify it under the terms of the GNU General Public License
#  as published by the Free Software Foundation; either version 2
#  of the License, or (at your option) any later version.
#  
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#  
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
# 

#' Biplot for Principal Components using ggplot2
#'
#' @param pcobj           an object returned by prcomp() or princomp()
#' @param choices         which PCs to plot
#' @param scale           covariance biplot (scale = 1), form biplot (scale = 0). When scale = 1, the inner product between the variables approximates the covariance and the distance between the points approximates the Mahalanobis distance.
#' @param obs.scale       scale factor to apply to observations
#' @param var.scale       scale factor to apply to variables
#' @param pc.biplot       for compatibility with biplot.princomp()
#' @param groups          optional factor variable indicating the groups that the observations belong to. If provided the points will be colored according to groups
#' @param ellipse         draw a normal data ellipse for each group?
#' @param ellipse.prob    size of the ellipse in Normal probability
#' @param labels          optional vector of labels for the observations
#' @param labels.size     size of the text used for the labels
#' @param alpha           alpha transparency value for the points (0 = transparent, 1 = opaque)
#' @param circle          draw a correlation circle? (only applies when prcomp was called with scale = TRUE and when var.scale = 1)
#' @param circle.prob     size of the correlation circle
#' @param var.axes        draw arrows for the variables?
#' @param varname.size    size of the text for variable names
#' @param varname.adjust  adjustment factor the placement of the variable names, >= 1 means farther from the arrow
#' @param varname.abbrev  whether or not to abbreviate the variable names
#'
#' New parameters
#' @param base_family     ggplot2 theme's base font family
#' @param family          ggplot2 text layer's font family for the labels and varname
#' @param id              plot (id = TRUE) id number of the observations
#' @param ...             currentry invaild
#'
#' @return                a ggplot2 plot
#' @export
#' @import                tidyverse
#' @importFrom            ggrepel geom_label_repel
#' @importFrom            grid arrow
#' @importFrom            plyr ddply
#' @importFrom            scales muted
#' @importFrom            stats predict qchisq var
#' @examples
#'   data(wine)
#'   wine.pca <- prcomp(wine, scale. = TRUE)
#'   ggbiplot(wine.pca, obs.scale = 1, var.scale = 1, groups = wine.class, ellipse = TRUE, circle = TRUE)
#'
ggbiplot <- function(pcobj, choices = 1:2, scale = 1, pc.biplot = TRUE, 
                     obs.scale = 1 - scale, var.scale = scale, 
                     groups = NULL, ellipse = FALSE, ellipse.prob = 0.68, 
                     labels = NULL, labels.size = 3, alpha = 1, 
                     var.axes = TRUE, 
                     circle = FALSE, circle.prob = 0.69, 
                     varname.size = 3, varname.adjust = 1.5, 
                     varname.abbrev = FALSE,
                     base_family = NA, family = NA, id = FALSE, ...)
{

  stopifnot(length(choices) == 2)

  # Recover the SVD
 if(inherits(pcobj, 'prcomp')){
    nobs.factor <- sqrt(nrow(pcobj$x) - 1)
    d <- pcobj$sdev
    u <- sweep(pcobj$x, 2, 1 / (d * nobs.factor), FUN = '*')
    v <- pcobj$rotation
  } else if(inherits(pcobj, 'princomp')) {
    nobs.factor <- sqrt(pcobj$n.obs)
    d <- pcobj$sdev
    u <- sweep(pcobj$scores, 2, 1 / (d * nobs.factor), FUN = '*')
    v <- pcobj$loadings
  } else if(inherits(pcobj, 'PCA')) {
    nobs.factor <- sqrt(nrow(pcobj$call$X))
    d <- unlist(sqrt(pcobj$eig)[1])
    u <- sweep(pcobj$ind$coord, 2, 1 / (d * nobs.factor), FUN = '*')
    v <- sweep(pcobj$var$coord, 2, 
               sqrt(pcobj$eig[1:ncol(pcobj$var$coord), 1]), FUN = "/")
  } else if(inherits(pcobj, "lda")) {
    nobs.factor <- sqrt(pcobj$N)
    d <- pcobj$svd
    u <- predict(pcobj)$x/nobs.factor
    v <- pcobj$scaling
    d.total <- sum(d^2)
  } else {
    stop('Expected a object of class prcomp, princomp, PCA, or lda')
  }

  # Scores
  choices <- pmin(choices, ncol(u))
  df.u <- as.data.frame(sweep(u[,choices], 2, d[choices]^obs.scale, FUN='*'))

  # Directions
  v <- sweep(v, 2, d^var.scale, FUN='*')
  df.v <- as.data.frame(v[, choices])

  names(df.u) <- c('xvar', 'yvar')
  names(df.v) <- names(df.u)

  if(pc.biplot) {
    df.u <- df.u * nobs.factor
  }

  # Scale the radius of the correlation circle so that it corresponds to 
  # a data ellipse for the standardized PC scores
  r <- sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(df.u^2))^(1/4)

  # Scale directions
  v.scale <- rowSums(v^2)
  df.v <- r * df.v / sqrt(max(v.scale))

  # Change the labels for the axes
  if(obs.scale == 0) {
    u.axis.labs <- paste('standardized PC', choices, sep='')
  } else {
    u.axis.labs <- paste('PC', choices, sep='')
  }

  # Append the proportion of explained variance to the axis labels
  u.axis.labs <- paste(u.axis.labs, 
                       sprintf('(%0.1f%% explained var.)', 
                               100 * pcobj$sdev[choices]^2/sum(pcobj$sdev^2)))

  # Score Labels
  if(!is.null(labels)) {
    df.u$labels <- labels
  }

  # Grouping variable
  if(!is.null(groups)) {
    df.u$groups <- groups
  }

  # Variable Names
  if(varname.abbrev) {
    df.v$varname <- abbreviate(rownames(v))
  } else {
    df.v$varname <- rownames(v)
  }

  # Variables for text label placement
  df.v$angle <- with(df.v, (180/pi) * atan(yvar / xvar))
  df.v$hjust = with(df.v, (1 - varname.adjust * sign(xvar)) / 2)
  
  # 識別番号（ID）を付与する
  df.u <- df.u %>% 
    rowid_to_column("id")

  ############################################################################
  # 描画処理はこれ以降
  ############################################################################

  # Base plot
  g <- ggplot(data = df.u, aes(x = xvar, y = yvar)) + 
    xlab(u.axis.labs[1]) + ylab(u.axis.labs[2]) +
    coord_equal() + theme(text = element_text(family = base_family))

  # 変量の矢印（ベクトル）を描く場合
  if(var.axes) {
    # Draw circle
    if(circle) {
      theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
      circle <- data.frame(xvar = r * cos(theta), yvar = r * sin(theta))
      g <- g + geom_path(data = circle, color = scales::muted('white'),
                         size = 1 / 2, alpha = 1 / 3)
    }

    # 変量のベクトルを描く（変量名は最後に描く）
    # Draw directions
    g <- g + geom_segment(data = df.v,
                          aes(x = 0, y = 0, xend = xvar, yend = yvar),
                          arrow = grid::arrow(length = unit(1/2, 'picas')), 
                          color = scales::muted('red'))
  }  # End of if(var.axis)

  # alphaチャネルが指定された場合は、文字の色を点の色調を合わせるために
  # 0.15～0.2程オフセットさせる（0.8以上はオフセットさせない）
  if(alpha > 0.8){
    offset = 1 - alpha
  } else {
    offset = 0.15
  }
  
  # データ用ラベルまたは点を描くになっているので、点とラベルを描くに変更
  # Draw either labels or points

  if(!is.null(df.u$labels)) {
    if(!is.null(df.u$groups)) {
      g <- g + geom_point(aes(color = groups), alpha = alpha) +
        ggrepel::geom_text_repel(aes(label = labels, color = groups), 
                                 alpha = alpha + offset, size = labels.size,
                                 family = family)
    } else {
      g <- g + geom_point(alpha = alpha) + 
        ggrepel::geom_text_repel(aes(label = labels),
                                 alpha = alpha + offset, size = labels.size,
                                 family = family)
    }
  # ラベルデータが指定されていない場合は点やIDを描く
  } else {
    if(!is.null(df.u$groups)) {
      if ( id == TRUE ) {
        g <- g + geom_point(aes(color = groups), alpha = alpha) +
          ggrepel::geom_text_repel(aes(label = id, color = groups),
                                   alpha = alpha + offset, size = labels.size,
                                   family = family)
      } else {
        g <- g + geom_point(aes(color = groups), alpha = alpha)
      }
    } else {
      if ( id == TRUE ) {
        g <- g + geom_point(alpha = alpha) +
          ggrepel::geom_text_repel(aes(label = id),
                                   alpha = alpha + offset, size = labels.size,
                                   family = family)
      } else {
        g <- g + geom_point(alpha = alpha)
      }
    }
  }

  # Overlay a concentration ellipse if there are groups
  if(!is.null(df.u$groups) && ellipse) {
    theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
    circle <- cbind(cos(theta), sin(theta))

    ell <- plyr::ddply(df.u, 'groups', function(x) {
      if(nrow(x) <= 2) {
        return(NULL)
      }
      sigma <- var(cbind(x$xvar, x$yvar))
      mu <- c(mean(x$xvar), mean(x$yvar))
      ed <- sqrt(qchisq(ellipse.prob, df = 2))
      data.frame(sweep(circle %*% chol(sigma) * ed, 2, mu, FUN = '+'), 
                 groups = x$groups[1])
    })
    names(ell)[1:2] <- c('xvar', 'yvar')
    g <- g + geom_path(data = ell, aes(color = groups, group = groups))
  }

  # Label（矢印の先に描かれる変量名） the variable axes
  if(var.axes) {
    g <- g + geom_text(data = df.v, 
                       aes(label = varname, x = xvar, y = yvar, 
                           angle = angle, hjust = hjust), 
                       color = 'darkred', size = varname.size,
                       family = family)
  }

  return(g)
}
