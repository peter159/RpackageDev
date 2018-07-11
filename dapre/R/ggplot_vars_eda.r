#' Data exploration plotting family based on ggplot
#'
#' A interface for plotting on data exploration
#'
#' This output will be a ggplot style plot
#'
#' @param data dataframe objects
#' @param x string name in data, project as x-axis
#' @param y string name in data, project as y-axis
#' @param facet vector of string as facet in ggplot
#' @param binwidth default 5000
#' @param ggtitle graph title
#' @param xlab x-axis label
#' @param ylab y-axis label
#'
#' @export
#' @rdname ggplot_vars_eda
ggplot_continuous_hist <- function(data,x,facet = NULL,
                                   binwidth = 'auto',
                                   ggtitle = sprintf('Histogram of %s',x),
                                   xlab = sprintf("%s",x),
                                   ylab = sprintf("Count of %s",x)){

  binwidth = match.arg(binwidth)
  if (binwidth == 'auto'){
    var.tmp <- data[,x]
    breaks <- pretty(range(var.tmp),n = nclass.FD(var.tmp),min.n = 1)
    binwidth <- (breaks[2] - breaks[1]) / 2
  }

  p <- ggplot(data, aes_string(x = x, fill = '..count..')) +
      geom_histogram(binwidth = binwidth) +
      ggtitle(ggtitle) +
      ylab(ylab) +
      xlab(xlab) +
      theme(plot.title = element_text(hjust = 0.5))

  if (!is.null(facet)){
    p <- p + facet_wrap(facet,labeller = "label_both")
  }

  return(p)
}

#' @examples
#' ggplot_continuous_hist(data,"ACV_1612",c("tag","CHAIN2"))

#' @export
#' @rdname ggplot_vars_eda
ggplot_factor_barplot <- function(data,x,facet = NULL,
                                  ggtitle = sprintf("Count of %s",x),
                                  ...){
  p <- ggplot(data, aes_string(x = x,fill = x)) +
      geom_bar()+
      scale_fill_hue(c = 80)+
      ggtitle(ggtitle)+
      theme(plot.title = element_text(hjust = 0.5),
            legend.position="right",
            legend.background = element_rect(fill="grey90",
                                             size=0.5,
                                             linetype="solid",
                                             colour ="black"))+
      geom_text(stat='count',aes(label=..count..),vjust=-0.25)

  if (!is.null(facet)){
    p <- p + facet_wrap(facet,labeller = "label_both")
  }

  return(p)
}

#' @export
#' @rdname ggplot_vars_eda
ggplot_factor_continuous_boxplot <- function(data,x,y,
                                             facet = NULL,
                                             ggtitle = sprintf("Boxplot of %s by %s",y,x)){
  p <- ggplot(data, aes_string(x=x, y=y, fill=x)) +
      geom_boxplot(alpha=0.3) +
      stat_summary(fun.y=mean,
                   geom="point",
                   shape=20,
                   size=4,
                   color="red",
                   fill="red") +
      theme(legend.position="none") +
      ggtitle(ggtitle) +
      theme(plot.title = element_text(hjust = 0.5))

  if (!is.null(facet)){
    p <- p + facet_wrap(facet,labeller = "label_both")
  }

  return(p)
}

#' @export
#' @rdname ggplot_vars_eda
ggplot_factor_continuous_hist <- function(data,x,y,
                                          facet = NULL,
                                          binwidth = 'auto',
                                          ggtitle = sprintf("Histogram of %s by %s",y,x)){
  binwidth = match.arg(binwidth)
  if (binwidth == 'auto'){
    var.tmp <- data[,y]
    breaks <- pretty(range(var.tmp),n = nclass.FD(var.tmp),min.n = 1)
    binwidth <- (breaks[2] - breaks[1]) / 2
  }

  p <- ggplot(data, aes_string(x = y,fill = sprintf('as.factor(%s)',x))) +
      geom_histogram(position = "stack", binwidth = binwidth) +
      ggtitle(ggtitle) +
      ylab("Count") +
      xlab(y) +
      scale_fill_discrete(name=x)+
      theme(plot.title = element_text(hjust = 0.5),
            legend.position=c(0.9,0.7),
            legend.background = element_rect(fill="grey90",
                                             size=0.5,
                                             linetype="solid",
                                             colour ="black"))

  if (!is.null(facet)){
    p <- p + facet_wrap(facet,labeller = "label_both")
  }

  return(p)
}

#' @export
#' @rdname ggplot_vars_eda
ggplot_continuous_continuous_scatter <- function(data,x,y,
                                                 facet = NULL,
                                                 ggtitle = sprintf("Scatter plot of %s and %s",x,y)){
  p <- ggplot(data, aes_string(x, y)) +
      geom_point(shape=1,alpha = 0.7) +
      geom_smooth(method=lm , color="red", se=FALSE)+
      ggtitle(ggtitle) +
      theme(plot.title = element_text(hjust = 0.4))

  if (!is.null(facet)){
    p <- p + facet_wrap(facet,labeller = "label_both")
  }

  return(p)
}
