#' multiple ggplot in one interface
#'
#' Try to place multiple ggplot in one interface
#'
#' This output will be ggplot style multiple plot.
#' If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
#' then plot 1 will go in the upper left, 2 will go in the upper right,
#' and 3 will go all the way across the bottom.
#'
#' @param .../plotlist ggplot obj list separated by comma,
#' or bracket use `list()` function
#' @param xvars x axis vars
#' @param yvars x axis vars
#' @param cols number of columns in layout
#' @param layout A matrix specifying the layout. If present, 'cols' is ignored.
#'
#' @export
#' @rdname multiplot
multiplot.auto <- function(data,xvars,yvars = NULL,
                           FUN = ggplot_continuous_hist){
  if (!is.function(FUN)) stop("Parameter FUN has to be a function")

  if (is.null(yvars)){
    plt.df <- expand.grid(x = xvars,stringsAsFactors = FALSE)
  }else{
    plt.df <- expand.grid(x = xvars,y = yvars,stringsAsFactors = FALSE)
  }

  for (i in seq(nrow(plt.df))){
    assign(paste0('p',i),
           do.call(FUN,list(data = data,
                            x = plt.df[i,1],
                            y = plt.df[i,2])))
  }

  plt.n <- nrow(plt.df)
  if (plt.n <= 5){
    cols <- ceiling(plt.n / 3)
  }else{
    cols <- ceiling(plt.n / 5)
  }

  plotlist <- lapply(seq(nrow(plt.df)),function(i) get(paste0('p',i)))
  multiplot.manual(plotlist = plotlist,cols = cols)
}
#' @examples
#' multiplot.auto(data = Glass,x = colnames(Glass)[1: 9],y = colnames(Glass)[9],FUN = ggplot_continuous_continuous_scatter)

#' @export
#' @rdname multiplot
multiplot.manual <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  install_n_require("grid")

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
#' @examples
#' multiplot(p1,p2,p3,layout = matrix(c(1,1,2,3),nrow = 2,byrow = TRUE))
