myseq_graph <- function(x){
  if (is.data.frame(x) != "TRUE"){
    stop("x should be data frame")
  }
  else if (ncol(x) != 4){
    stop("x must have 4 values")
  }
  else {
    num <- vector(mode = "double", length = nrow(x))
    for (i in seq_along(num)){
      a <- x[i,]
      num[[i]] <- myseq_n(as.double(c(a[1], a[2], a[3])), n = as.integer(a[4]))
    }
    xlab <- x[,4]
    ylab <- data.frame(num)
    dataset <- cbind(xlab, ylab)
    colnames(dataset) <- c("xlab", "ylab")
    dataset <- data.frame(dataset)
    ggplot2::ggplot(data = dataset, mapping = ggplot2::aes(x = xlab, y = ylab))+
      ggplot2::geom_line()+
      ggplot2::xlab("n")+
      ggplot2::ylab("output")+
      ggplot2::ggtitle("My sequence")
  }
}

