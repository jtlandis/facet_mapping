facet_mapping <- function(aes_map=NULL, scales = "fixed",
                          space = "fixed", shrink = TRUE,
                          labeller = "label_value", as.table = TRUE,
                          drop = TRUE, margins = FALSE, ncol=NULL, nrow=NULL, byrow=TRUE) {
 
  scales <- match.arg(scales, c("fixed", "free_x", "free_y", "free"))
  free <- list(
    x = any(scales %in% c("free_x", "free")),
    y = any(scales %in% c("free_y", "free"))
  )
  
  space <- match.arg(space, c("fixed", "free_x", "free_y", "free"))
  space_free <- list(
    x = any(space %in% c("free_x", "free")),
    y = any(space %in% c("free_y", "free"))
  )
  dim <- make_gridMatrix(length(aes_map))
  ncol <- dim[1]
  nrow <- dim[2]
    
  ggproto(NULL, FacetMapping,
          shrink = shrink,
          params = list(
            aes_map = aes_map,
            free = free, space_free = space_free,
            ncol=ncol, nrow=nrow, byrow=byrow
          ))
}
make_gridMatrix <- function(x){
  ncol <- ceiling(sqrt(x))
  nrow <- ceiling(x/ncol)
  return(c(ncol,nrow))
}
make_mapping <- function(data,PANEL, aes_mapping){
  mapping <- lapply(aes_mapping[[PANEL]], dplyr::as_label)
  if(mapping$x%in%colnames(data)&&mapping$y%in%colnames(data)){
    .x <- data[[mapping$x]]
    .y <- data[[mapping$y]]
    data$x <- .x
    data$y <- .y
  }
  data$PANEL <- PANEL
  data
}

FacetMapping <- ggproto("FacetMapping",
                        Facet,
                        shrink = TRUE,
                        compute_layout = function(data, params) {
                          data.frame(PANEL = 1:(params$ncol*params$nrow), SCALE_X = 1L, SCALE_Y = 1L)
                        },
                        map_data = function(data, layout, params) {
                          if (is.null(data) || nrow(data) == 0) {
                            return(cbind(data, PANEL = integer(0)))
                          }
                          dat <- c()
                          for(i in 1:length(params$aes_map)){
                            if(!is.null(params$aes_map[[i]])){
                              .tmp <- make_mapping(data = data, PANEL = i, aes_mapping = params$aes_map)
                              dat <- rbind(dat,.tmp)
                            }
                          }
                          dat
                          
                        },
                        draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord, data,
                                               theme, params) {
                          browser()
                          n <- length(params$aes_map)
                          ncol <- params$ncol
                          nrow <- params$nrow
                          panels[which(!unlist(lapply(params$aes_map,is.null)))] <- panels
                          #Adjust any NULL passes to the correct Panel Position
                          for(pos in which(unlist(lapply(params$aes_map,is.null)))){
                            panels[[pos]] <- zeroGrob()
                          }
                          if((n)!=ncol*nrow){
                            for(pos in (n+1):(ncol*nrow)) {
                              panels[[pos]] <- zeroGrob()
                            }
                          }
                          # panels <- lapply(panels, function(x){if(is.null(x)){
                          #   return(zeroGrob())
                          # }else{
                          #   return(x)
                          # }})
                          panels <- matrix(panels, ncol = ncol, nrow = nrow, byrow = params$byrow)
                          panel_table <- gtable::gtable_matrix("layout", panels, 
                                                               widths = unit(rep(1, params$ncol), "null"), heights = unit(rep(1,params$nrow), "null"), clip = "on")
                          panel_table$layout$name <- paste0("panel-", 1:(ncol*nrow))
                          panel_table
                        })
