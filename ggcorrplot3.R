ggcorrplot3 <-
  function(
    data,
    data.y = NULL,
    select.x = NULL,
    select.y = NULL,
    stat.method = c("pearson", "spearman", "kendall"),
    fig.method = c("circle", "square", "number", "ellipse", "heatmap"),
    col = NULL,
    sig.lvl = c(0.05, 0.01, 0.001), 
    number.digits = 2,
    grid.color = "grey93",
    show.signif = FALSE,
    signif.color = "black",
    signif.size = 5,
    signif.shape = c("*"),
    signif.x.pos = 0,
    signif.y.pos = 0,
    show.insignif = FALSE,
    insignif.color = "black",
    insignif.size = 5,
    insignif.shape = 4,
    corr.na = TRUE,
    corr.na.label = "NA",
    corr.na.size = 4,
    corr.na.color = "grey65",
    blank.insignif = FALSE,
    blank.insignif.transparent = 60,
    show.coef = FALSE,
    coef.color = "black",
    coef.size = 6,
    coef.transparent = 60,
    coef.x.pos = 0,
    coef.y.pos = 0,
    colorbar.limit = c(-1, 1),
    colorbar.width = 1.5,
    colorbar.height = 15,
    colorbar.position = c("right", "bottom", "left", "top", "none"),
    x.axis.label.position = c("top", "bottom"),
    both.x.axis.label = FALSE,
    y.axis.label.position = c("left", "right"),
    both.y.axis.label = FALSE,
    ellipse.length = 250){
    
  
    # check tidyverse r packages
    
    if (!require("tidyverse"))  install.packages("tidyverse")  
    if (!require("dplyr"))      install.packages("dplyr")
    if (!require("ggplot2"))    install.packages("ggplot2")
    if (!require("tidyr"))      install.packages("tidyr")
    if (!require("ggforce"))    install.packages("ggforce")
    
    # load tidyverse R package
    
    library(tidyverse)
    
    # check correlation r package
    
    if (!require("correlation")) install.packages("correlation", repos = "https://easystats.r-universe.dev")
      
    # load easystats correlation R package
    
    library(correlation)
    
    # check input dataset
    
    if (is.null(data.y) && is.null(select.x) && is.null(select.y)) {
      stop("Need 2 datasets or 1 dataset and 2 select group")
    }
    
    
    if (is.null(data.y) && !is.null(select.x) && !is.null(select.y)) {
      data.y <-  data %>% select(all_of(select.y))
      data   <-  data %>% select(all_of(select.x))
      
    }
    
    # match arguments  
    
    stat.method <- match.arg(stat.method)
    figt.method <- match.arg(fig.method)
  
  # variable names and lengths
    
    x_vars    <-  colnames(data)
    n_x_vars  <-  length(x_vars)
    y_vars    <-  colnames(data.y)
    n_y_vars  <-  length(y_vars)
    
  # correlation
    
    corr <-
      correlation(
        data    = data.y,
        data2   = data,
        method  = stat.method,
      )
    
    cor_res <- 
      corr %>%
      data.frame()

    # code for significance symbol
    
    sig_codes <-
      sapply(seq_along(sig.lvl), function(i) {
      paste(rep(signif.shape, i), collapse = "")
      })

    # add some data in main result table
    
    cor_res <-
      cor_res %>%
      rename(Coef = 3) %>%
      mutate(
        Rid = as.integer(as.factor(Parameter1)),
        Cid = as.integer(as.factor(Parameter2))
      ) %>%
      mutate(Coef_abs = abs(Coef)) %>%
      mutate(
        Coef_label = case_when(
          Coef == 1 ~ Coef,
          TRUE ~ round(Coef, digits = number.digits)
        )
      ) %>%
      mutate(Signif = as.numeric(p <= max(sig.lvl))) %>%
      mutate(Sig_codes = cut(
        p,
        breaks = c(sig.lvl, 0, 1),
        labels = c(rev(sig_codes), ""),
        include.lowest = T)
      ) #%>%
   #   mutate(Blank = Coef * Signif)    
    
    # Color
    
    if(is.null(col)) col <- rev(RColorBrewer::brewer.pal(n = 11, name = "RdBu"))
    
    col2 <- grDevices::colorRampPalette(col)
    
    # table plot
    
    p <-
      cor_res %>%
      ggplot() +
      geom_rect(
        mapping = aes(
          xmin = Cid - 0.5,
          xmax = Cid + 0.5,
          ymin = Rid - 0.5,
          ymax = Rid + 0.5),
        color = grid.color,
        fill = NA) +
      coord_fixed() +
      theme_bw() +
      theme(
        legend.margin = margin(0, unit = 'cm'),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
    
    # plot based on figure method
    
    if (fig.method == "circle") {
      if (blank.insignif) {
        p <- 
          p +
          ggforce::geom_circle(
            data = cor_res[cor_res$Signif == 1, ],
            mapping = aes(
              x0 = Cid,
              y0 = Rid,
              r = Coef_abs/2 - 0.02,
              fill = Coef),
            color = NA
            ) +
          ggforce::geom_circle(
            data = cor_res[cor_res$Signif == 0, ],
            mapping = aes(
              x0 = Cid,
              y0 = Rid,
              r = Coef_abs/2 - 0.02,
              fill = Coef),
            color = NA,
            alpha = blank.insignif.transparent / 100
            ) 
          
      } else {
        p <- 
          p +
          ggforce::geom_circle(
            data = cor_res,
            mapping = aes(
              x0 = Cid,
              y0 = Rid,
              r = Coef_abs/2 - 0.02,
              fill = Coef),
            color = NA)
        }
    } else if (fig.method == "square") {
      if (blank.insignif) {
        p <- 
          p +
          geom_rect(
            data = cor_res[cor_res$Signif == 1, ], 
            mapping = aes(
              xmin = Cid - 0.5 * (Coef_abs - 0.04),
              xmax = Cid + 0.5 * (Coef_abs - 0.04),
              ymin = Rid - 0.5 * (Coef_abs - 0.04),
              ymax = Rid + 0.5 * (Coef_abs - 0.04),
              fill = Coef)
            ) +
          geom_rect(
            data = cor_res[cor_res$Signif == 0, ], 
            mapping = aes(
              xmin = Cid - 0.5 * (Coef_abs - 0.04),
              xmax = Cid + 0.5 * (Coef_abs - 0.04),
              ymin = Rid - 0.5 * (Coef_abs - 0.04),
              ymax = Rid + 0.5 * (Coef_abs - 0.04),
              fill = Coef),
              alpha = blank.insignif.transparent / 100
          )
        
      } else {
        p <- 
          p +
          geom_rect(
            data = cor_res, 
            mapping = aes(
              xmin = Cid - 0.5 * (Coef_abs - 0.04),
              xmax = Cid + 0.5 * (Coef_abs - 0.04),
              ymin = Rid - 0.5 * (Coef_abs - 0.04),
              ymax = Rid + 0.5 * (Coef_abs - 0.04),
              fill = Coef)
          )
        }
        
    } else if (fig.method == "number") {
      if (blank.insignif) {
        p <- 
          p +
          geom_text(
            data = cor_res[cor_res$Signif == 1, ],
            mapping = aes(
              x = Cid,
              y = Rid,
              colour = Coef,
              label = Coef_label)
          ) +
          geom_text(
            data = cor_res[cor_res$Signif == 0, ],
            mapping = aes(
              x = Cid,
              y = Rid,
              colour = Coef,
              label = Coef_label),
            alpha = blank.insignif.transparent / 100
          )
        
      } else {
        p <- 
          p +
          geom_text(
            data = cor_res,
            mapping = aes(
              x = Cid,
              y = Rid,
              colour = Coef,
              label = Coef_label)
          )
      }
      
    } else if (fig.method == "ellipse") {
      
      # ellipse function
      
      ellipse_xy <-
        function(Coef) {
          theta <- seq(0, 2 * pi, length = ellipse.length)
          if (Coef == 1) Coef <- Coef - 1e-4
          d <- acos(-Coef)
          X <- cos(theta + d / 2) / 2
          Y <- cos(theta - d / 2) / 2
          as.data.frame(cbind(X, Y))
        }
      
      # function for correlation result table
      
      my_ellips_fun <-
        function(df) {
          res <-
            ellipse_xy(df$Coef) %>%
            mutate(
              Coef = df$Coef,
              Rid = df$Rid,
              Cid = df$Cid,
              Signif = df$Signif,
              Blank = df$Blank
            ) %>%
            mutate(
              X1 = 0.9 * X + Cid,
              Y1 = 0.9 * Y + Rid,
              Group = paste(Rid, Cid, sep = "-")
            )
        }
      
      # transformed correlation result table
      
      ellipse_dat <-
        cor_res %>%
        group_by(Parameter1, Parameter2) %>%
        group_modify(~my_ellips_fun(.)) %>%
        ungroup()
      
      # ellipse plot
      
      if (blank.insignif) {
        p <-
          p +
          geom_polygon(
            data = ellipse_dat[ellipse_dat$Signif == 1, ],
            mapping = aes(
              x = X1,
              y = Y1,
              fill = Coef,
              group = Group),
            color = NA) +
          geom_polygon(
            data = ellipse_dat[ellipse_dat$Signif == 0, ],
            mapping = aes(
              x = X1,
              y = Y1,
              fill = Coef,
              group = Group),
            color = NA, 
            alpha = blank.insignif.transparent / 100
            )
        
      } else {
        p <-
          p +
          geom_polygon(
            data = ellipse_dat,
            mapping = aes(
              x = X1,
              y = Y1,
              fill = Coef,
              group = Group),
            color = NA)
      }
    } else if (fig.method == "heatmap") {
      if (blank.insignif) {
        p <- 
          p +
          geom_tile(
            data = cor_res[cor_res$Signif == 1, ], 
            mapping = aes(
              x = Cid,
              y = Rid,
              fill = Coef)
          ) +
          
          geom_tile(
            data = cor_res[cor_res$Signif == 0, ], 
            mapping = aes(
              x = Cid,
              y = Rid,
              fill = Coef),
            alpha = blank.insignif.transparent / 100
          ) 
      } else {
        p <- 
          p +
          geom_tile(
            data = cor_res, 
            mapping = aes(
              x = Cid,
              y = Rid,
              fill = Coef)
          ) 
        }
      }  
    
    # indicate NA correlation coefficient
    
    if (corr.na) {
      if (any(is.na(cor_res$Coef))) {
        p <-
          p +
          geom_text(
            data = cor_res[is.na(cor_res$Coef), ],
            aes(
              x = Cid,
              y = Rid),
            label = corr.na.label,
            size = corr.na.size,
            color = corr.na.color)
        }
      }
    
    # show significance marks
    
    if(show.signif) {
      p <-
        p +
        geom_text(
          data = cor_res,
          mapping = aes(
            x = Cid + signif.x.pos,
            y = Rid - signif.y.pos,
            label = Sig_codes),
          color = signif.color,
          size = signif.size
        )
      
    }
    
    # show insignificance marks
    
    if (show.insignif) {
      if (is.character(insignif.shape)) {
        p <-
          p +
          geom_text(
            data = cor_res[cor_res$Signif == 0, ],
            mapping = aes(
              x = Cid,
              y = Rid),
            label = insignif.shape,
            color = insignif.color,
            size  = insignif.size
          )
      } else if (is.numeric(insignif.shape)) {
        p <-
          p +
          geom_point(
            data = cor_res[cor_res$Signif == 0, ],
            mapping = aes(
              x = Cid,
              y = Rid),
            shape = insignif.shape,
            color = insignif.color,
            size  = insignif.size
          )   
        }
      }
   
    # show coefficients
    
    if (show.coef) {
      if (fig.method != "number") {
        if (blank.insignif) {
          p <- 
            p +
            geom_text(
              data = cor_res[cor_res$Signif == 1, ],
              mapping = aes(
                x = Cid + coef.x.pos,
                y = Rid + coef.y.pos,
                label = Coef_label),
              color = coef.color
            ) +
            geom_text(
              data = cor_res[cor_res$Signif == 0, ],
              mapping = aes(
                x = Cid + coef.x.pos,
                y = Rid + coef.y.pos,
                label = Coef_label),
              color = coef.color,
              alpha = coef.transparent / 100
            )
        } else {
          p <- 
            p +
            geom_text(
              data = cor_res,
              mapping = aes(
                x = Cid + coef.x.pos,
                y = Rid + coef.y.pos,
                label = Coef_label),
              color = coef.color
            )
        }
        
      }
    }
    
    # colorbar
    if (fig.method == "number") {
    p <- 
      p +
      scale_colour_gradientn(
        colours = col2(200),
        limits = colorbar.limit,
        guide = guide_colorbar(
          title = "",
          nbin = 1000,
          ticks.colour = "black",
          frame.colour = "black",
          barwidth = colorbar.width,
          barheight = colorbar.height)
      ) +
      theme(legend.position = colorbar.position) 
    } else {
    p <- 
      p +
      scale_fill_gradientn(
        colours = col2(200),
        limits = colorbar.limit,
        guide = guide_colorbar(
          title = "",
          nbin = 1000,
          ticks.colour = "black",
          frame.colour = "black",
          barwidth = colorbar.width,
          barheight = colorbar.height)
      ) +
      theme(legend.position = colorbar.position)
    }
    
    # x axis labels
    
    if (both.x.axis.label) {
      p <- 
        p +
        scale_x_continuous(
          breaks = 1:n_x_vars,
          labels = x_vars,
          expand = c(0, 0),
          position = x.axis.label.position, 
          sec.axis = dup_axis()
        )
      } else {
      p <- 
        p +
        scale_x_continuous(
          breaks = 1:n_x_vars,
          labels = x_vars,
          expand = c(0, 0),
          position = x.axis.label.position
        )
      }
      
    # y axis labels
    
    if (both.y.axis.label) {
      p <- 
        p +
        scale_y_reverse(
          breaks = 1:n_y_vars,
          labels = y_vars,
          expand = c(0, 0),
          position = y.axis.label.position, 
          sec.axis = dup_axis()
        )
      } else {
      p <- 
        p +
        scale_y_reverse(
          breaks = 1:n_y_vars,
          labels = y_vars,
          expand = c(0, 0),
          position = y.axis.label.position 
          )
      }
         
   # add correlation results as nested list 
      
   p$correlation <- corr
    
   return(p)
    
  }
