#' @title Pick climate futures from model changes (NPS PCA method)
#'
#' @description
#' Picks a small set of climate models that span the range of projected
#' change, following the NPS climate futures method. Each model run in `df`
#' has a change in two variables (`x` and `y`, e.g. temperature and
#' precipitation):
#' 1. Only models ranked high skill (rank 18 or better) for `region` in the
#'    NPS GCM skill table are kept. The table is read from GitHub
#'    (rossyndicate/CCRP_automated_climate_futures), so it needs internet.
#' 2. Each model gets a climate future (`CF`) from the means and quartiles of
#'    `x` and `y`: `"Central"` or one of the four corners (e.g. "Low x, High
#'    y").
#' 3. A principal component analysis of `x` and `y` picks the models at the
#'    minimum and maximum of PC1 and PC2. A missing corner is filled with the
#'    model nearest that corner, and a model picked twice is dropped.
#'
#' It prints the means of `y` and `x`.
#'
#' @param df Data frame. One row per model run, with a `GCM` column of
#'   `"<model>.<scenario>"` names as in the skill table (e.g.
#'   `"CCSM4.rcp45"`), an `RCP` column (for the plot), and the `x` and `y`
#'   columns. Required.
#' @param x String. The name of the column for the x axis, e.g. the change
#'   in mean temperature. Required.
#' @param y String. The name of the column for the y axis, e.g. the change
#'   in precipitation. Required.
#' @param x_name String. The label for `x` in the climate future names and
#'   the plot. Default: `x`.
#' @param y_name String. The label for `y`. Default: `y`.
#' @param high_x String or `NULL`. The name for high `x`, e.g. `"Hot"`.
#'   * A string: used as is in the climate future names.
#'   * `NULL`: `"High <x_name>,"`.
#'
#'   Default: `NULL`.
#' @param low_x String or `NULL`. The name for low `x`, e.g. `"Warm"`; `NULL`
#'   gives `"Low <x_name>,"`. Default: `NULL`.
#' @param high_y String or `NULL`. The name for high `y`, e.g. `"Wet"`;
#'   `NULL` gives `"High <y_name>"`. Default: `NULL`.
#' @param low_y String or `NULL`. The name for low `y`, e.g. `"Dry"`; `NULL`
#'   gives `"Low <y_name>"`. Default: `NULL`.
#' @param to_plot Logical. Whether to also return a plot.
#'   * `TRUE`: returns a list of the selected models and a ggplot scatter
#'     plot of every model, with the quartile box, the means and the selected
#'     models circled.
#'   * `FALSE`: returns only the selected models.
#'
#'   Default: `TRUE`.
#' @param park String or `NULL`. The park name for the plot title. Default:
#'   `NULL`.
#' @param fill_var String. The column of `df` that colors the points.
#'   * `"CF"`: the climate future, with set colors.
#'   * Another numeric column: a yellow-to-red gradient.
#'
#'   Default: `"CF"`.
#' @param fill_var_name String. The legend title for `fill_var`. Default:
#'   `"Climate Future"`.
#' @param region String. The NPS region in the skill table whose skill
#'   ranks are used, e.g. `"SWR"`. Default: `"SWR"`.
#' @param highlight Character vector or `NULL`. Climate futures (`CF`
#'   values) whose selected models get a gold circle in the plot; `NULL` for
#'   none. Default: `NULL`.
#'
#' @return With `to_plot = FALSE`, a tibble of the selected models with
#'   `GCM`, `CF`, and the `x` and `y` columns. With `to_plot = TRUE`, a list
#'   of that tibble and the ggplot.
#' @export
#' @seealso [get_climate_future()], which downloads the selected models.
#' @examplesIf interactive()
#' # Reads the GCM skill table from GitHub, so it needs internet
#' models <- c("HadGEM2-ES365", "HadGEM2-CC365", "CNRM-CM5", "CCSM4", "CanESM2",
#'             "MIROC5", "IPSL-CM5A-MR", "NorESM1-M", "bcc-csm1-1-m",
#'             "IPSL-CM5A-LR", "CSIRO-Mk3-6-0", "BNU-ESM", "MRI-CGCM3",
#'             "GFDL-ESM2M", "GFDL-ESM2G", "MIROC-ESM", "inmcm4",
#'             "MIROC-ESM-CHEM", "bcc-csm1-1", "IPSL-CM5B-LR")
#' set.seed(1)
#' deltas <- data.frame(GCM = paste0(rep(models, 2), ".", rep(c("rcp45", "rcp85"), each = 20)),
#'                      RCP = rep(c("rcp45", "rcp85"), each = 20),
#'                      delta_tmean = c(runif(20, 1, 3), runif(20, 2, 5)),
#'                      delta_ppt = runif(40, -15, 15))
#'
#' futures <- select_cfs_deep_dive(deltas, x = "delta_tmean", y = "delta_ppt",
#'                                 x_name = "Temperature", y_name = "Precipitation",
#'                                 high_x = "Hot", low_x = "Warm",
#'                                 high_y = "Wet", low_y = "Dry",
#'                                 park = "ZION")
#' futures[[1]]
#' futures[[2]]

select_cfs_deep_dive <- function(df,
                                 x,
                                 y,
                                 x_name = x,
                                 y_name = y,
                                 high_x = NULL,
                                 low_x = NULL,
                                 high_y = NULL,
                                 low_y = NULL,
                                 to_plot = TRUE,
                                 park = NULL,
                                 fill_var = "CF",
                                 fill_var_name = "Climate Future",
                                 region = "SWR",
                                 highlight = NULL) {
  
  
  # Read in all available gcms
  all_cfs <-
    readr::read_delim( "https://raw.githubusercontent.com/rossyndicate/CCRP_automated_climate_futures/master/data/general/GCM_skill_by_region.txt" ) %>%
    dplyr::filter(Region == region) %>% #,
    #Rank < 19) %>%
    dplyr::mutate(GCM = ifelse(GCM == "HadGEM2-ES.", "HadGEM2-ES365.", GCM),
                  GCM = ifelse(GCM == "HadGEM2-CC.", "HadGEM2-CC365.", GCM)) %>%
    tidyr::crossing(data.table::data.table(RCP = c("rcp45","rcp85"))) %>%
    dplyr::mutate(GCM = paste0(GCM,RCP)) %>% 
    dplyr::mutate(Skill = ifelse(Rank > 18, "Low Skill", "High Skill"))
  
  df_og <- df

    df <- df %>%
    dplyr::filter(GCM %in% all_cfs[all_cfs$Skill == "High Skill",]$GCM)
  
  mean_x <- df[[x]] %>% mean()
  q75_x <- df[[x]] %>% stats::quantile(.75) %>% as.numeric()
  q25_x <- df[[x]] %>% stats::quantile(.25) %>% as.numeric()
  mean_y <- df[[y]] %>% mean()
  q75_y <- df[[y]] %>% stats::quantile(.75) %>% as.numeric()
  q25_y <- df[[y]] %>% stats::quantile(.25) %>% as.numeric()
  
  print(mean_y)
  print(mean_x)
  
  high_x <- ifelse(is.null(high_x), paste0("High ",x_name, ","), high_x)
  low_x <- ifelse(is.null(low_x), paste0("Low ", x_name, ","), low_x)
  high_y <- ifelse(is.null(high_y), paste0("High ", y_name), high_y)
  low_y <- ifelse(is.null(low_y), paste0("Low ", y_name), low_y)  
  
  # CF assignment
  df <- df %>%
    dplyr::ungroup() %>%
    # designate climate future classification based on cf limits
    dplyr::mutate(
      CF = dplyr::case_when(
        !!dplyr::sym(x) < mean_x & !!dplyr::sym(y) > q75_y | !!dplyr::sym(x) < q25_x & !!dplyr::sym(y) > mean_y ~ paste0(low_x, " ", high_y),
        !!dplyr::sym(x) > mean_x & !!dplyr::sym(y) > q75_y | !!dplyr::sym(x) > q75_x & !!dplyr::sym(y) > mean_y ~ paste0(high_x, " ", high_y),
        !!dplyr::sym(x) > q25_x & !!dplyr::sym(x) < q75_x & !!dplyr::sym(y) > q25_y & !!dplyr::sym(y) < q75_y ~ "Central",
        !!dplyr::sym(x) < mean_x & !!dplyr::sym(y) < q25_y | !!dplyr::sym(x) < q25_x & !!dplyr::sym(y) < mean_y ~ paste0(low_x, " ", low_y),
        !!dplyr::sym(x) > mean_x & !!dplyr::sym(y) < q25_y | !!dplyr::sym(x) > q75_x & !!dplyr::sym(y) < mean_y ~ paste0(high_x, " ", low_y)))
  
  
  CFs_all <- c(paste0(low_x, " ", high_y), 
               paste0(high_x, " ", high_y), 
               "Central",
               paste0(low_x, " ", low_y), 
               paste0(high_x, " ", low_y))
  
  
  ## Corners Method
  
  #### Select Corner GCMs, assuming x and y
  lx = min(df[[x]])
  ux = max(df[[x]])
  ly = min(df[[y]])
  uy = max(df[[y]])
  
  #convert to points
  lx_hy = c(lx,uy) # Upper left corner, low x, high y
  lx_ly = c(lx,ly) # Lower left corner, low x, low y
  hx_ly = c(ux,uy) # Lower Right Corner, high x, low y
  hx_hy = c(ux,ly) # Upper Right Corner, high x, high y
  
  corners <- df %>%
    # calculate euclidiean distance of each point/model from each corner
    dplyr::mutate(
      lx_hy_dist = sqrt((!!dplyr::sym(x) - lx_hy[1]) ^ 2 + (!!dplyr::sym(y) - lx_hy[2]) ^ 2),
      lx_ly_dist = sqrt((!!dplyr::sym(x) - lx_ly[1]) ^ 2 + (!!dplyr::sym(y) - lx_ly[2]) ^ 2),
      hx_ly_dist = sqrt((!!dplyr::sym(x) - hx_ly[1]) ^ 2 + (!!dplyr::sym(y) - hx_ly[2]) ^ 2),
      hx_hy_dist =  sqrt((!!dplyr::sym(x) - hx_hy[1]) ^ 2 + (!!dplyr::sym(y) - hx_hy[2]) ^ 2),
      corners = CF
    ) %>%
    dplyr::filter(lx_hy_dist == min(lx_hy_dist) |
                    hx_hy_dist == min(hx_hy_dist) |
                    lx_ly_dist == min(lx_ly_dist) |
                    hx_ly_dist == min(hx_ly_dist)) %>%
    dplyr::select(GCM, corners)
  
  df <- df %>%
    dplyr::left_join(corners, by = "GCM")
  
  
  ## PCA Method
  
  #Using just change in precip and avg temp (Runyon et al. methods), may want to explore other variables for more in-depth analysis at the park level.
  
  # set up for PCA
  future_pca_1 <- df %>% 
    dplyr::select(GCM, {{x}}, {{y}}) %>% 
    # set up for prcomp
    tibble::column_to_rownames(var = 'GCM')
  
  
  pca_1 <- stats::prcomp(future_pca_1, center = TRUE, scale. = TRUE) 
  
  # quick plot
  ggplot2::autoplot(pca_1, data = future_pca_1, loadings = TRUE,label=TRUE)
  
  # get dataframe
  pca_1_df <- as.data.frame(pca_1$x)
  
  
  #Take the min/max of each of the PCs
  PCs <- pca_1_df %>% 
    dplyr::filter(PC1 == min(PC1) |
             PC1 == max(PC1) |
             PC2 == min(PC2) |
             PC2 == max(PC2)) %>% 
    tibble::rownames_to_column(var = "GCM")
  
  
  #Assigns CFs to diagonals
  diagonals <-
    rbind(
      data.frame(CF = CFs_all[c(1, 5)], diagonals = "diagonal1"),
      data.frame(CF = CFs_all[c(4, 2)], diagonals = "diagonal2")
    )
  
  
  PCA <-
    df %>% 
    dplyr::filter(GCM %in% PCs$GCM) %>% 
    dplyr::left_join(diagonals, by = "CF") %>% 
    dplyr::right_join(PCs, by = "GCM")
  
  # create column with selected pca models
  df <- df %>%
    dplyr::mutate(pca = dplyr::if_else(GCM %in% PCs$GCM, CF, NA))
  
  #Handling missing and/or extra quadrats
  
  # function to deal with redundant quadrat
  ID.redundant.gcm <- function(PCA) {
    redundant.diag <- dplyr::count(PCA$diagonals)$x[which(dplyr::count(PCA$diagonals)$freq == 1)] #ID redundant diagonal
    PC.foul = PCA$PC[which(PCA$diagonals == redundant.diag)] #ID which PC has the redundant diagonal
    PCA$GCM[which(PCA$PC == PC.foul &
                    PCA$GCM != PCA$GCM[which(PCA$diagonals == redundant.diag)])] #ID GCM that is in both the  redundant diagonal and the duplicative PC
  }
  
  #if a quadrant is missing
  if (length(setdiff(CFs_all[CFs_all != "Central"], df$pca)) > 0) {
    missing_quadrants <- setdiff(CFs_all[CFs_all != "Central"], df$pca)
    if (missing_quadrants %in% df$corners %>% unique()) {
      #assign corners selection to that CF
      df$pca[which(df$corners == setdiff(CFs_all[CFs_all != "Central"], df$pca))] = setdiff(CFs_all[CFs_all != "Central"], df$pca)
    }
  }
  
  #If there is a redundant GCM #KEC: Should this be redundant quadrant / CF? 
  if (!nrow(PCA[duplicated(PCA$GCM),]) > 0) {
    df$pca = df$pca #Do nothing - otherwise end up with empty quadrant. This line could be removed and make the previous statment inverse but it makes it more confusing what's gonig on that way
  } else {
    df$pca[which(df$GCM == ID.redundant.gcm(PCA))] = NA #Removes the GCM that is in redundant diagonal
  }
  
  ## Return Selected Models
  
  # return selected methods based on method identified in parameters
  selected_gcms <- df %>% 
    tidyr::drop_na("pca") %>% 
    dplyr::select(GCM, CF, !!dplyr::sym(x), !!dplyr::sym(y))
  
  #Presentation Figure:
  
  ###Scatter plot showing delta precip and tavg, color by emissions scenario, with box for central CF
  
  if (to_plot == TRUE) {
    
    if (fill_var == "CF") {
      fill_asth <- ggplot2::scale_fill_manual(values = c("gray", "darkred", "darkblue", "pink", "lightblue"))
      guide_asth <- ggplot2::guides(fill = ggplot2::guide_legend(title = fill_var_name, 
                                               override.aes = list(shape = 21, size = 5, color = "black")),
                           shape = ggplot2::guide_legend(override.aes = list(fill = "black"))) 
    } else {
      fill_asth <- ggplot2::scale_fill_gradient(fill_var_name, guide = "colorbar", high = "red", low = "yellow")
      guide_asth <- ggplot2::guides(fill = "colorbar")
    }
    
    p <- ggplot2::ggplot(df, ggplot2::aes(!!dplyr::sym(x), !!dplyr::sym(y), xmin = q25_x, xmax = q75_x, 
                        ymin = q25_y, ymax = q75_y) ) + 
      ggplot2::geom_point(ggplot2::aes(shape = RCP, fill = !!dplyr::sym(fill_var)), size = 4, stroke = 1) +
      #PlotTheme +
      ggplot2::labs(
        title = paste(park, " Changes in climate means in", 2050 , "by GCM run"),
        x =paste0("Change in ", x_name),
        y = paste0("Change in ", y_name)
      ) +
      fill_asth +
      ggplot2::scale_shape_manual(values = c(21, 24)) +
      guide_asth +
      
      #geom_point(colour="black",size=4) +
      ggplot2::theme(
        axis.text = ggplot2::element_text(size = 16),
        axis.title.x = ggplot2::element_text(size = 16, vjust = -0.2),
        axis.title.y = ggplot2::element_text(size = 16, vjust = 0.2),
        plot.title = ggplot2::element_text(
          size = 20,
          face = "bold",
          vjust = 2,
          hjust = 0.5
        ),
        legend.text = ggplot2::element_text(size = 16),
        legend.title = ggplot2::element_text(size = 16),
        legend.key = ggplot2::element_blank() 
      ) +
      ggplot2::geom_rect(color = "black", alpha = 0) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = mean_y), linetype = 2) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = mean_x), linetype = 2) +
      ggforce::geom_mark_circle(data = dplyr::filter(df, GCM %in% selected_gcms$GCM),
                       ggplot2::aes(group = GCM), expand = 0.025, size = 1, linetype = 2) + 
      {if (!is.null(highlight)) 
        ggforce::geom_mark_circle(data = dplyr::filter(df, GCM %in% selected_gcms$GCM[selected_gcms$CF %in% highlight]),
                         ggplot2::aes(group = GCM), expand = 0.04, size = 1.5, color = "gold2")} +
      ggrepel::geom_text_repel(ggplot2::aes(label = GCM))
    
    return(list(selected_gcms, p))
    
  } else {
    return(selected_gcms)
  }
}
