#### Extend colorsclae ####
get_colors <- function(nb.cols=18){
  # Adds nb.cols-8 colors to extend the color ramp
  mycolors <- colorRampPalette(brewer.pal(8, "Set1"))(nb.cols)
  return(mycolors)
}


#### Brackets grob ####
bracketsGrob <- function(...){
  # Adds brackets to graph
  l <- list(...)
  e <- new.env()
  e$l <- l
  grid:::recordGrob(  {
    do.call(grid.brackets, l)
  }, e)
}

#### Probability density for a given topic among the class dataframe ####
prob_dens <- function(class_df, preds_df, i){
  class_df  %>% 
    group_by(Topic) %>% 
    mutate(Prop = Frequency/sum(Frequency)) %>% 
    ungroup() %>% 
    select(Topic, Class, Prop) %>% 
    # Select only one document and its topic
    filter(Topic == preds_df[i, "Topic"] %>% pull()) %>%
    pivot_wider(names_prefix = "Class", names_from = Class, values_from = Prop)
}

#### Get the probability distribution for each topic among its predicted topic ####
top_pred_test <- function(class_list, pred_list){
  results_list <- list()
  
  # Loop over all models 
  for(model_idx in 1:length(pred_list)){
    # Retrieve the classes and predictions for the current model
    classes <- class_list[[model_idx]]
    preds <- pred_list[[model_idx]]
    
    # Calculate the probability distribution for each document
    res_df <- lapply(seq_len(nrow(preds)), function(x) prob_dens(classes, preds, x))
    # Save the list as a tibble
    results_list[[model_idx]] <- bind_rows(res_df)
  }
  
  top_pred <- results_list[[1]] %>% tibble::rownames_to_column() %>% mutate(model = 1)
  # Append all rows to one data frame
  for(i in 2:length(results_list)){
    top_pred <- bind_rows(top_pred, results_list[[i]] %>% 
                            tibble::rownames_to_column() %>% mutate(model = i))
  }
  # # Append all rows to one data frame
  # bind_rows(results_list[[1]] %>% tibble::rownames_to_column() %>% mutate(model = 1), 
  #           results_list[[2]] %>% tibble::rownames_to_column() %>% mutate(model = 2), 
  #           results_list[[3]] %>% tibble::rownames_to_column() %>% mutate(model = 3), 
  #           results_list[[4]] %>% tibble::rownames_to_column() %>% mutate(model = 4), 
  #           results_list[[5]] %>% tibble::rownames_to_column() %>% mutate(model = 5), 
  #           results_list[[6]] %>% tibble::rownames_to_column() %>% mutate(model = 6)) -> df
  return(top_pred)
  
}

# Save this as probability distributions
#top_pred_test()


#### Topics per class####
facet_class <- function(df, mycolors = get_colors()) {
  
  df %>% 
    mutate(Class = recode(Class, 
                          "-1" = "Unknown",
                          "0" = "Action",
                          "1" = "Adventure",
                          "2" = "Animation",
                          "3" = "Comedy",
                          "4" = "Crime",
                          "5" = "Drama",
                          "6" = "Family",
                          "7" = "Film Noir",
                          "8" = "Historical",
                          "9" = "Horror",
                          "10" = "Musical",
                          "11" = "Mystery",
                          "12" = "Romance",
                          "13" = "Sci-Fi",
                          "14" = "Thriller",
                          "16" = "War",
                          "17" = "Western"
    )) %>% 
    mutate(Class = factor(Class)) %>% 
    group_by(Class) %>% 
    mutate(Prop = Frequency/sum(Frequency)) %>% 
    # Keep only the top 7 proportions for each Class
    slice_max(order_by=Prop, n=7) %>% 
    ggplot(aes(factor(Topic), Prop, fill=Class)) + 
    geom_bar(stat="identity", width=0.5) + 
    facet_wrap(~Class, scales = "free_x", ncol=3) + 
    xlab("Topic") + ylab("Proportion") +
    theme_bw() + 
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.position = "none", 
          strip.text = element_text(face = "bold"),
          strip.background = element_rect(fill="lightcyan2"),
          axis.title = element_text(face = "bold"),
          axis.text = element_text(color="black", size=12),
          axis.text.x = element_text(angle=0)) + 
    scale_fill_manual(values = mycolors, drop=FALSE)
}

#### Classes per topics ####
facet_topics <- function(df, mycolors = get_colors()) {
  df %>% 
    mutate(Class = recode(Class, 
                          "-1" = "Unknown",
                          "0" = "Action",
                          "1" = "Adventure",
                          "2" = "Animation",
                          "3" = "Comedy",
                          "4" = "Crime",
                          "5" = "Drama",
                          "6" = "Family",
                          "7" = "Film Noir",
                          "8" = "Historical",
                          "9" = "Horror",
                          "10" = "Musical",
                          "11" = "Mystery",
                          "12" = "Romance",
                          "13" = "Sci-Fi",
                          "14" = "Thriller",
                          "16" = "War",
                          "17" = "Western"
    )) %>% 
    mutate(Class = factor(Class)) %>% 
    group_by(Topic) %>% 
    mutate(Prop = Frequency/sum(Frequency)) %>% 
    # Keep only the top 3 for each topic
    slice_max(order_by=Prop, n=3) %>%
    ungroup() %>% 
    # Split into groups with width 8
    mutate(Group = cut_width(Topic, width=8)) %>% 
    # Convert the typical facet text to Topics: start-end
    mutate(Group = factor(Group, labels = 
                            str_replace(
                              paste("Topics: ",
                                    paste(
                                      apply(t(sapply(str_split(levels(Group), ","), c)), 1, 
                                            function(x) as.numeric(gsub("\\(|\\[|\\]|\\)", "", x)))[1,] + 1,
                                      str_replace(apply(t(sapply(str_split(levels(Group), ","), c)), 1, 
                                                        function(x) gsub("\\(|\\[|\\]|\\)", "", x))[2,],
                                                  last(apply(t(sapply(str_split(levels(Group), ","), c)), 1, 
                                                             function(x) gsub("\\(|\\[|\\]|\\)", "", x))[2,]),
                                                  as.character(max(Topic))),
                                      sep = "\u2013"
                                    ),
                                    sep = ""),
                              "-\\d", "-1"
                            ))) %>% 
    ggplot(., aes(factor(Topic), Prop, fill=Class)) +
    geom_bar(position=position_dodge(width=0.7), width = 0.5, stat="identity") + 
    facet_wrap(~Group, scales="free_x", ncol=3) +
    xlab("Topic") + ylab("Proportion") +
    theme_bw() + 
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.position = "top", 
          legend.direction = "horizontal",
          legend.text = element_text(size=12),
          strip.text = element_text(face = "bold"),
          strip.background = element_rect(fill="lightcyan2"),
          axis.title = element_text(face = "bold"),
          axis.text = element_text(color="black", size=12),
          axis.text.x = element_text(angle=0)) + 
    scale_fill_manual(name = "Genres", values = mycolors, drop = FALSE,
                      # Change legend attributes
                      guide = guide_legend(title.position = "top", 
                                           title.hjust = 0.5, 
                                           title.theme = element_text(face="bold"),
                                           nrow = 3, 
                                           keywidth = unit(0.5, "cm"),
                                           keyheight = unit(0.5, "cm")) 
    )
}

#### (Mean) probability distribution of genres #### 
mean_prob_class <- function(prob_df, model_id){
  prob_df %>%
    select(-Topic) %>% 
    filter(model == model_id) %>% 
    select(-model) %>% 
    mutate(rowname = as.integer(rowname)) %>% 
    pivot_longer(-rowname) %>% 
    mutate(Class = recode(name, 
                          "Class-1" = "Unknown",
                          "Class0" = "Action",
                          "Class1" = "Adventure",
                          "Class2" = "Animation",
                          "Class3" = "Comedy",
                          "Class4" = "Crime",
                          "Class5" = "Drama",
                          "Class6" = "Family",
                          "Class7" = "Film Noir",
                          "Class8" = "Historical",
                          "Class9" = "Horror",
                          "Class10" = "Musical",
                          "Class11" = "Mystery",
                          "Class12" = "Romance",
                          "Class13" = "Sci-Fi",
                          "Class14" = "Thriller",
                          "Class16" = "War",
                          "Class17" = "Western"
    )) %>% 
    replace(is.na(.), 0) %>% 
    mutate(Class = factor(Class)) %>% 
    group_by(Class) %>% 
    summarise(MeanProb = mean(value)) %>% 
    arrange(MeanProb) %>% 
    mutate(Model = model_id)
}


#### Genre distribution for models and true distribution ####
class_dist <- function(prob_df, df_clean) {
  # Colorblind friendly palette
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                 "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  bind_rows(mean_prob_class(prob_df, 1), mean_prob_class(prob_df, 2), mean_prob_class(prob_df, 3),
            mean_prob_class(prob_df, 4), mean_prob_class(prob_df, 5), mean_prob_class(prob_df, 6)) %>% 
    full_join(df_clean %>% 
                select(GenreFix) %>% 
                filter(GenreFix != "unknown") %>% 
                group_by(GenreFix) %>% 
                count() %>% 
                ungroup() %>% 
                mutate(Prop = n/sum(n)) %>% 
                mutate(Class = recode(GenreFix, 
                                      "action" = "Action",
                                      "adventure" = "Adventure",
                                      "animation" = "Animation",
                                      "comedy" = "Comedy",
                                      "crime" = "Crime",
                                      "drama" = "Drama",
                                      "family" = "Family",
                                      "film noir" = "Film Noir",
                                      "historical" = "Historical",
                                      "horror" = "Horror",
                                      "musical" = "Musical",
                                      "mystery" = "Mystery",
                                      "romance" = "Romance",
                                      "science fiction" = "Sci-Fi",
                                      "thriller" = "Thriller",
                                      "war" = "War",
                                      "western" = "Western"
                )) %>% 
                select(Class, Prop) %>% 
                # Model 7 = actual distribution
                mutate(Model = 7)
              , by = c("Class", "Model")) %>% 
    # To shift values from one column to another
    mutate(MeanProb = ifelse(is.na(MeanProb), Prop, MeanProb)) %>% 
    select(-Prop) %>% 
    pivot_longer(-c(Class, Model), names_to = "Variable", values_to = "Prob") %>% 
    ggplot(aes(Class, Prob)) +
    geom_bar(aes(group=Model, fill = factor(Model)), 
             stat="identity", position = "dodge") +
    xlab("Genre") + ylab("Probability") + 
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.position = "top",
          legend.direction = "horizontal",
          legend.text = element_text(size=12),
          strip.text = element_text(face = "bold"),
          strip.background = element_rect(fill="lightcyan2"),
          axis.title = element_text(face = "bold"),
          axis.text = element_text(color="black", size=12),
          axis.text.x = element_text(angle=0)) +
    scale_fill_manual(name="", 
                      #values=brewer.pal(10, "Paired")[c(1:4, 7:8, 6)],
                      values=brewer.pal(10, "Paired")[c(1, 3, 7, 6)], # LDA
                      #labels = c("Unsupervised: Topic size 10", "Semi-Supervised: Topic size 10",
                      #           "Unsupervised: Topic size 20", "Semi-Supervised: Topic size 20",
                      #           "Unsupervised: Topic size 30", "Semi-Supervised: Topic size 30",
                      #           "Actual distribution"),
                      labels = c("20 topics", "30 topics", "40 topics",
                                 "Actual distribution"), # LDA
                      guide = guide_legend(title.position = "top", 
                                           title.hjust = 0.5, 
                                           title.theme = element_text(face="bold"),
                                           nrow = 3, 
                                           keywidth = unit(0.5, "cm"),
                                           keyheight = unit(0.5, "cm")) ) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2))
}


#### HEATMAP ####
pred_heatmap <- function(pred_dist) {
  # Decade information (obtained from rle())
  decade <- c(9,                                           # 1900's
              9+27,                                        # 1910's
              9+27+24,                                     # 1920's
              9+27+24+1,                                   # 1930's
              9+27+24+1+127,                               # 1940's
              9+27+24+1+127+263,                           # 1950's
              9+27+24+1+127+263+483,                       # 1960's
              9+27+24+1+127+263+483+777,                   # 1970's
              9+27+24+1+127+263+483+777+723,               # 1980's
              9+27+24+1+127+263+483+777+723+1251,          # 1990's
              9+27+24+1+127+263+483+777+723+1251+1053,     # 2000's
              9+27+24+1+127+263+483+777+723+1251+1053+1345 # 2010's
            )
  
  # Custom palette of colors
  pal <- RColorBrewer::brewer.pal(8, "Blues")[c(8, 6, 3, 1)]
  
  # Custom fermenter function
  scale_fill_fermenter_custom <- function(pal, na.value = "grey50", guide = "coloursteps", aesthetics = "fill", ...) {
    binned_scale("fill", "fermenter", ggplot2:::binned_pal(scales::manual_pal(unname(pal))), na.value = na.value, guide = guide, ...)  
  }
  
  # Heatmap of obs vs class
  pred_dist %>% 
    # Order by release year
    slice(df_clean %>% filter(Genre == "unknown") %>% tibble::rownames_to_column() %>% 
            arrange(`Release Year`) %>% pull(rowname) %>% as.integer()) %>% 
    mutate(rowname = as.integer(rowname),
           year = df_clean %>% filter(Genre == "unknown") %>% tibble::rownames_to_column() %>% 
             arrange(`Release Year`) %>% pull(`Release Year`) ) %>% 
    pivot_longer(-c(rowname, year)) %>% 
    mutate(Class = recode(name, 
                          "Class-1" = "Unknown",
                          "Class0" = "Action",
                          "Class1" = "Adventure",
                          "Class2" = "Animation",
                          "Class3" = "Comedy",
                          "Class4" = "Crime",
                          "Class5" = "Drama",
                          "Class6" = "Family",
                          "Class7" = "Film Noir",
                          "Class8" = "Historical",
                          "Class9" = "Horror",
                          "Class10" = "Musical",
                          "Class11" = "Mystery",
                          "Class12" = "Romance",
                          "Class13" = "Sci-Fi",
                          "Class14" = "Thriller",
                          "Class16" = "War",
                          "Class17" = "Western"
    )) %>% 
    replace(is.na(.), 0) %>% 
    mutate(Class = factor(Class)) %>% 
    ggplot(aes(rowname, Class, fill=value)) + geom_tile() + 
    scale_x_continuous(expand = c(0.0001, 0), breaks = seq(0, 7083, 1000), 
                       #breaks = seq(1, 6083, 1), 
                       #labels = df_clean %>% filter(Genre == "unknown") %>% 
                       #   tibble::rownames_to_column() %>% 
                       #   arrange(`Release Year`) %>% pull(`Release Year`) %>% 
                       #   str_replace(., "19|20", "") %>% str_replace(., "(?<=\\d)[0-9]", "0's"),
                       guide = guide_axis(check.overlap = TRUE)) +
    scale_y_discrete(expand = c(0, 0)) + 
    xlab("Test set - document index") + ylab("Genre") +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.position = c(0.5, 1.14),#"top", #c(0.83, 0.08), 
          legend.direction = "horizontal",
          legend.text = element_text(size=12),
          strip.text = element_text(face = "bold"),
          strip.background = element_rect(fill="lightcyan2"),
          axis.title = element_text(face = "bold"),
          axis.text = element_text(color="black", size=12),
          axis.text.x = element_text(angle=0),
          plot.margin=unit(c(t=100.5, r=5.5, b=5.5, l=5.5), "points")) + 
    scale_fill_stepsn(colors=pal, 
                      name="Probability",
                      breaks= c(0.05, 0.10, 0.20, 0.5),
                      limits = c(0, 1),
                      # Rescale so values are correct
                      values = scales::rescale(c(0.0025, 0.075, 0.15, 0.35, 0.75), 
                                               from = c(0, 1)),
                      guide = guide_colorsteps(even.steps = TRUE,
                                               title.position = "top",
                                               title.hjust = 0.5,
                                               title.theme = element_text(face="bold"),
                                               barwidth = unit(7, "cm"),
                                               frame.colour = "black",
                                               ticks.colour = "black",
                                               show.limits = TRUE)
    ) + 
    geom_vline(xintercept = decade[5:12], linetype="dashed") + 
    # 1900's-1940's
    annotation_custom(bracketsGrob(x1=decade[5] / decade[12],
                                   y1=1.01, 
                                   x2=0.00, 
                                   y2=1.01, h=-0.03, type=4, lwd=2, col="gray70")) + 
    # 1950's
    annotation_custom(bracketsGrob(x1=decade[6] / decade[12],
                                   y1=1.01, 
                                   x2=decade[5] / decade[12], 
                                   y2=1.01, h=-0.03, type=4, lwd=2, col="gray70")) + 
    # 1960's
    annotation_custom(bracketsGrob(x1=decade[7] / decade[12],
                                   y1=1.01, 
                                   x2=decade[6] / decade[12], 
                                   y2=1.01, h=-0.03, type=4, lwd=2, col="gray70")) + 
    # 1970's
    annotation_custom(bracketsGrob(x1=decade[8] / decade[12], 
                                   y1=1.01, 
                                   x2=decade[7] / decade[12], 
                                   y2=1.01, h=-0.03, type=4, lwd=2, col="gray70")) + 
    # 1980's
    annotation_custom(bracketsGrob(x1=decade[9] / decade[12], 
                                   y1=1.01,
                                   x2=decade[8] / decade[12],
                                   y2=1.01, h=-0.03, type=4, lwd=2, col="gray70")) + 
    # 1990's
    annotation_custom(bracketsGrob(x1=decade[10] / decade[12], 
                                   y1=1.01,
                                   x2=decade[9] / decade[12],
                                   y2=1.01, h=-0.03, type=4, lwd=2, col="gray70")) + 
    # 2000's
    annotation_custom(bracketsGrob(x1=decade[11] / decade[12], 
                                   y1=1.01,
                                   x2=decade[10] / decade[12],
                                   y2=1.01, h=-0.03, type=4, lwd=2, col="gray70")) + 
    # 2010's
    annotation_custom(bracketsGrob(x1=1.00,
                                   y1=1.01, 
                                   x2=decade[11] / decade[12], 
                                   y2=1.01, h=-0.03, type=4, lwd=2, col="gray70")) +
    # Add corresponding text
    annotate("text", x = (decade[5]) / 2,
             y = 18.90, label = "1900's-\n1940's", fontface = 'italic') +
    annotate("text", x = (decade[5] + decade[6]) / 2, 
             y = 18.60, label = "1950's", fontface = 'italic') +
    annotate("text", x = (decade[6] + decade[7]) / 2, 
             y = 18.60, label = "1960's", fontface = 'italic') +
    annotate("text", x = (decade[7] + decade[8]) / 2, 
             y = 18.60, label = "1970's", fontface = 'italic') +
    annotate("text", x = (decade[8] + decade[9]) / 2, 
             y = 18.60, label = "1980's", fontface = 'italic') +
    annotate("text", x = (decade[9] + decade[10]) / 2, 
             y = 18.60, label = "1990's", fontface = 'italic') +
    annotate("text", x = (decade[10] + decade[11]) / 2, 
             y = 18.60, label = "2000's", fontface = 'italic') +
    annotate("text", x = (decade[11] + decade[12]) / 2, 
             y = 18.60, label = "2010's", fontface = 'italic') +
    coord_cartesian(xlim = c(0,6083), ylim=c(0.5,17.5),
                    clip = "off") 
}
