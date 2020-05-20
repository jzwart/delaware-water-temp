plot_downscaled_met <- function(met_file_names, VarNames, working_directory){

  full.data = NULL
  
  for(i in 2:length(met_file_names)){
    tmp.data = read.csv(met_file_names[i]) %>%
      dplyr::mutate(ens = i - 1)
    full.data = rbind(full.data, tmp.data)
  }
  pdf(paste0(working_directory, "/downscaled_met_plots.pdf"))
  for(i in 1:length(VarNames)){
   print(ggplot(data = full.data, aes(x = time)) +
            geom_line(aes(y = get(paste(VarNames[i])), color = "Downscaled", group = ens), alpha = 0.3) +
            ylab(paste(VarNames[i]))+
            xlab("time")+
            theme_linedraw() + 
            theme(text = element_text(size = 14)) +
            scale_color_manual(values = c("firebrick2","black")))
  }
  dev.off()
}

