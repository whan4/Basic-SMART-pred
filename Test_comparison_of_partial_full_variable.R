# Install and load the necessary packages  
if (!requireNamespace("ggplot2", quietly = TRUE)) {  
  install.packages("ggplot2")  
}  
if (!requireNamespace("reshape2", quietly = TRUE)) {  
  install.packages("reshape2")  
}  
if (!requireNamespace("scales", quietly = TRUE)) {  
  install.packages("scales")  
}  

library(ggplot2)  
library(reshape2)  
library(scales)  

# Read csv file  
part_test_metrics <- read.csv("~/Desktop/ML_pipeline/pipline/part_test_Metrics.csv", stringsAsFactors = FALSE)  
full_test_metrics <- read.csv("~/Desktop/ML_pipeline/pipline/full_test_Metrics.csv", stringsAsFactors = FALSE)  
part_test_metrics$Model <- NULL
full_test_metrics$Model <- NULL
if (!is.null(part_test_metrics$Best_Params)) {
  part_test_metrics$Best_Params <- NULL
  full_test_metrics$Best_Params <- NULL
}
if (!is.null(full_test_metrics$Accuracy..95..CI.))
{
  full_test_metrics$Accuracy..95..CI. <- NULL
}
# Add a new column to identify the source of the dataset  
part_test_metrics$Model <- "Reduced model"  
full_test_metrics$Model <- "Full model"  

# Merge two data frames  
combined_data <- rbind(part_test_metrics, full_test_metrics)  

# Convert the data frame to long format  
data_long <- melt(combined_data, id.vars = c("X", "Model"),   
                  variable.name = "Metric", value.name = "Value")  
data_long$Value <- as.numeric(as.character(data_long$Value)) 

# Set labels for each plot
model_labels <- setNames(  
  paste0("(", letters[1:length(full_test_metrics$X)], ") ", full_test_metrics$X),  
  full_test_metrics$X
)  

# Draw a chart  
ggplot(data_long, aes(x = Metric, y = Value, fill = Model)) +  
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +  
  facet_wrap(~ factor(X, full_test_metrics$X), scales = "free_x", ncol = 3, labeller = as_labeller(model_labels)) +  
  labs(x = "Performance metrics",  
       y = "Metric score") +  
  scale_y_continuous(labels = number_format(accuracy = 0.0001)) +  
  theme_minimal(base_size = 20) +  
  theme(  
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  
    axis.text.y = element_text(size = 12),  
    legend.position = c(0.9, 0.05), 
    legend.justification = c(1, 0),  
    legend.box.just = "right",
    legend.box = "horizontal",  
    legend.key.size = unit(1, "cm"),
    legend.title.position = "left",
    legend.text = element_text(size = 18),
    strip.text = element_text(size = 20), 
    panel.spacing = unit(2, "lines"),  
    plot.margin = unit(c(20, 20, 20, 20), "points")  
  ) +  guides(fill=guide_legend(nrow=1))
  coord_cartesian(clip = "off") 

ggsave("./test_comparison.png", width = 15, height =18, units = "in", dpi = 300)