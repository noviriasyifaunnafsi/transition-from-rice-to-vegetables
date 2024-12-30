library(ggplot2)
library(ggrepel)
library(ggthemes)

#Call data for stakeholders' expertise, willingness and potential contribution 
#in the decision analysis#
stakeholder <- read.csv("stakeholders.csv", sep = ";")


# Plot stakeholders' experience, availability and expertise in decision analysis ####
expertplot <- ggplot(data = stakeholder, aes(x = Experience, 
                                             y = Availability, 
                                             label = stakeholders, 
                                             color = Expertise)) + 
  geom_point(aes(shape = Gender)) +
  labs(title = "Stakeholder Analysis", color = "Expertise", shape = "Gender") +
  xlab("Relevant Experiences") +
  scale_x_continuous(labels = paste(seq(0, 5, by = 1)), 
                     breaks = seq(0, 5, by = 1), 
                     limits = c(0, 5),
                     expand = c(0, 1)) +
  scale_y_continuous(labels = paste(seq(0, 5, by = 1)), 
                     breaks = seq(0, 5, by = 1), 
                     limits = c(0, 5),
                     expand = c(0, 1)) +
  theme(
    aspect.ratio = 1, # Makes the plot panel square
    plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"), # Reduce margins around plot
    axis.text = element_text(size = 13), # Adjust axis text size
    axis.title = element_text(size = 13), # Adjust axis title size
    axis.title.x = element_text(size = 15, face = "bold"),
    axis.title.y = element_text(size = 15, face = "bold"),
    legend.title = element_text(size = 14), face = "bold",
    legend.position = "bottom", # Legends below the plot
    legend.box = "horizontal", # Arrange legend types side by side
    legend.box.just = "center", # Center-align legends
    legend.spacing.x = unit(0.5, "cm"), # Add horizontal spacing
    legend.spacing.y = unit(0.3, "cm"), # Add vertical spacing
    legend.margin = margin(5, 5, 5, 5), # Adjust margins around legends
    legend.key.size = unit(0.5, "cm"), # Adjust size of legend keys
    legend.text = element_text(size = 12), # Adjust legend text size
    plot.title = element_text(size = rel(1.7), hjust = 0.5, face = "bold"),
    plot.title.position = "plot",
    panel.background = element_rect(fill = "aliceblue"),
    panel.grid.minor = element_line(colour = "white"),
    panel.ontop = FALSE,
    strip.background = element_rect(colour = "black", fill = "white")
  ) +
  guides(
    color = guide_legend(order = 1, ncol = 1), # Expertise legend (1 column)
    shape = guide_legend(order = 2, ncol = 1)  # Gender legend (1 column)
  ) +
  # Categorization lines
  geom_hline(yintercept = 2.5, color = "lightgrey", size = 0.5) +
  geom_vline(xintercept = 2.5, color = "lightgrey", size = 0.5) +
  
  # Display overlapped stakeholder names
  geom_text_repel(box.padding = 0.3, max.overlaps = Inf, size = 4) +
  
  # Annotations
  annotate("text", label = "Potential core experts",
           x = 4.5, y = 3.3, size = 6.5, colour = "darkgrey") +
  annotate("text", label = "Resource persons",
           x = 4.5, y = 0.25, size = 6.5, colour = "darkgrey")

# Save the plot (optional)
ggsave("expertplot_blue.png", plot = expertplot, dpi = 300, 
       height = 15, width = 12)

# Display the plot
plot(expertplot)



#2. Stakeholder power and interest analysis

stakeholder_power_interest <- ggplot(data = stakeholder, 
                                     aes(x = Interest, 
                                         y = Influence,
                                         label = stakeholders, 
                                         color = Attitude, 
                                         size = Relevance)) +
  geom_point(alpha = 0.5) +
  labs(title = "Stakeholder Power Interest Analysis") +
  xlab("Perceived Interest") +
  ylab("Perceived Influence") +
  labs(alpha = "Attitude", shape = "Relevance") +
    
    scale_x_continuous(labels = paste(seq(0, 5, by = 1)), 
                       breaks = seq(0, 5, by = 1), 
                       limits = c(-0.5, 5.5)) +
    scale_y_continuous(labels = paste(seq(0, 5, by = 1)), 
                       breaks = seq(0, 5, by = 1), 
                       limits = c(-0.5, 5.5)) +
    
    scale_size(labels = paste(seq(0, 5, by = 1)), 
               breaks = seq(0, 5, by = 1), 
               limits = c(0, 5)) +
    
    coord_cartesian(clip = "off") +
    
    theme(
      plot.margin = unit(c(1, 1, 1, 1), "cm"),
      legend.position = "right",
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 12),
      axis.text = element_text(size = 11), # Adjust axis text size
      axis.title = element_text(size = 12), # Adjust axis title size
      plot.title = element_text(size = rel(1.2), hjust = 0.5, face = "bold"),
      panel.background = element_rect(fill = "aliceblue"),
      panel.ontop = FALSE,
      strip.background = element_rect(colour = "black", fill = "white")
    ) +
    
    scale_color_manual(breaks = c("Positive", "Neutral", "No information"),
                       values = c("darkgreen", "blue", "saddlebrown")) +
    
    geom_hline(yintercept = 2.5, color = "lightgrey", size = 0.5) +
    geom_vline(xintercept = 2.5, color = "lightgrey", size = 0.5) +
    
    geom_text_repel(box.padding = 0.3, max.overlaps = Inf, size = 2) +
    
    annotate("text", label = "High Interest, High Influence",
             x = 4.0, y = 5.5, size = 3, colour = "darkgrey") +
    annotate("text", label = "High Interest, Low Influence",
             x = 4.0, y = -0.5, size = 3, colour = "darkgrey") +
    annotate("text", label = "Low Interest, High Influence",
             x = 1.0, y = 5.5, size = 3, colour = "darkgrey") +
    annotate("text", label = "Low Interest, Low Influence",
             x = 1.0, y = -0.5, size = 3, colour = "darkgrey")
  
  ggsave("stakeholders_power_interest.png", dpi = 400, height = 8, width = 7.5)
  