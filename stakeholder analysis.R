library(ggplot2)
library(ggrepel)
library(ggthemes)

# Call data for stakeholders' expertise, willingness and potential contribution 
# in the decision analysis 
stakeholder <- read.csv("stakeholders.csv", sep = ";")


# Plot stakeholders' perceived interest, influence, relevance, and attitude 
# in decision analysis

stakeholder_analysis <- ggplot(data = stakeholder, 
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
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    axis.text = element_text(size = 14), # Adjust axis text size
    axis.title = element_text(size = 16), # Adjust axis title size
    plot.title = element_text(size = rel(1.2), hjust = 0.5, face = "bold"),
    panel.background = element_rect(fill = "aliceblue"),
    panel.ontop = FALSE,
    strip.background = element_rect(colour = "black", fill = "white")
  ) +
  
  
  scale_color_manual(breaks = c("Positive", "Neutral", "No information"),
                     values = c("darkgreen", "blue", "saddlebrown")) +
  
  geom_hline(yintercept = 2.5, color = "lightgrey", size = 0.5) +
  geom_vline(xintercept = 2.5, color = "lightgrey", size = 0.5) +
  
  geom_text_repel(box.padding = 0.3, max.overlaps = Inf, size = 3) +
  
  annotate("text", label = "High Interest, High Influence",
           x = 4.0, y = 5.5, size = 3.5, colour = "darkgrey") +
  annotate("text", label = "High Interest, Low Influence",
           x = 4.0, y = -0.5, size = 3.5, colour = "darkgrey") +
  annotate("text", label = "Low Interest, High Influence",
           x = 1.0, y = 5.5, size = 3.5, colour = "darkgrey") +
  annotate("text", label = "Low Interest, Low Influence",
           x = 1.0, y = -0.5, size = 3.5, colour = "darkgrey")

ggsave("stakeholders_interest_influence.png", dpi = 400, height = 8, width = 7.5)
