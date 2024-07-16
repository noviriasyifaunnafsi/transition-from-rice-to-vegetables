library(ggplot2)
library(ggrepel)
library(ggthemes)
#Call data for stakeholders' expertise, willingness and potential contribution 
#in the decision analysis#

stakeholder<-read.csv("stakeholders.csv", sep = ";")
# Plot stakeholders' experience, availability and expertise in decision analysis ####

expertplot<-ggplot(data = stakeholder, aes(x=Experience, y=Availability, 
                              label =stakeholders, color=Expertise))+ 
  geom_point(aes(shape=Gender))+
  xlab("Relevant Experiences")+
  # labs(title = "Categorization of relevant stakeholders in transitioning from rice to vegetables in upland area of Indonesia")+
 #label names of stakeholder and expand space to show full names
  scale_x_continuous(labels = paste(seq(0, 5, by = 1)), 
                     breaks = seq(0,5, by = 1), 
                     limits = c(0, 5),
                     expand = c(0,1))+
  scale_y_continuous(labels = paste(seq(0, 5, by = 1)), 
                     breaks = seq(0,5, by = 1), 
                     limits = c(0, 5),
                     expand = c(0,1))+
  theme(plot.margin = unit(c(1,1,1,1), "cm"),
        legend.title = element_text(size = 12),
        legend.position = "right",
        # legend.justification = c("right", "top"),
        # legend.box.just = "left",
        legend.margin = margin(2,2,2,2),
        legend.text = element_text(size = 11),
        plot.title = element_text(size=rel(1.7)),
        plot.title.position = "plot",
        panel.background = element_rect(fill = "lightyellow"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        # panel.border = element_rect(linetype = "solid", fill = NA),
        panel.ontop = FALSE,
        strip.background = element_rect(colour = "black", fill = "white"))+

  # Create line to categorize stakeholders
  geom_hline(yintercept=2.5, color="yellow", size=0.5)+
  geom_vline(xintercept=2.5, color="yellow", size=0.5)+

  # Show all names of overlapped values
  # https://ggrepel.slowkow.com/articles/examples.html
  
  geom_text_repel(box.padding = 0.5, max.overlaps = Inf, size=4)+
  annotate("text", label = "Potential core experts",
    x = 4.5, y = 3.3, size = 5, colour = "grey")+
  annotate("text", label = "Resource persons",
           x = 4.5, y = 0.25, size = 5, colour = "grey")

plot(expertplot)

  # save the plot
  ggsave("expertplot.png", dpi=400, height=7, width = 15)
 
  ggsave("expertplot1.png", dpi=400, height=7, width = 15)
  
  ggsave("expertplot2.png", dpi=400, height=7, width = 15)
  
  ggsave("expertplot3.png", dpi=400, height=7, width = 15)
  
  ggsave("expertplot4.png", dpi=400, height=7, width = 15)
  
  ggsave("expertplot5.png", dpi=400, height=8, width = 15)
  
  ggsave("expertplot6.png", dpi=400, height=8, width = 15)
  
  ggsave("expertplot7.png", dpi=400, height=10, width = 15)
  
  ggsave("expertplot8.png", dpi=400, height=10, width = 15)
  
  ggsave("expertplot9.png", dpi=400, height=10, width = 15)
  
  ggsave("expertplot10.png", dpi=400, height=8, width = 13)
  
  ggsave("expertplot11.png", dpi=300, height=9, width = 19)
  
  ggsave("expertplot12.png", dpi=300, height=12, width = 15)

  
  #2. Stakeholder power and interest analysis

  stakeholder_power_interest<-ggplot(data = stakeholder, aes(x=Interest, y=Influence, 
                                                            label=stakeholders, color=Attitude, size=Relevance))+ 
    geom_point(alpha=0.5)+
    xlab("Perceived Interest")+
    ylab("Perceived Influence")+
    labs (color="Attitude", size="Relevance")+
    
    #label names of stakeholder and expand space to show full names
    scale_x_continuous(labels = paste(seq(0, 5, by = 1)), 
                       breaks = seq(0,5, by = 1), 
                       limits = c(-0.5, 5.5))+
    scale_y_continuous(labels = paste(seq(0, 5, by = 1)), 
                       breaks = seq(0,5, by = 1), 
                       limits = c(-0.5, 5.5))+
    
    # Show the size from 0 to 5, not only from 2-5 although we do not have 0 and 1 in the data table
    scale_size(labels = paste(seq(0, 5, by = 1)), 
               breaks = seq(0,5, by = 1), 
               limits = c(0, 5))+
    
    coord_cartesian(clip = "off")+
    
    theme(plot.margin = unit(c(1,1,1,1), "cm"),
                legend.position = "right",
                legend.text = element_text(size = 12),
                plot.title = element_text(size=rel(1.7)),
                plot.title.position = "plot",
                panel.background = element_rect(fill = "lightyellow"),
                panel.ontop = FALSE,
                strip.background = element_rect(colour = "black", fill = "white"))+
    
    scale_color_manual(breaks = c("Positive", "Neutral", "No information"),
                       values=c("darkgreen", "blue", "brown"))+
    # Create line to categorize stakeholders
    geom_hline(yintercept=2.5, color="lightgrey", size=0.5)+
    geom_vline(xintercept=2.5, color="lightgrey", size=0.5)+
    # Show all names of overlapped values
    # https://ggrepel.slowkow.com/articles/examples.html
    geom_text_repel(box.padding = 0.3, max.overlaps = Inf, size=2)+
    annotate("text", label = "High Interest, High Influence",
             x = 4.0, y = 5.5, size = 3.8, colour = "darkgrey")+
    annotate("text", label = "High Interest, Low Influence",
             x = 4.0, y = -0.5, size = 3.8, colour = "darkgrey")+
    annotate("text", label = "Low Interest, High Influence",
             x = 1.0, y = 5.5, size = 3.8, colour = "darkgrey")+
    annotate("text", label = "Low Interest, Low Influence",
             x = 1.0, y = -0.5, size = 3.8, colour = "darkgrey")
  
  ggsave("stakeholders_power_interest.png", dpi=400, height=7, width = 8)
  
  ggsave("stakeholders_power_interest1.png", dpi=400, height=7, width = 8)
  
  ggsave("stakeholders_power_interest2.png", dpi=400, height=7, width = 8)
  