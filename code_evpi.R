# VoI analysis (EVPI) ####
mcSimulation_table <- data.frame(TRV_mc_simulation$x, TRV_mc_simulation$y[1:2])
evpi_TRV <- multi_EVPI(mc = mcSimulation_table, 
                       first_out_var = "NPV_vegetables_decision_do_with_gov_assistance", 
                       write_table = TRUE)

## Plot EVPI individually (?) ####

# EVPI with government assistance
plot_evpi_vegetable_decision_with_gov_assistance  <- 
  plot_evpi(evpi_TRV,
            decision_vars = c("NPV_vegetables_decision_do_with_gov_assistance"),
            new_names = c("NPV vegetables decision with government assistance"),
            bar_color = c("seagreen3"),
            base_size = 15)+
  scale_y_discrete(
    position = "right", # Move y-axis to the right
    labels = function(x) gsub("_", " ", x)  # Replace underscores with spaces
  ) 

# EVPI without government assistance
plot_evpi_vegetable_decision_with_gov_assistance  <- 
  plot_evpi(evpi_TRV,
            decision_vars = c("NPV_vegetables_decision_do_with_gov_assistance"),
            new_names = c("With Government Assistance"),
            bar_color = c("seagreen3"),
            base_size = 20) +
  scale_y_discrete(
    position = "right", # Move y-axis to the right
    labels = function(x) gsub("_", " ", x)  # Replace underscores with spaces
  ) +
  labs(x = "EVPI (USD)", y = "Variable", size = 10)+
  theme(
    legend.text = element_text(size = 12), # Adjust legend text size
    axis.title.y = element_blank(), # Optional: Remove y-axis title
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 12) # Adjust y-axis label text size
  )

# Save EVPI plot figure 
png("evpi_with.png", width = 3000, height = 1500, res = 250)
plot(plot_evpi_vegetable_decision_with_gov_assistance)
dev.off()


# EVPI without government assistance
plot_evpi_vegetable_decision_without_gov_assistance  <- 
  plot_evpi(evpi_TRV,
            decision_vars = c("NPV_vegetables_decision_do_without_gov_assistance"),
            new_names = c("Without Government Assistance"),
            bar_color = c("lightblue"),
            base_size = 20)+
  scale_y_discrete(
    position = "right", # Move y-axis to the right
    labels = function(x) gsub("_", " ", x)  # Replace underscores with spaces
  ) +
  labs(x = "EVPI (USD)", y = "Variable", size = 10)+
  theme(
    legend.text = element_text(size = 12), # Adjust legend text size
    axis.title.y = element_blank(), # Optional: Remove y-axis title
    axis.text.y = element_text(size = 12) # Adjust y-axis label text size
  )

# Save EVPI plot figure   
png("evpi_without.png", width = 3000, height = 1500, res = 250)
plot(plot_evpi_vegetable_decision_without_gov_assistance)
dev.off()

# Combined two individual EVPI plot
evpi_combined <- (plot_evpi_vegetable_decision_with_gov_assistance / plot_evpi_vegetable_decision_without_gov_assistance) 


# Save combined EVPI plot figure
png("evpi_combined.png", width = 2000, height = 1500, res = 250)
plot(evpi_combined)
dev.off()


## Plot EVPI (better?) ####

# Load the first dataset
data1 <- read.csv("EVPI_table_NPV_vegetables_decision_do_with_gov_assistance.csv")
data1 <- data1 %>%
  filter(!is.na(EVPI_do) & EVPI_do != 0) %>%
  mutate(source = "With Government Assistance") # Add a column for the source

# Load the second dataset
data2 <- read.csv("EVPI_table_NPV_vegetables_decision_do_without_gov_assistance.csv")
data2 <- data2 %>%
  filter(!is.na(EVPI_do) & EVPI_do != 0) %>%
  mutate(source = "Without Government Assistance") # Add a column for the source

# Combine the two datasets
combined_data <- bind_rows(data1, data2)

# Calculate percentages within each variable
combined_data <- combined_data %>%
  group_by(variable) %>%
  mutate(percentage = (EVPI_do / sum(EVPI_do)) * 100) %>%
  ungroup()

# Replace underscores in variable names with spaces (for labels)
combined_data <- combined_data %>%
  mutate(variable = gsub("_", " ", variable),  # Replace underscores with spaces
         variable = toTitleCase(variable)) # Capitalize each word


# Create a single combined plot with percentages
EVPI_combined_plot <- 
  ggplot(combined_data, aes(x = "", y = EVPI_do, fill = source)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_stack(vjust = 0.5), size = 4) + # Add percentage labels within stacks
  coord_flip() + # Flip for horizontal bar style
  labs(
    x = NULL,
    y = "Expected Value of Perfect Information (USD)",
    fill = "Scenario",
    title = "EVPI with Percentages for Each Variable"
  ) +
  facet_wrap(~variable, scales = "free", ncol = 1) + # Remove additional text from labels
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5), # Bold and italic title
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    strip.text = element_text(size = 13, face = "bold.italic", hjust = 0), # Style facet labels
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold"),
    strip.background = element_blank() # Optional: Remove background of facet titles
  )

# Save EVPI plot figure
png("evpi_combined_plot.png", width = 2000, height = 1500, res = 250)
plot(EVPI_combined_plot)
dev.off()

