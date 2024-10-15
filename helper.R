# ---------------------------------- #
# --- create_spatial_plot_legend --- #
# ---------------------------------- #

create_spatial_plot_legend <- function(target_level, target_cell_type, color_palette) {

  # Convert the color palette into a data frame
  color_data <- data.frame(
    cell_type = names(color_palette),
    color = unname(color_palette)
  )
  
  # If a specific cell type is selected, set all other colors to gray
  if (target_cell_type != "All") {
    color_data$color <- ifelse(color_data$cell_type == target_cell_type, 
                               color_data$color, 'lightgray')
  }
  
  # Base plot setup with tiles
  legend_plot <- ggplot(color_data, aes(x = factor(cell_type), y = 1, fill = color)) +
    geom_tile() +  
    scale_fill_identity() +
    theme_void() +  # No gridlines or axis titles
    theme(
      legend.position = "none",  # No separate legend, using tiles as legend
      axis.text.x = element_text(angle = 45, hjust = 1, size = 14, vjust = 1),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
  
  # Adjust legend design based on the target level
  if (target_level == "neighborhood") {
    legend_plot <- legend_plot +
      coord_fixed(ratio = 0.4) +
      theme(
        axis.text.x = element_text(size = 12, margin = margin(t = 5))  
      ) 
  } else if (target_level == "l1.cell_type") {
    legend_plot <- legend_plot +
      coord_fixed(ratio = 0.8) + 
      theme(
        plot.margin = margin(5, 50, 5, 50),  
        axis.text.x = element_text(size = 14, margin = margin(t = 5))
      )
  } else if (target_level == "l2.cell_type") {
    legend_plot <- legend_plot +
      theme(
        plot.margin = margin(5, 200, 5, 200),  
        axis.text.x = element_text(size = 14, margin = margin(t = 5))
      )
  } else if (target_level == "l3.cell_type") {
    legend_plot <- legend_plot +
      theme(
        plot.margin = margin(5, 100, 5, 100),  
        axis.text.x = element_text(size = 14, margin = margin(t = 5))
      )
  } else if (target_level == "l3.cell_type") {
    legend_plot <- legend_plot +
      theme(
        plot.margin = margin(5, 40, 5, 40),  
        axis.text.x = element_text(size = 14, margin = margin(t = 5))
      )
  } else {
    # Default margins for other levels
    legend_plot <- legend_plot +
      theme(
        plot.margin = margin(5, 5, 5, 5)
      )
  }
  return(legend_plot)
}


# ------------------------- #
# --- plot_spatial_data --- #
# ------------------------- #

# --- Extracted functions from Seurat Package --- #
NoLegend <- function(...) {
  no.legend.theme <- theme(
    # Remove the legend
    legend.position = 'none',
    # Validate the theme
    validate = TRUE,
    ...
  )
  return(no.legend.theme)
}

NoAxes <- function(..., keep.text = FALSE, keep.ticks = FALSE) {
  blank <- element_blank()
  no.axes.theme <- theme(
    # Remove the axis elements
    axis.line.x = blank,
    axis.line.y = blank,
    # Validate the theme
    validate = TRUE,
    ...
  )
  if (!keep.text) {
    no.axes.theme <- no.axes.theme + theme(
      axis.text.x = blank,
      axis.text.y = blank,
      axis.title.x = blank,
      axis.title.y = blank,
      validate = TRUE,
      ...
    )
  }
  if (!keep.ticks){
    no.axes.theme <- no.axes.theme + theme(
      axis.ticks.x = blank,
      axis.ticks.y = blank,
      validate = TRUE,
      ...
    )
  }
  return(no.axes.theme)
}
# -------------------------------------------------- #

plot_spatial_data <- function(target_anatomic_site, target_level, target_cell_type, target_donor_id, color_palette) {
  if (target_donor_id == "") return(NULL)
  if (target_cell_type == "") return(NULL)
  
  # Create a spatial theme for the plot
  spatial_theme <- theme(
    rect = element_blank(),
    line = element_blank(),
    text = element_text(color = 'black', size = 10),
    title = element_text(face = 'bold', size = 12, color = 'black', hjust = 0.5),
    strip.background = element_rect(fill = 'black', color = 'black'),
    strip.text = element_text(color = 'white', face = 'bold', size = 12),
    plot.title = element_text(size = 14, color = 'black', face = 'bold', hjust = 0.5),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = 'none'
  )
  
  plot_layers <- list(
    geom_point(size = 0.25),
    theme_classic(),
    spatial_theme,
    coord_fixed(),
    scale_color_manual(values = color_palette),
    NoLegend(),
    NoAxes()
  )
  
  # Get metadata from the global environment
  metadata_name <- paste0(target_donor_id, "_human_skin_merfish_atlas_skinny_metadata")
  metadata <- get(metadata_name, envir = .GlobalEnv)
  
  # Filter data based on anatomic site
  filtered_data <- metadata %>% filter(anatomic_site == target_anatomic_site)
  
  # If not viewing "All" cell types, plot only the selected cell type in color and the others in gray
  if (target_cell_type != "All") {
    filtered_data <- filtered_data %>%
      mutate(cell_color = ifelse(!!sym(target_level) == target_cell_type, 
                                 color_palette[as.character(!!sym(target_level))], 'lightgray'))
    
    plot <- ggplot(filtered_data, aes(x = spatial_1, y = spatial_2, color = cell_color)) +
      plot_layers +
      scale_color_identity()
  } else {
    # Plot all cell types with the full color palette
    plot <- ggplot(filtered_data, aes(x = spatial_1, y = spatial_2, color = factor(!!sym(target_level)))) +
      plot_layers
  }
  return(plot)
}

# ------------------------------------- #
# --- create_expression_plot_legend --- #
# ------------------------------------- #

create_expression_plot_legend <- function(patient_gene_expression_range, expression_color_palette) {
  
  # Create a data frame that spans the range of expression values
  legend_data <- data.frame(x = seq(patient_gene_expression_range$min, patient_gene_expression_range$max, length.out = 100))

  # Create the legend plot using ggplot
  legend_plot <- ggplot(legend_data, aes(x = x, y = 1, fill = x)) +
            geom_tile() +
            scale_fill_gradientn(
              colors = expression_color_palette,  # Define color gradient
              limits = c(patient_gene_expression_range$min, patient_gene_expression_range$max)
            ) +
            scale_x_continuous(name = NULL, expand = c(0, 0)) +  # Adding x-axis labels
            theme_minimal() +
            ggtitle("Expression Level") +  # Adding plot title
            theme(
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.x = element_text(size = 13),
              panel.grid = element_blank(),
              legend.position = "none",
              plot.title = element_text(size = 15, hjust = 0.5, vjust = 1)  # Centering and styling the plot title
            )
  
  return(legend_plot)
}

# ---------------------------- #
# --- plot_expression_data --- #
# ---------------------------- #

plot_expression_data <- function(target_anatomic_site, target_gene, target_donor_id, expression_color_palette, patient_gene_expression_range) {

  if (target_donor_id == "" || target_gene == "") return(NULL) # Early return if any key input is missing

  # Theme for spatial plots
  spatial_theme <- theme(
    rect = element_blank(),
    line = element_blank(),
    text = element_text(color = 'black', size = 10),
    title = element_blank(),
    strip.background = element_rect(fill = 'black', color = 'black'),
    strip.text = element_text(color = 'white', face = 'bold', size = 12),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.position = 'none'
  )

  plot_layers <- list(
    geom_point(size = 0.25),
    theme_classic(),
    spatial_theme,
    coord_fixed(),
    scale_color_gradientn(colors = expression_color_palette, na.value = "lightgray", limits = c(patient_gene_expression_range$min, patient_gene_expression_range$max), oob = scales::squish),
    labs(color = "Expression\nLevel")
  )

  # Use get() to retrieve the actual metadata data frame from the global environment
  metadata_name <- paste0(target_donor_id, "_human_skin_merfish_atlas_skinny_metadata")
  metadata <- get(metadata_name, envir = .GlobalEnv)

  # Find the index of the target gene
  target_gene_idx <- match(target_gene, gene_ids)
  if (is.na(target_gene_idx)) {
    stop("Gene not found in gene_ids")
  }

  # Extract the gene expression values for the target gene
  gene_expr <- expression_values[target_gene_idx, ]
  gene_expression_for_cell_id <- data.frame(cell_id = cell_ids, gene_expression = gene_expr)

  # Filter metadata for specified anatomic site
  filtered_data <- metadata %>% filter(anatomic_site == target_anatomic_site)

  # Perform a left join to add gene expression values to the metadata
  merged_data <- filtered_data %>%
    left_join(gene_expression_for_cell_id, by = c("cell_barcode" = "cell_id")) %>%
    arrange(gene_expression) 

  # Define the color vector based on the gene expression across all cell types
  color_vector <- merged_data$gene_expression

  # Plot spatial expression data
  plot <- ggplot(merged_data, aes(x = spatial_1, y = spatial_2, color = color_vector)) + plot_layers

  return(plot)
}


# ---------------------- #
# --- generate_table --- #
# ---------------------- #
# Function to generate a table with a specified number of columns from the list of abbreviations

generate_table <- function(abbreviations, columns) {
  
  rows <- ceiling(length(abbreviations) / columns)  # Calculate the number of rows
  
  # Generate the HTML table with specified style and content
  tags$table(
    style = "width: 100%; font-size: 15px;",  # Adjust font size and width
    lapply(1:rows, function(row) {  # Loop over each row
      tags$tr(
        lapply(1:columns, function(col) {  # Loop over each column in the row
          idx <- (row - 1) * columns + col  # Calculate the index of the abbreviation
          if (idx <= length(abbreviations)) {  # If index is within range of the list
            tags$td(
              style = "width: 16.66%; padding-right: 10px;",  # Define column width and spacing
              abbreviations[[idx]]  # Display the corresponding abbreviation
            )
          } else {
            tags$td("")  # Insert an empty cell if no abbreviation is available
          }
        })
      )
    })
  )
}