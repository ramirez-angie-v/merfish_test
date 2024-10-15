library(shiny)
library(shinythemes)
library(shinyBS)
library(htmltools)
library(fst)
library(rhdf5)
library(dplyr)
library(rlang)
library(scales)
library(ggplot2)

source("global.R")
source("helper.R")


######################### User Interface (UI) of the Shiny app ######################## 

ui <- navbarPage(
  tags$head(
    # Custom CSS for the app
    tags$link(rel = "stylesheet", type = "text/css", href = "jilab_merfish.css"),
    # FontAwesome library for icons
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css")
  ),
  # ---- Script to modify the header with logos and external links --- #
  tags$script(HTML("
    var header = $('.navbar > .container-fluid');
    header.append('<div class=\"header-right\">' + 
      '<a href=\"https://labs.icahn.mssm.edu/jilab/\" target=\"_blank\">' +
      '<img src=\"ji_lab.png\" alt=\"Ji Lab logo\" class=\"header-img-jilab\"></a>' + 
      '<a href=\"https://bings.mssm.edu/\" target=\"_blank\">' +
      '<img src=\"bings_logo_white.png\" alt=\"Bings logo\" class=\"header-img-bings\"></a>' +
      '<a href=\"https://icahn.mssm.edu/research/skin-biology-disease-resource-center\" target=\"_blank\">' +
      '<img src=\"sbdrc_logo_white_merged.png\" alt=\"SBDRC logo\" class=\"header-img-sbdrc\"></a></div>'
    );
    console.log(header);
  ")
  ),
  title = "Human Skin MERFISH Atlas", 
  theme = shinytheme("yeti"),  # Apply a theme from shinythemes
  
  # ---- Define the content of the "Home" tab panel ---- #
  tabPanel("Home",
           fluidPage(
             mainPanel(width = 12,  # Main content with full width
                       h3("The spatially-resolved single-cell anatomic heterogeneity of the human skin microenvironment", class = "study-summary-header"),
                       p("The spatial organization of human skin underlies its many critical functions, including barrier establishment and immune surveillance. While physiological differences in the skin across anatomic sites are well documented, the underlying molecular programs and cell communication networks that underlie this patterning are not well understood. When these pathways go awry, disease often ensues. To better understand tissue-intrinsic anatomic heterogeneity in the skin microenvironment, we applied single-cell resolution spatial transcriptomics profiling via multiplexed error robust fluorescence in situ hybridization (MERFISH) to comprehensively localize and profile over 889k cells from 114 samples, establishing a comprehensive resource for the community.", class = "study-summary"), 
                       br(),
                       # ----  Display graphical abstract and level descriptions side by side ---- #
                       fluidRow(
                         column(8,
                                style = "text-align: center;",
                                div(
                                  tags$img(src = "graphical-abstract.png", 
                                           style = "max-width: 100%; max-height: 100%; height: auto; width: 1250px; display: block; margin: 0 auto;"
                                  )
                                ),
                         ),
                         column(4,
                                h3("Level descriptions"),
                                p(HTML("
                                        <strong>Neighborhood</strong> - Multicellular spatial domains identified via cellcharter<br>
                                        <strong>Level 1: Cell Category</strong> - Broad cellular compartment<br>
                                        <strong>Level 2: Broad Cell Type</strong> - Broad cell type<br>
                                        <strong>Level 3: Broad Cell Type - Refined</strong> - Broad cell type annotations refined after subclustering<br>
                                        <strong>Level 4: Detailed Cell Type</strong> - Granular cell type annotations identified via subclustering
                                      "))
                         )
                       ),
                       br(),
                       br(),
                       
                       # ---- Control panel for selecting patient, level, cell type, and gene symbol ---- #
                       fluidRow(
                         column(width = 1),
                         column(width = 10, class = "controls-row",
                                div(
                                  column(3,
                                         # Tooltip for patient ID selection
                                         helpText(
                                           strong("Select or type patient ID to explore the data"),
                                           tags$span(
                                             tags$i(class = "fa fa-question-circle", style = "color: #007BFF; cursor: pointer; margin-left: 5px;"),
                                             id = "tooltip_patient"
                                           )
                                         ),
                                         selectizeInput(
                                           "patient_id",
                                           label = "Patient ID:",
                                           choices = as.list(patient_list),
                                           selected = NULL,
                                           multiple = FALSE,
                                           options = list(maxItems = 1, placeholder = 'Select patient')
                                         ),
                                         # Tooltip text that shows when user hovers over the question mark icon for patient ID
                                         bsTooltip("tooltip_patient", "Selecting a different patient will keep all previously selected options", placement = "right", trigger = "hover")
                                  ),
                                  column(3,
                                         # Tooltip for level selection
                                         helpText(
                                           strong("Select or type neighborhood or level to explore the data"),
                                           tags$span(
                                             tags$i(class = "fa fa-question-circle", style = "color: #007BFF; cursor: pointer; margin-left: 5px;"),
                                             id = "tooltip_level"
                                           )
                                         ),
                                         selectizeInput(
                                           "level_selection",
                                           label = "Level:",
                                           choices = as.list(level_list),
                                           selected = NULL,
                                           multiple = FALSE,
                                           options = list(maxItems = 1, placeholder = 'Select level')
                                         ),
                                         # Tooltip text that shows when user hovers over the question mark icon for level selection
                                         bsTooltip("tooltip_level", "Each level will show different options for \"Cell Type\"", placement = "right", trigger = "hover")
                                  ),
                                  column(3,
                                         # Tooltip for cell type selection
                                         helpText(
                                           strong("Select or type cell type to explore the data"),
                                           tags$span(
                                             tags$i(class = "fa fa-question-circle", style = "color: #007BFF; cursor: pointer; margin-left: 5px;"),
                                             id = "tooltip_cell_type"
                                           )
                                         ),
                                         selectizeInput(
                                           "cell_type",
                                           label = "Cell Type:",
                                           choices = NULL,
                                           selected = "All",
                                           multiple = FALSE,
                                           options = list(maxItems = 1, placeholder = 'Select cell type')
                                         ),
                                         # Tooltip text that shows when user hovers over the question mark icon for cell type selection
                                         bsTooltip("tooltip_cell_type", "Cell type entries will change according to the selected \"Level\"", placement = "right", trigger = "hover")
                                  ),
                                  column(3,
                                         # Tooltip for gene symbol selection
                                         helpText(
                                           strong("Select or type Gene Symbol to explore the data"),
                                           tags$span(
                                             tags$i(class = "fa fa-question-circle", style = "color: #007BFF; cursor: pointer; margin-left: 5px;"),
                                             id = "tooltip_gene"
                                           )
                                         ),
                                         selectizeInput(
                                           "gene_identifier",
                                           label = "Gene Symbol:",
                                           choices = sort(as.character(gene_ids)),  # Use sort() to sort gene_ids
                                           selected = NULL,
                                           multiple = FALSE,
                                           options = list(maxItems = 1, placeholder = 'Enter Gene Symbol')
                                         ),
                                         # Tooltip text that shows when user hovers over the question mark icon for gene symbol
                                         bsTooltip("tooltip_gene", "Search for a gene to view its expression across all cells", placement = "right", trigger = "hover")
                                  )
                                )
                         ),
                         column(width = 1)
                       ),
                       br(),
                       uiOutput("patient_id_title"),
                       
                       # ------------------- Spatial Plots ------------------- #
                       fluidRow(h3("MERFISH Spatial Plots", style = "border-bottom: 1px solid gray;")),
                       br(),
                       fluidRow(plotOutput("spatial_plot_legend", height = "125px")),
                       fluidRow(class = "first_row_spatial_clustering_t1",
                                column(width = 2, 
                                       h4("Antecubital Fossa"),
                                       plotOutput("antecubital_fossa_cluster_plot_t1", height = "400px"),
                                ),
                                column(width = 2, 
                                       h4("Postauricular"),
                                       plotOutput("postauricular_cluster_plot_t1", height = "400px"),
                                ),
                                column(width = 1, 
                                       h4("Central Scalp"),
                                       plotOutput("central_scalp_plot_t1", height = "400px"),
                                ),
                                column(width = 2,
                                       img(class = "collection-strategy-figure", src ="FRONT.png")
                                ),
                                column(width = 2, 
                                       h4("Occipital Scalp"),
                                       plotOutput("occipital_scalp_plot_t1", height = "400px"),
                                ),
                                column(width = 1, 
                                       h4("Knee"),
                                       plotOutput("knee_plot_t1", height = "400px"),
                                ),
                                column(width = 2, 
                                       h4("Abdomen"),
                                       plotOutput("abdomen_plot_t1", height = "400px")
                                )
                       ),
                       br(),
                       fluidRow(class = "second_row_spatial_clustering_t1",
                                column(width = 2, 
                                       h4("Elbow"),
                                       plotOutput("elbow_cluster_plot_t1", height = "400px"),
                                ),
                                column(width = 1, 
                                       h4("Inguinal Fold"),
                                       plotOutput("inguinal_fold_cluster_plot_t1", height = "400px"),
                                ),
                                column(width = 2, 
                                       h4("Back"),
                                       plotOutput("back_cluster_plot_t1", height = "400px"),
                                ),
                                column(width = 2,
                                       img(class = "collection-strategy-figure", src ="BACK.png")
                                ),
                                column(width = 1, 
                                       h4("Buttocks"),
                                       plotOutput("buttocks_cluster_plot_t1", height = "400px"),
                                ),
                                column(width = 2, 
                                       h4("Popliteal Fossa"),
                                       plotOutput("popliteal_fossa_cluster_plot_t1", height = "400px"),
                                ),
                                column(width = 2, 
                                       h4("Sole"),
                                       plotOutput("sole_cluster_plot_t1", height = "400px"),
                                )
                       ),

                       # ------------------- Expression Plots ------------------- #
                       fluidRow(h3("Gene Expression Plots", style = "border-bottom: 1px solid gray;",
                                   # Tooltip for expression plots section
                                   tags$span(
                                     tags$i(class = "fa fa-question-circle", 
                                            style = "color: #007BFF; cursor: pointer; margin-left: 5px; font-size: 15px;"),
                                            id = "tooltip_gene_expr_plot")
                                   ),
                                #Expression plot tooltip when using the minimum and maximum values to scale the legend 
                                bsTooltip("tooltip_gene_expr_plot", "Plots display gene expression levels with color scaled between the minimum and maximum value of the selected patient, ordered from low to high", placement = "right", trigger = "hover")
                                
                                # Expression plot tooltip when using the 1st and 99th percentile to scale the legend
                                # bsTooltip("tooltip_gene_expr_plot", "Plots display gene expression levels, with color scaled between the 1st percentile (lower boundary) and 99th percentile (upper boundary) of the selected patient, ordered from low to high", placement = "right", trigger = "hover")
                       ),
                       br(),
                       fluidRow(
                         column(width = 4),
                         column(width = 4,
                                plotOutput("shared_legend_plot", height = "67px")
                         ),
                         column(width = 4)
                       ),
                       br(),
                       fluidRow(class = "first_row_spatial_expression_t1",
                                column(width = 2,
                                       h4("Antecubital Fossa"),
                                       plotOutput("antecubital_fossa_expression_plot_t1", height = "400px")
                                ),
                                column(width = 2, 
                                       h4("Postauricular"),
                                       plotOutput("postauricular_expression_plot_t1", height = "400px")
                                ),
                                column(width = 1, 
                                       h4("Central Scalp"),
                                       plotOutput("central_scalp_expression_plot_t1", height = "400px")
                                ),
                                column(width = 2,
                                       img(class = "collection-strategy-figure", src ="FRONT.png")
                                ),
                                column(width = 2, 
                                       h4("Occipital Scalp"),
                                       plotOutput("occipital_scalp_expression_plot_t1", height = "400px")
                                ),
                                column(width = 1, 
                                       h4("Knee"),
                                       plotOutput("knee_expression_plot_t1", height = "400px")
                                ),
                                column(width = 2, 
                                       h4("Abdomen"),
                                       plotOutput("abdomen_expression_plot_t1", height = "400px")
                                )
                       ),
                       br(),
                       fluidRow(class = "second_row_spatial_expression_t1",
                                column(width = 2, 
                                       h4("Elbow"),
                                       plotOutput("elbow_expression_plot_t1", height = "400px")
                                ),
                                column(width = 1, 
                                       h4("Inguinal Fold"),
                                       plotOutput("inguinal_fold_expression_plot_t1", height = "400px")
                                ),
                                column(width = 2, 
                                       h4("Back"),
                                       plotOutput("back_expression_plot_t1", height = "400px")
                                ),
                                column(width = 2,
                                       img(class = "collection-strategy-figure", src ="BACK.png")
                                ),
                                column(width = 1, 
                                       h4("Buttocks"),
                                       plotOutput("buttocks_expression_plot_t1", height = "400px")
                                ),
                                column(width = 2, 
                                       h4("Popliteal Fossa"),
                                       plotOutput("popliteal_fossa_expression_plot_t1", height = "400px")
                                ),
                                column(width = 2, 
                                       h4("Sole"),
                                       plotOutput("sole_expression_plot_t1", height = "400px")
                                ),
                                br(),
                       ),
                       h3("List of abbreviations", style = "border-bottom: 1px solid gray;"),
                       p(generate_table(abbreviations, columns = 4)),
                       br(),
                       fluidRow(div(class = "footer", "Developed by Angie Ramirez (BiNGS)"))
             ),
           )
  ),
  
  # ---- Define the content of the "Compare" tab panel ---- #
  tabPanel("Compare",
           fluidPage(
             titlePanel("Side-by-side Patient Comparison"),
             br(),
             fluidRow(
               p(class = "tab2-text", "A new way to explore the data is in development and coming soon!")
             )
           )
  )
)

######################## Server-side logic of the Shiny app ######################## 

server <- function(input, output, session) {

  # Update the displayed patient title based on the selected patient ID
  observeEvent(input$patient_id, {
    output$patient_id_title <- renderUI({
      if (!is.null(input$patient_id) && input$patient_id != "") {
        h3(paste0("Patient ID: ", input$patient_id), style = "text-align: center; font-weight: 500;")
      } else {
        h3("No patient selected", style = "text-align: center;")
      }
    })
  })
  
  # Create a reactive expression for the color palette based on selected level
  cell_type_color_palette <- reactive({
    selected_level <- input$level_selection
    switch(selected_level,
           "neighborhood" = neighborhood_color_pal,
           "l1.cell_type" = l1_cell_type_color_pal,
           "l2.cell_type" = l2_cell_type_color_pal,
           "l3.cell_type" = l3_cell_type_color_pal,
           "l4.cell_type" = l4_cell_type_color_pal,
           stop("Invalid level selected")
    )
  })
  
  # Store the current level and cell type selection as a reactive value
  level_and_cell_type_selection <- reactiveVal(c(NULL, NULL))
  
  # Update cell type options when the level selection changes
  observeEvent(input$level_selection, {
    level_and_cell_type_selection(c(input$level_selection, "All"))
    updateSelectizeInput(session, "cell_type",
                         choices = c("All", sort(as.character(names(cell_type_color_palette())))),
                         selected = "All",
                         options = list(maxItems = 1, placeholder = 'Select cell type'))
    })
  
  # Update the selected cell type when the user changes the input
  observeEvent(input$cell_type, {
      level_and_cell_type_selection(c(level_and_cell_type_selection()[1], input$cell_type))
  })
  
  # ------------------- Spatial Plots ------------------- #
  
  # --- Render the spatial plots based on the selected patient, level, and cell type --- #

    observeEvent(c(input$level_selection, input$cell_type), {
    # Render the spatial plot legend based on the current selection
    output$spatial_plot_legend <- renderPlot({
      create_spatial_plot_legend(
        target_level = input$level_selection,
        target_cell_type = input$cell_type,
        color_palette = cell_type_color_palette()
      )
    })
  })

  # Observe the patient selection and load relevant data
  observeEvent(input$patient_id, {
    
      output$antecubital_fossa_cluster_plot_t1 <- renderPlot({
      plot_spatial_data("antecubital fossa", level_and_cell_type_selection()[1], level_and_cell_type_selection()[2], input$patient_id, cell_type_color_palette())
    })
    
    output$postauricular_cluster_plot_t1 <- renderPlot({
      plot_spatial_data("postauricular", level_and_cell_type_selection()[1], level_and_cell_type_selection()[2], input$patient_id, cell_type_color_palette())
    })
    
    output$central_scalp_plot_t1 <- renderPlot({
      plot_spatial_data("central scalp", level_and_cell_type_selection()[1], level_and_cell_type_selection()[2], input$patient_id, cell_type_color_palette())
    })
    
    output$occipital_scalp_plot_t1 <- renderPlot({
      plot_spatial_data("occipital scalp", level_and_cell_type_selection()[1], level_and_cell_type_selection()[2], input$patient_id, cell_type_color_palette())
    })
    
    output$knee_plot_t1 <- renderPlot({
      plot_spatial_data("knee", level_and_cell_type_selection()[1], level_and_cell_type_selection()[2], input$patient_id, cell_type_color_palette())
    })
    
    output$abdomen_plot_t1 <- renderPlot({
      plot_spatial_data("abdomen", level_and_cell_type_selection()[1], level_and_cell_type_selection()[2], input$patient_id, cell_type_color_palette())
    })
    
    # -- SECOND ROW -- #
    
    output$elbow_cluster_plot_t1 <- renderPlot({
      plot_spatial_data("elbow", level_and_cell_type_selection()[1], level_and_cell_type_selection()[2], input$patient_id, cell_type_color_palette())
    })
    
    output$inguinal_fold_cluster_plot_t1 <- renderPlot({
      plot_spatial_data("inguinal fold", level_and_cell_type_selection()[1], level_and_cell_type_selection()[2], input$patient_id, cell_type_color_palette())
    })
    
    output$back_cluster_plot_t1 <- renderPlot({
      plot_spatial_data("back", level_and_cell_type_selection()[1], level_and_cell_type_selection()[2], input$patient_id, cell_type_color_palette())
    })
    
    output$buttocks_cluster_plot_t1 <- renderPlot({
      plot_spatial_data("buttocks", level_and_cell_type_selection()[1], level_and_cell_type_selection()[2], input$patient_id, cell_type_color_palette())
    })
    
    output$popliteal_fossa_cluster_plot_t1 <- renderPlot({
      plot_spatial_data("popliteal fossae", level_and_cell_type_selection()[1], level_and_cell_type_selection()[2], input$patient_id, cell_type_color_palette())
    })
    
    output$sole_cluster_plot_t1 <- renderPlot({
      plot_spatial_data("sole", level_and_cell_type_selection()[1], level_and_cell_type_selection()[2], input$patient_id, cell_type_color_palette()) 
    })
  })
  
  # ------------------- Expression Plots ------------------- #

  # reactive value to store global min and max
  patient_gene_expression_range <- reactiveValues(min = NULL, max = NULL)

  # Observe gene and patient selection to update the expression plots
  observeEvent(c(input$gene_identifier, input$patient_id), {

    # Only proceed if both gene and patient are selected
    if (!is.null(input$gene_identifier) && !is.null(input$patient_id)) {

      # Get the metadata for the selected patient
      patient_metadata_name <- paste0(input$patient_id, "_human_skin_merfish_atlas_skinny_metadata")
      patient_metadata <- get(patient_metadata_name, envir = .GlobalEnv)

      # Find the indices of the cells belonging to the selected patient in cell_ids
      patient_cell_indices <- which(cell_ids %in% patient_metadata$cell_barcode)

      # Calculate the gene expression values for the selected gene for those patient cells
      target_gene_idx <- match(input$gene_identifier, gene_ids)

      if (!is.na(target_gene_idx)) {
        
        # Filter for patient cells
        gene_expr <- expression_values[target_gene_idx, patient_cell_indices] 
        
        # Store the patient min and max expression values of the selected gene 
        # (comment out if using 1st and 99th percentile cutoffs)
        patient_gene_expression_range$min <- min(gene_expr, na.rm = TRUE)
        patient_gene_expression_range$max <- max(gene_expr, na.rm = TRUE)

        # Un-comment section below (along with the appropriate tooltip text) to calculate the 1st and 99th percentiles of the patient gene expression values
        # and use them to generate the expression plots' legend

        # if (sum(gene_expr) != 0) {  # Ensure there are non-zero values
        #   patient_gene_expression_range$min <- quantile(gene_expr[gene_expr > 0], probs = 0.01)  # 1st percentile
        #   patient_gene_expression_range$max <- quantile(gene_expr[gene_expr > 0], probs = 0.99)  # 99th percentile
        # } else {
        #   # Use original min and max if not > 0
        #   patient_gene_expression_range$min <- min(gene_expr, na.rm = TRUE)
        #   patient_gene_expression_range$max <- max(gene_expr, na.rm = TRUE)
      }
    }

    # Render the expression plot legend based on the current selection
    output$shared_legend_plot <- renderPlot({
      create_expression_plot_legend(patient_gene_expression_range, expression_color_palette)
    })

    # ---- Render individual gene expression plots for different body regions ---- #
    
    output$antecubital_fossa_expression_plot_t1 <- renderPlot({
      plot_expression_data("antecubital fossa", input$gene_identifier, input$patient_id, expression_color_palette, patient_gene_expression_range)
    })
    
    output$postauricular_expression_plot_t1 <- renderPlot({
      plot_expression_data("postauricular", input$gene_identifier, input$patient_id, expression_color_palette, patient_gene_expression_range)
    })
    
    output$central_scalp_expression_plot_t1 <- renderPlot({
      plot_expression_data("central scalp", input$gene_identifier, input$patient_id, expression_color_palette, patient_gene_expression_range)
    })
    
    output$occipital_scalp_expression_plot_t1 <- renderPlot({
      plot_expression_data("occipital scalp", input$gene_identifier, input$patient_id, expression_color_palette, patient_gene_expression_range)
    })
    
    output$knee_expression_plot_t1 <- renderPlot({
      plot_expression_data("knee", input$gene_identifier, input$patient_id, expression_color_palette, patient_gene_expression_range)
    })
    
    output$abdomen_expression_plot_t1 <- renderPlot({
      plot_expression_data("abdomen", input$gene_identifier, input$patient_id, expression_color_palette, patient_gene_expression_range)
    })
    
    # -- SECOND ROW -- #
    
    output$elbow_expression_plot_t1 <- renderPlot({
      plot_expression_data("elbow", input$gene_identifier, input$patient_id, expression_color_palette, patient_gene_expression_range)
    })
    
    output$inguinal_fold_expression_plot_t1 <- renderPlot({
      plot_expression_data("inguinal fold", input$gene_identifier, input$patient_id, expression_color_palette, patient_gene_expression_range)
    })
    
    output$back_expression_plot_t1 <- renderPlot({
      plot_expression_data("back", input$gene_identifier, input$patient_id, expression_color_palette, patient_gene_expression_range)
    })
    
    output$buttocks_expression_plot_t1 <- renderPlot({
      plot_expression_data("buttocks", input$gene_identifier, input$patient_id, expression_color_palette, patient_gene_expression_range)
    })
    
    output$popliteal_fossa_expression_plot_t1 <- renderPlot({
      plot_expression_data("popliteal fossae", input$gene_identifier, input$patient_id, expression_color_palette, patient_gene_expression_range)
    })
    
    output$sole_expression_plot_t1 <- renderPlot({
      plot_expression_data("sole", input$gene_identifier, input$patient_id, expression_color_palette, patient_gene_expression_range)
    })
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)

