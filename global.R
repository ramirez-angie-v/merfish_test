### Prepare Global Variables ###

# Format of the variables:
# expression_h5_file_hs_skin_atlas: Path to the expression data H5 file.
# cell_meta_data_hs_skin_atlas: Metadata of cells including spatial coordinates and other attributes.
# gene_ids_hs_skin_atlas: List of gene identifiers from the H5 file.
# cell_ids_hs_skin_atlas: List of cell identifiers from the H5 file.  

# Flag to determine whether to load data from AWS S3 Bucket
load_data_from_s3 = FALSE

expression_h5_file_hs_skin_atlas <- "./data/ns-atlas.merfish.annotated.skinny_updated.h5"
cell_ids <- as.character(h5read(expression_h5_file_hs_skin_atlas, "/cell_ids", s3 = load_data_from_s3))
expression_values <- h5read(expression_h5_file_hs_skin_atlas, "/expr", s3 = load_data_from_s3)
gene_ids <- as.character(h5read(expression_h5_file_hs_skin_atlas, "/gene_ids", s3 = load_data_from_s3))

# Load metadata for individual patients from FST files
D077_human_skin_merfish_atlas_skinny_metadata <- fst::read_fst("./data/D077/D077_human_skin_merfish_atlas_skinny_metadata.fst") 
D082_human_skin_merfish_atlas_skinny_metadata <- fst::read_fst("./data/D082/D082_human_skin_merfish_atlas_skinny_metadata.fst")
D107_human_skin_merfish_atlas_skinny_metadata <- fst::read_fst("./data/D107/D107_human_skin_merfish_atlas_skinny_metadata.fst") 
D145_human_skin_merfish_atlas_skinny_metadata <- fst::read_fst("./data/D145/D145_human_skin_merfish_atlas_skinny_metadata.fst") 
D149_human_skin_merfish_atlas_skinny_metadata <- fst::read_fst("./data/D149/D149_human_skin_merfish_atlas_skinny_metadata.fst") 
D151_human_skin_merfish_atlas_skinny_metadata <- fst::read_fst("./data/D151/D151_human_skin_merfish_atlas_skinny_metadata.fst") 
D165_human_skin_merfish_atlas_skinny_metadata <- fst::read_fst("./data/D165/D165_human_skin_merfish_atlas_skinny_metadata.fst") 

# Patient ID list that will be used for dropdown options
patient_list <- sort(c("D077", "D082", "D107", "D145", "D149", "D151", "D165"))

# List cell type levels that will be used for drop down options and reference chosen level in create_legend_plot() and plot_spatial_data()
level_list <- c("Neighborhood" = "neighborhood",
            "Level 1: Cell Category" = "l1.cell_type",
            "Level 2: Broad Cell Type" = "l2.cell_type",
            "Level 3: Broad Cell Type - Refined" = "l3.cell_type",
            "Level 4: Detailed Cell Type" = "l4.cell_type")

# List of abbreviations displayed at the bottom of the app
abbreviations <- c(
  "Adipo: Adipocyte", "Bas: Basal", "Cyc: Cycling", "DC: Dendritic Cell", 
  "Diff: Differentiated", "DP: Dermal Papilla", "DS: Dermal Sheath", 
  "Ecc: Eccrine", "EC: Endothelial Cell", "Fib: Fibroblast", 
  "HEC: High Endothelial Venules", "HFE: Hair Follicle Epithelia", 
  "HS: Hair Shaft", "Imm: Immune", "Inf: Infindibulum", 
  "IRS: Inner Root Sheath", "LC: Langerhans Cell", "LEC: Lymphatic Endothelial Cell", 
  "Lym: Lymphocyte", "Mac: Macrophage", "Melano: Melanocyte", 
  "ORS: Outer Root Sheath", "Papil: Papillary", "Peri: Pericyte", 
  "Perivasc: Perivascular", "Retic: Reticular", "Seb: Sebaceous", 
  "SM: Smooth Muscle", "Spn: Spinous", "Tc: T Cytotoxic", 
  "Th: T Helper", "Treg: T Regulatory", "VEC: Vascular Endothelial Cell"
)

# ---- Color palettes for cell types and gene expression ---- #

# Neighborhood 
neighborhood_color_pal <- c(
  "Early Diff Epithelia" = "firebrick",  
  "Late Diff Epithelia" = "#FF1493",     
  "Basement Membrane" = "tomato",        
  "Upper Immune II" = "lightcoral",     
  "Lower Immune" = "mediumorchid",       
  "Lower Stroma" = "orchid",             
  "Perifollicular" = "mediumpurple1",    
  "Upper Immune I" = "#8A2BE2",         
  "Lymphatic" = "cyan3",                
  "Eccrine" = "navy",                    
  "Subcutis" = "deepskyblue",            
  "Pilosebaceous" = "darkturquoise"      
)

# Level 1: Cell Category
l1_cell_type_color_pal <- c(
  "Epithelia" = "firebrick",    
  "Immune" = "limegreen",      
  "Stroma" = "dodgerblue3",    
  "Fibroblast" = "cyan3",       
  "Doublet" = "gold"            
)


# Level 2: Broad Cell Type
l2_cell_type_color_pal <- c(
  ## Epithelia
  "Grn KC" = "firebrick", 
  "Spn KC" = "#FF1493",  
  "Bas KC" = "tomato", 
  "Cyc KC" = "lightcoral",
  "HFE" = 'mediumorchid', 
  "Seb" = "orchid",
  "Ecc Duct" = "mediumpurple1", 
  "Ecc Gland"= "#8A2BE2",  
  ## Fib
  "Fib" = "cyan3", 
  "DP/DS" = "navy",
  ## Stroma
  "Schwann" = "black",  
  "SM" =  "deepskyblue", 
  "Peri" =  "darkturquoise",
  "EC" = "dodgerblue3", 
  "Adipo" = 'darkslategray',  
  "LEC" = "#006d2c",  
  "Mast" = "limegreen", 
  "Mac" = "#D81A5FFF", 
  "DC" = "#157a1b",
  "CD8+ Lym" = "#ff7400", 
  "CD4+ Lym" = "#ffa700"
)


# Level 3: Broad Cell Type - Refined
l3_cell_type_color_pal <- c(
  "Grn KC" = "firebrick",      
  "Spn KC" = "#FF1493",         
  "LC" = "#bb2679",           
  "Melano" = "#6030c9",   
  "Cyc KC" = "lightcoral",      
  "Bas KC" = "tomato",          
  "EC" = "dodgerblue3",         
  "Mast" = "limegreen",         
  "Papil Fib" = "navy",         
  "Adipo" = "darkslategray",    
  "CD4+ Treg" = "#ffa700",     
  "Schwann" = "black",          
  "Doublet" = "gold",           
  "Seb" = "orchid",             
  "Perivasc Fib" = "lightseagreen",  
  "DC" = "#157a1b",            
  "Mac" = "#D81A5FFF",        
  "CD4+ Th" = "#c98911",        
  "Retic Fib" = "cyan3",        
  "HFE" = "mediumorchid",       
  "Plasma" = "lightblue",       
  "DP/DS" = "navy",             
  "CD8+ Tc" = "#ff7400",      
  "Peri" = "darkturquoise",     
  "Ecc Duct" = "mediumpurple1",
  "Ecc Gland" = "#8A2BE2",    
  "LEC" = "#006d2c",            
  "SM" = "deepskyblue",         
  "Cyc Imm" = "salmon"          
)

# Level 4: Detailed Cell Type
l4_cell_type_color_pal <- c(
  "Grn KC II" = "firebrick",      
  "Grn KC I" = "#A52A2A",        
  "Spn KC I" = "#FF1493",        
  "LC" = "#bb2679",            
  "Melano" = "#6030c9",   
  "Cyc KC" = "lightcoral",        
  "Bas KC" = "tomato",            
  "VEC" = "dodgerblue3",          
  "Mast" = "limegreen",           
  "Papil Fib" = "#0d409b",        
  "Spn KC II" = "#FF1493",       
  "Adipo" = "darkslategray",       
  "CD4+ Treg" = "#ffa700",         
  "Schwann" = "black",           
  "Doublet" = "gold",             
  "Bas Seb" = "orchid",          
  "Perivasc Fib II" = "#20B2AA",  
  "DC" = "#157a1b",                
  "HEC" = "#00BFFF",                 
  "Mac" = "#D81A5FFF",            
  "Perivasc Fib I" = "#1b976c",  
  "CD4+ Th" = "#ffa700",          
  "Retic Fib I" = "cyan3",        
  "Inf" = "lightgreen",            
  "Retic Fib II" = "#50a0bb",      
  "Plasma" = "lightblue",          
  "Bulge/ORS Basal" = "lightseagreen",
  "ORS Suprabasal" = "#4a689d", 
  "DP/DS" = "navy",                
  "Retic Fib III" = "#43c2c0",       
  "IRS/HS" = "#590d9b",      
  "CD8+ Tc" = "#ff7400",           
  "Peri" = "darkturquoise",        
  "Ecc Duct" = "mediumpurple1",    
  "Ecc Gland" = "#8A2BE2",        
  "Diff Seb" = "orchid",           
  "LEC" = "#006d2c",               
  "SM" = "deepskyblue",         
  "Cyc Imm" = "salmon"            
)


expression_color_palette <- c("lightgray", "blue")
# expression_color_palette <- viridisLite::magma(100)  # 100 colors from the magma palette
