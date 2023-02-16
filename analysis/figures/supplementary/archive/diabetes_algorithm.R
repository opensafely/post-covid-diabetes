# GENERATE DIABETES FLOWCHART

# AUTHOR: KURT TAYLOR - kurt.taylor@bristol.ac.uk

# INSTRUCTIONS: 1) Load libraries (install first if necessary), 2) change the results and output directories, 3) change outcomes (should be suffix to CSV name)

# LIBRARIES ---------------------------------------------------------------

libraries <- c("readr", "dplyr", "stringr", "tidyverse", "DiagrammeR", "DiagrammeRsvg", "rsvg", "plyr")
lapply(libraries, require, character.only=T)

# DATA --------------------------------------------------------------------

results_dir <- paste0("/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/three-cohort-results-v2/descriptive/")
output_dir <- paste0("/Users/kt17109/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-diabetes/three-cohort-results-v2/generated-figures/")

  # BUILD FLOW --------------------------------------------------------------
  
  flow <- DiagrammeR::grViz("
digraph graph2 {

graph [layout = dot]

# NODE DEFINITIONS AND SUBSTITUTED TEXT

node [shape = rectangle, width = 4, fillcolor = Biege]
a [label = '@@1', fontname = 'Arial Rounded MT']
b [label = '@@2', fontname = 'Arial Rounded MT']
c [label = '@@3', fontname = 'Arial Rounded MT']
e [label = '@@5', fontname = 'Arial Rounded MT']
g [label = '@@7', fontname = 'Arial Rounded MT']
i [label = '@@9', fontname = 'Arial Rounded MT']
j [label = '@@10', fontname = 'Arial Rounded MT']
k [label = '@@11', fontname = 'Arial Rounded MT']
l [label = '@@12', fontname = 'Arial Rounded MT']
m [label = '@@13', fontname = 'Arial Rounded MT']
n [label = '@@14', fontname = 'Arial Rounded MT']
o [label = '@@15', fontname = 'Arial Rounded MT']
p [label = '@@16', fontname = 'Arial Rounded MT']
q [label = '@@17', fontname = 'Arial Rounded MT']

node [shape = oval, width = 4, style = filled]
d [label = '@@4', fillcolor = red, fontname = 'Arial Rounded MT Bold']
f [label = '@@6', fillcolor = red, fontname = 'Arial Rounded MT Bold']
h [label = '@@8', fillcolor = red, fontname = 'Arial Rounded MT Bold']
r [label = '@@18', fillcolor = red, fontname = 'Arial Rounded MT Bold']
s [label = '@@19', fillcolor = red, fontname = 'Arial Rounded MT Bold']

# DRAW FLOW CHART AND LABELS

a -> b
b -> c [label = 'Yes']
b -> e [label = 'No']
c -> d [label = 'No']
c -> e [label = 'Yes']
e -> f [label = 'Yes']
e -> g [label = 'No']
g -> h [label = 'Yes']
g -> i [label = 'No']
i -> f [label = 'Yes']
i -> j [label = 'No']
j -> h [label = 'Yes']
j -> k [label = 'No']
k -> l [label = 'Yes']
l -> h [label = 'Yes']
l -> m [label = 'No']
m -> f [label = 'Yes']
m -> n [label = 'No']
n -> h [label = 'Yes']
n -> o [label = 'No']
o -> f [label = 'Yes']
o -> p [label = 'No']
p -> f [label = 'Yes']
p -> h [label = 'No']
k -> q [label = 'No']
q -> r [label = 'Yes']
q -> s [label = 'No']

}

# WRITE LABELS TO BE USED ABOVE

[1]: paste0('Study population')
[2]: paste0('1. Any gestational diabetes code')
[3]: paste0('1a. Any type 1 / type 2 codes')
[4]: paste0('Gestational diabetes')
[5]: paste0('2. Non-metformin oral anti-diabetic')
[6]: paste0('Type 2 diabetes')
[7]: paste0('3. Type 1 code and no Type 2 code')
[8]: paste0('Type 1 diabetes')
[9]: paste0('4. Type 2 code and no Type 1 code')
[10]: paste0('5. Aged < 35 yrs (<30 yrs if South Asian / African) at first diagnostic code')
[11]: paste0('6. Type 1 and Type 2 codes present')
[12]: paste0('6a. Type 1 only recorded in primary care')
[13]: paste0('6b. Type 2 only recorded in primary care')
[14]: paste0('6c. N Type 1 > N Type 2 codes')
[15]: paste0('6d. N Type 2 > N Type 1 codes')
[16]: paste0('6e. Type 2 code most recent')
[17]: paste0('7. Diabetes medication OR >= 5 process codes OR HbA1c >= 47.5mmol')
[18]: paste0('Other or non-specified diabetes')
[19]: paste0('Diabetes unlikely')
")
  
  ###############################################
  # 2. Output  -------------------- #
  ###############################################
  
  flow %>%
    export_svg() %>%
    charToRaw %>%
    rsvg_png(paste0(output_dir,"diabetes_flow_algorithm.png"))
  
# END