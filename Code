library(tidytuesdayR)
library(tidyverse)
library(gtExtras)
library(gtable)
library(gt)
library(gtsummary)
library(dplyr)
tuesdata <- tidytuesdayR::tt_load(2021, week = 40)
papers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/papers.csv')
authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/authors.csv')
programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/programs.csv')
paper_authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_authors.csv')
paper_programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_programs.csv')

left_join(papers, paper_authors) %>% 
  left_join(authors) %>% 
  left_join(paper_programs) %>% 
  left_join(programs)%>% 
  mutate(
    catalogue_group = str_sub(paper, 1, 1),
    catalogue_group = case_when(
      catalogue_group == "h" ~ "Historical",
      catalogue_group == "t" ~ "Technical",
      catalogue_group == "w" ~ "General"
    ),
    .after = paper
  )->paper1

paper1%>%drop_na()%>%filter(year>=2020)%>%
  distinct(paper,.keep_all = TRUE)%>%
  select(catalogue_group,year,title,program_desc)%>%
  group_by(program_desc, year)%>%
  count()%>%
  pivot_wider(names_from = year,values_from = n)%>%
  mutate(across(everything(), .fns = ~replace_na(.,0)))->paper2

data.frame(paper2)->paper2
paper2%>%
  arrange(program_desc)->paper2
  
paper2%>%rowwise()%>%mutate(TOTAL=sum(c_across(("2020":"2021"))))->paper2                         
paper2[c(1,3,4,2)]->paper2
paper2%>%arrange(-TOTAL)->paper2

paper1%>%drop_na()%>%filter(year>=2020)%>%
  distinct(paper,.keep_all = TRUE)%>%
  select(catalogue_group,year,title,program_desc)%>%
  group_by(program_desc, year)%>%
  arrange(year)%>%
  count()%>%
  group_by(program_desc)%>% 
  mutate(total=sum(n))%>%
  arrange(year, program_desc)%>%
  mutate(spark=list(n))%>%
  select(program_desc, spark, total) %>%
  distinct()->paper6
  
paper2
paper2%>%inner_join(paper6,by="program_desc")%>%
  arrange(desc(total))->paper7

paper7[c(1,5,2,3,4)]->paper7

colnames(paper7)<-c("PROGRAM","TOTAL","2020","2021","TREND")

source_tag <- "Data: <a href='https://www.nber.org/'>NBER</a> via TidyTuesday| Design and Analysis: @annapurani93"


paper7%>%
  gt()%>%
  gt_sparkline(TREND,
               type = "sparkline",
               line_color = "white",
               range_colors = c("#fe8a71","#3da4ab"),
               fill_color = "#f6cd61")%>%
  tab_spanner(
    label = "NUMBER OF PAPERS",
    columns = c(TOTAL, `2020` , `2021`,TREND)
  )%>%
  gt_plt_dot(TOTAL,PROGRAM,palette = "#3da4ab")%>%
  cols_width(PROGRAM~px(380))%>%
  gt_color_box(columns = `2020`, domain = 0:240, palette = c("#fe8a71","#f6cd61","#3da4ab"))%>%
  gt_color_box(columns = `2021`, domain = 0:240, palette = c("#fe8a71","#f6cd61","#3da4ab"))%>%
  tab_header(
    title = md("**PROMINENT PROGRAMS DURING THE PANDEMIC AT THE NBER**"),
    subtitle = "2066 papers on 20 different topics have been published between 2020 and 2021 (thus far) at the National Bureau of Economic Research. 
    And, of the 20 topics, Economic Fluctions and Growth, Asset Pricing are the subjects that saw the highest number of working papers during this pandemic-hit period. Here's an overview"
  )%>%
  tab_source_note(md(html(source_tag)))%>%
  tab_style(
    style = list(
      cell_text(
        align = "right",
        color = "#fe8a71"
      )
    ),
    locations = cells_source_notes()
  )%>%
  cols_align(
    align = "center",
    columns = everything())%>% 
  tab_style(style = cell_text(weight="bold"),
            locations = cells_column_labels(everything())
  )%>%
  tab_style(
    style = cell_text(align="center"),
    locations = cells_body(
      columns = everything(),
      rows = everything()
    )
  )%>%
  opt_table_lines("all")%>%
  opt_table_outline()%>%
  opt_row_striping()%>%
  tab_style(
    style = list(
      cell_borders(
        sides = c("top", "bottom"),
        color = "#989898",
        weight = px(0.2),
        style="dotted"
      ),
      cell_borders(
        sides = c("left", "right"),
        color = "#989898",
        weight = px(0.2),
        style="dotted"
      )),
    locations = cells_column_labels(
      columns = everything()
    )
  )%>%
  tab_style(
    style = list(
      cell_borders(
        sides = c("top", "bottom"),
        color = "#989898",
        weight = px(1),
        style="dashed"
      ),
      cell_borders(
        sides = c("left", "right"),
        color = "#989898",
        weight = px(1),
        style="dashed"
      )),
    locations = cells_body(
      columns = everything(),
      rows = everything()
    )
  )%>%
  tab_style(
    style = list(
      cell_text(
        color = "#2abdc7",
        transform = "uppercase"
      )
    ),
    locations = list(
      cells_title(groups = "title")
    )
  ) %>%
  # Adjust sub-title font
  tab_style(
    style = list(
      cell_text(
        color="#fe8a71"
      )
    ),
    locations = list(
      cells_title(groups = "subtitle")
    )
  )%>%
  tab_style(
    style = cell_text(weight = "bold", align="center",color = "#2abdc7"),
    locations = cells_body(
      columns = TOTAL,
      rows = everything()
    )
  )%>%
  cols_width(
    vars(TOTAL) ~ px(100),
    vars(`2020`) ~ px(100),
    vars(`2021`) ~ px(100),
    vars(TREND) ~ px(100)
  )%>%
  tab_style(
    style = list(cell_text(color = "black", weight="bold"),
                 cell_fill(color = "#f6cd61")),
    locations = cells_column_spanners()
  )%>%
  tab_options(
    table.background.color = "black",
    table.align = "center",
    source_notes.background.color = "black",
    heading.background.color = "black",
    table_body.hlines.color = "#989898",
    table_body.border.top.color = "#989898",
    heading.border.bottom.color = "#989898",
  )->table4

gtsave(table4,"table4.png")





