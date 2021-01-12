
# Wrangler function


wrangler <- function(data){ 

#data <- readxl::read_xlsx("takeoff_decluttered.xlsx")
    
  
df <- data %>%
janitor::clean_names() %>% as_tibble()%>%
  mutate(level = str_trim(substr(level,6,8))) %>% 
  mutate_if(is.character, tolower) %>% 
  mutate(service = case_when(
    service == "stucco"~"stucco",
    service == "framing"~"framing",
    service == "stucco adittion"~"stucco_adittion",
    service == "exterior paint"~"exterior_paint",
    service == "interior paint"~"interior_paint",
    service == "drywall & finish"~"drywall_finish")) %>% 
  filter(service == "stucco") %>%
  group_by(level,material_surface) %>% 
  summarise(total_sf = sum(total_sf), .groups = "drop") %>% 
  mutate(yards = round(total_sf/9)) %>%
  mutate(yard_price = case_when(material_surface == "5/8_texture_stucco" & level == 1~38,
                                material_surface == "5/8_texture_stucco" & level == 2~39,
                                material_surface == "5/8_texture_stucco_wire_lath" & level == 1~41,
                                material_surface == "5/8_texture_stucco_wire_lath" & level == 2~42)) %>% 
  mutate(level = ifelse(level == 1, "First Level", "Second Level")) %>% 
  mutate(Price = yards*yard_price) %>% select(-yard_price) %>% adorn_totals() %>% 
  rename("Floor Levels" = level, "Material & Suerface" = material_surface,
         "SquareF" = total_sf, Yards = yards) %>% mutate(Price = scales::dollar(Price))

return(df)

}
