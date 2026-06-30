# Fetches EcoEvoJobs Google Sheet and makes a map (ecoevojobs.net)
# May need to be updated every year since the sheets are archived? New URL for a new year?
# Output is two HTML files with maps: One for Permanent/Faculty positions and the other for PostDocs

# automatically download uninstalled packages from the registry
libNames <- c('tidyverse', 'httr', 'geodata', "tidyterra", 'leaflet', 'flextable', 'htmlwidgets')
for(i in 1:length(libNames)){
    if (!libNames[i] %in% rownames(installed.packages())){
        install.packages(libNames[i], dependencies = TRUE)
    }
    library(libNames[i], character.only=TRUE)
}

##########
## Vars ##
##########

# set working directory
# Most importantly, output is sent to the working directory. Geodata also put GADM files for the maps there, too, by default. 
setwd(r"(C:\Users\Conrad\Desktop\JobSearch\EcoEvoJobsMap)")

# Set GADM path
# This is where the geodata package will put the files that store the state / country polygones (can be a few megabytes each)
# By default: current working directory (".")
gadm_path <- "."

# EvoEco Jobs Google Sheets URL (current May 2026)
# Need both the 
EvoEcoURL <- r"(https://docs.google.com/spreadsheets/d/1P7BfU0emdcGFVIWIs_erFxyy0UGXXORw7h0rpU19gQ8/edit?gid=1228591705)"

# IDs for each sheet (may need to be updated)
# This is the string of numbers after "gid=" in the URL
# I hard-coded handling for the Permanent Faculty and PostDoc sheet; would need to rework this if more sheets got added
gSheet_IDs <- c(PermFaculty = 1219796980,
                PostDocs = 1228591705)

# Cutoff for review date when filtering (greater than or equal to this date)
review_cutoff <- lubridate::today() + lubridate::days(0)

# Cutoff for old posts (greater than or equal to this date)
stale_cutoff <- lubridate::today() - lubridate::days(60)

###########
## /Vars ##
###########

# Google Drive ID for the document extracted from the URL (may need to change if Google every changes how Drive IDs work)
gDrive_ID <- stringr::str_extract(EvoEcoURL, "[A-Za-z0-9-_]{30,}")

# Build the Download URLs
gSheet_DownloadURLs <- c(PermFaculty = paste0('https://docs.google.com/spreadsheets/export?id=',gDrive_ID,'&format=tsv&gid=',gSheet_IDs["PermFaculty"]),
                         PostDocs = paste0('https://docs.google.com/spreadsheets/export?id=',gDrive_ID,'&format=tsv&gid=',gSheet_IDs["PostDocs"]))

# Download Data
PermFacultyData <- httr::content(httr::GET(gSheet_DownloadURLs["PermFaculty"]), as = "parsed", type = "text/tab-separated-values", encoding = "UTF-8", skip = 1, show_col_types = FALSE)
PostDocsData <- httr::content(httr::GET(gSheet_DownloadURLs["PostDocs"]), as = "parsed", type = "text/tab-separated-values", encoding = "UTF-8", skip = 1, show_col_types = FALSE)

# Fix spaces in column names to make working with this easier
colnames(PermFacultyData) <- stringr::str_replace_all(colnames(PermFacultyData), " ", "_")
colnames(PostDocsData) <- stringr::str_replace_all(colnames(PostDocsData), " ", "_")

# For some reason, the Faculty Data has duplicate columns, so drop them and fix names (may no longer be needed in the future)
PermFacultyData <- PermFacultyData %>% 
    select(-Notes...13, -Number_Applied...14) %>%
    rename(Notes = Notes...10,
           Number_Applied = Number_Applied...11)

# Drop 'Mod_Flag' and 'Number_Applied' columns (I don't see these as useful)
PermFacultyData <- PermFacultyData %>% select(-Mod_Flag, -Number_Applied)
PostDocsData <- PostDocsData %>% select(-Mod_Flag, -Notes_Backup)

# Correctly format columns as datetime
PermFacultyData <- PermFacultyData %>% 
    mutate(Timestamp = parse_date_time(Timestamp, orders = "mdy HM"),
           Review_Date = parse_date_time(Review_Date, orders = "mdy"),
           Last_Update = parse_date_time(Last_Update, orders = "mdy HM"))

PostDocsData <- PostDocsData %>% 
    mutate(Timestamp = parse_date_time(Timestamp, orders = "mdy HM"),
           Review_Date = parse_date_time(Review_Date, orders = "mdy"),
           Last_Update = parse_date_time(Last_Update, orders = "mdy HM"))

# Put the District of Columbia in Maryland
PermFacultyData <- PermFacultyData %>% 
    mutate(Location = if_else(Location == "District of Columbia", "Maryland", Location))
PostDocsData <- PostDocsData %>% 
    mutate(Location = if_else(Location == "District of Columbia", "Maryland", Location))

# Apply filtering by review date
PermFacultyData <- PermFacultyData %>%
    filter(Review_Date >= review_cutoff | is.na(Review_Date), Timestamp >= stale_cutoff)
PostDocsData <- PostDocsData %>%
    filter(Review_Date >= review_cutoff | is.na(Review_Date), Timestamp >= stale_cutoff)

# Truncates 'Notes' columns so it looks less terrible when making the tables in the map
PermFacultyData <- PermFacultyData %>%
    mutate(Notes = stringr::str_trunc(Notes, width = 180, side = "right"))
PostDocsData <- PostDocsData %>%
    mutate(Notes = stringr::str_trunc(Notes, width = 180, side = "right"))

# Final Arrange -> sort by timestamp
PermFacultyData <- PermFacultyData %>% arrange(desc(Timestamp))
PostDocsData <- PostDocsData %>% arrange(desc(Timestamp))

# Create the "Land of Other" for the people who don't put a US state or country in the location field
land_of_other <- vect(
    matrix(c(-42,26,
             -42,30,
             -38,30,
             -38,26,
             -42,26),
           ncol = 2,
           byrow = TRUE),
    type = "polygons",
    atts = data.frame(Location_Associable = "Other"),
    crs = crs(geodata::gadm("USA", resolution = 2, path = gadm_path))
)

##############
## Mapping ###
##############

# PermFaculty Mapping
PermFaculty_states <- intersect(unique(PermFacultyData %>% pull(Location)), state.name)
PermFaculty_countries <- intersect(unique(PermFacultyData %>% pull(Location)), country_codes() %>% pull(NAME))
PermFaculty_other <- setdiff(unique(PermFacultyData %>% pull(Location)), c(PermFaculty_states, PermFaculty_countries))

# Catch things that aren't states or countries in the location field; call them "Other"
# This column is purely for associating data with map locations
PermFacultyData <- PermFacultyData %>% 
    mutate(Location_Associable = if_else(Location %in% PermFaculty_other, "Other", Location))

# Create the map areas and add the Location_Associable field to bind these to the data
PermFaculty_states_gadm <- geodata::gadm("USA", resolution = 2, path = gadm_path) %>%
    filter(NAME_1 %in% PermFaculty_states)
PermFaculty_countries_gadm <- geodata::gadm(PermFaculty_countries, level = 0, resolution = 2, path = gadm_path)
PermFaculty_AllAreas <- rbind(PermFaculty_countries_gadm, PermFaculty_states_gadm) %>%
    mutate(Location_Associable = if_else(COUNTRY == "United States", NAME_1, COUNTRY))
PermFaculty_AllAreas <- rbind(PermFaculty_AllAreas, land_of_other)

# Render
PermFaculty_leaflet_render <- leaflet() %>%
    addTiles(urlTemplate = "https://{s}.tile.openstreetmap.fr/hot/{z}/{x}/{y}.png")

for(loc in unique(PermFacultyData %>% pull(Location_Associable))){
    if(loc == "Other"){
        select_these <- c("Timestamp", "Location", "Institution", "Subject_Area", "Review_Date", "Position_Type", "URL", "Notes")
    }else{
        select_these <- c("Timestamp", "Institution", "Subject_Area", "Review_Date", "Position_Type", "URL", "Notes")
    }
    ft <- flextable(PermFacultyData %>% 
                        filter(Location_Associable == loc) %>% 
                        select(all_of(select_these))) %>%
        compose(j = "URL", value = as_paragraph(hyperlink_text(x = URL, url = URL))) %>%
        theme_zebra() %>%
        valign(valign = "top", part = "body") %>%
        set_table_properties(layout = "fixed",
                             opts_html = list(
                                 scroll = list(
                                     height = "450px"
                                 )
                             )) %>%
        htmltools_value() %>%
        as.character() %>% 
        stringr::str_replace("<style></style>\n", "") %>%
        stringr::str_replace_all("<a ", "<a target=\"_blank\" ")
    PermFaculty_leaflet_render <- PermFaculty_leaflet_render %>% 
        addPolygons(data = PermFaculty_AllAreas %>% 
                        filter(Location_Associable == loc),
                    popup = paste0("<h3>", loc, " (<a href=\"http://ecoevojobs.net\" target= \"_blank\">EcoEvoJobs.net</a> for complete Notes); Map last updated: ", date(today()),"</h3>", ft),
                    options = list(
                        popupOptions = list(
                            minWidth = 400, 
                            maxWidth = 600,
                            maxHeight = 500 
                            )
                        ),
                    color = "darkgrey",
                    weight = 1,
                    smoothFactor = 2,
                    opacity = 1,
                    fillOpacity = 0.70,
                    fillColor = "darkred",
                    highlightOptions = list(
                        color = "lightgrey",
                        weight = 2,
                        bringToFront = TRUE
                    ))
}

htmlwidgets::saveWidget(PermFaculty_leaflet_render, 
                        paste0("PermFaculty.html"), 
                        selfcontained = TRUE,
                        title = "PermFacultyJobs - EcoEvoJobs")

# PostDocs Mapping
PostDocs_states <- intersect(unique(PostDocsData %>% pull(Location)), state.name)
PostDocs_countries <- intersect(unique(PostDocsData %>% pull(Location)), country_codes() %>% pull(NAME))
PostDocs_other <- setdiff(unique(PostDocsData %>% pull(Location)), c(PostDocs_states, PostDocs_countries))

# Catch things that aren't states or countries in the location field; call them "Other"
# This column is purely for associating data with map locations
PostDocsData <- PostDocsData %>% 
    mutate(Location_Associable = if_else(Location %in% PostDocs_other, "Other", Location))

# Create the map areas and add the Location_Associable field to bind these to the data
PostDocs_states_gadm <- geodata::gadm("USA", resolution = 2, path = gadm_path) %>%
    filter(NAME_1 %in% PostDocs_states)
PostDocs_countries_gadm <- geodata::gadm(PostDocs_countries, level = 0, resolution = 2, path = gadm_path)
PostDocs_AllAreas <- rbind(PostDocs_countries_gadm, PostDocs_states_gadm) %>%
    mutate(Location_Associable = if_else(COUNTRY == "United States", NAME_1, COUNTRY))
PostDocs_AllAreas <- rbind(PostDocs_AllAreas, land_of_other)

# Render
PostDocs_leaflet_render <- leaflet() %>%
    addTiles(urlTemplate = "https://{s}.tile.openstreetmap.fr/hot/{z}/{x}/{y}.png")

for(loc in unique(PostDocsData %>% pull(Location_Associable))){
    if(loc %in% PostDocs_other){
        select_these <- c("Timestamp", "Last_Update", "Location", "Institution", "Subject_Area", "Review_Date", "PI", "URL", "Notes")
    }else{
        select_these <- c("Timestamp", "Last_Update", "Institution", "Subject_Area", "Review_Date", "PI", "URL", "Notes")
    }
    ft <- flextable(PostDocsData %>% 
                        filter(Location_Associable == loc) %>% 
                        select(all_of(select_these))) %>%
        compose(j = "URL", value = as_paragraph(hyperlink_text(x = URL, url = URL))) %>%
        theme_zebra() %>%
        valign(valign = "top", part = "body") %>%
        set_table_properties(layout = "fixed",
                             opts_html = list(
                                 scroll = list(
                                     height = "450px"
                                 )
                             )) %>%
        htmltools_value() %>%
        as.character() %>% 
        stringr::str_replace("<style></style>\n", "") %>%
        stringr::str_replace_all("<a ", "<a target=\"_blank\" ")
    PostDocs_leaflet_render <- PostDocs_leaflet_render %>% 
        addPolygons(data = PostDocs_AllAreas %>% 
                        filter(Location_Associable == loc),
                    popup = paste0("<h3>", loc, " (<a href=\"http://ecoevojobs.net\" target= \"_blank\">EcoEvoJobs.net</a> for complete Notes); Map last updated: ", date(today()),"</h3>", ft),
                    options = list(
                        popupOptions = list(
                            minWidth = 400, 
                            maxWidth = 600,
                            maxHeight = 500 
                        )
                    ),
                    color = "darkgrey",
                    weight = 1,
                    smoothFactor = 2,
                    opacity = 1,
                    fillOpacity = 0.70,
                    fillColor = "darkred",
                    highlightOptions = list(
                        color = "lightgrey",
                        weight = 2,
                        bringToFront = TRUE
                    ))
}

htmlwidgets::saveWidget(PostDocs_leaflet_render, 
                        paste0("PostDocsJobs.html"), 
                        selfcontained = TRUE,
                        title = "PostDocsJobs - EcoEvoJobs")
