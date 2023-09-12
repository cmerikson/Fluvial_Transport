library(shiny)
library(shinythemes)
library(shinycssloaders)
library(data.table)
library(tools)
library(openxlsx)
library(sf)
library(lubridate)
library(ggplot2)
library(plotly)
library(viridis)

ui <- fluidPage(
  theme=shinytheme('cosmo'),
  titlePanel("Fluvial Transport App"),
  sidebarLayout(
    sidebarPanel(
      # Explanatory text using HTML
      HTML("<p>This app calculates the distance moved for each subsequent survey date and the total distance moved in meters.<p>
      <p>Store all data in one folder and enter the path to this folder. Do not include quotation marks in the path name.<p>
      <p>Files may be a combination of .csv or .xlsx. A .shp centerline must be included as well.<p>
      <p>Specify the CRS value used to record tracer locations and ensure it matches the projection of the stream centerline. The default in WGS84 UTM Zone 18N.</p>
      <p>After processing, results in the table can be downloaded.</p>"),
      textInput("folder_path", "Enter Folder Path:", value = ""),
      textInput("crs", "Enter CRS:", value = "32618"),
      textInput("ID", "Enter ID Column", value= "Comment"),
      textInput("X", "Enter X Coordinate Column", value="Easting"),
      textInput("Y", "Enter Y Coordinate Column", value="Northing"),
      textInput("Date", "Enter Date Column", value="GPSTime"),
      actionButton("process_data", "Process Data"),
      downloadButton("download_transport", "Download Transport Table")  # Add download button
    ),
    mainPanel(
      tabsetPanel(
        # Tab for displaying the output table
        tabPanel("Output Table", withSpinner(tableOutput("table"))),
        
        # Tab for displaying the plot
        tabPanel("Plot", withSpinner(plotlyOutput("plot")))
      )
    )
  )
)

server <- function(input, output) {
  transport_data <- reactiveVal(NULL)
  plotly_obj <- reactiveVal(NULL)
  
  observeEvent(input$process_data, {
    
    # Get user inputs
    folder_path <- input$folder_path
    crs_string <- input$crs
    id_column <- input$ID
    x_column <- input$X
    y_column <- input$Y
    date_column <- input$Date
    
    # Function to list files in folder
    list_full = function(folder_path){
      csv_files <<- list.files(folder_path, pattern='csv', full.names=T)
      xlsx_files <<- list.files(folder_path, pattern='xlsx', full.names=T)
      shapefiles <<- list.files(folder_path, pattern='shp', full.names=T)
    }
    
    #Call function to list files
    list_full(folder_path)
    
    # Function to read files in a folder
    files = lapply(csv_files, fread)
    Data = rbindlist(files,fill=T)
    
    # Include Excel files
    if (length(xlsx_files)>0) {
      excel = lapply(xlsx_files, read.xlsx)
      excel = setDT(excel)
      if (typeof(excel) == 'list') {
        excel = rbindlist(excel)
        Data = rbind(Data,excel)
      }
    }
    
    # Test formatting
    date_check = function(data) {
      dates = as.character(substr(parse_date(data[,(strsplit(GPSTime,' '))]),start=1,stop=10))
      pattern = "^\\d{4}-\\d{2}-\\d{2}$"
      results = ifelse(sapply(dates, grepl, pattern=pattern), 0, 1)
      if(c(1)%in%results){
        stop("Error: Unable to interpret date format of at least one file.")}
      data = data[,Date:=as.character(substr(parse_date(data[,(strsplit(GPSTime,' '))]),start=1,stop=10))]
    }
    
    date_check(Data)
    
    # Read shapefile
    centerline = read_sf(shapefiles[endsWith(shapefiles,'shp')])
    centerline = st_set_crs(centerline, crs_string)
    
    # Avoid variable call issues
    Data = Data[,ID:=get(id_column)]
    
    # Clean Data
    Data = Data[,c('Date',..y_column,..x_column,'ID')]
    
    # Determine repeat observations
    Data = Data[,count:=.N,by='ID']
    
    # Function to get Survey Dates from column with date and time
    survey_dates = function (table) {
      surveys <<- unique(table$Date)
    }
    
    # Call survey date function
    survey_dates(Data)
    
    # Create sf object of Data
    sf_Data <<- st_as_sf(Data,coords = c(x_column,y_column),crs=crs_string)
    
    # Snap tracers to centerline
    snap_point_to_line <- function(point, line) {
      nearest_points <- st_nearest_points(point, line)
      if (length(nearest_points) > 1) { 
        length_test = function(points) {
          line_lengths = st_length(points)
        }
        shortest = which.min((lapply(nearest_points, length_test)))
        nearest_points = nearest_points[shortest]
      }
      nearest_points = st_cast(nearest_points,'POINT')[seq(2, 2, 2)]
      nearest_points_coordinates = st_coordinates(nearest_points)
      return(nearest_points_coordinates)
    }
    
    snapped_points <- lapply(sf_Data$geometry, snap_point_to_line, line = st_zm(centerline))
    
    table = do.call(rbind,snapped_points)
    table = as.data.table(table)
    
    table = st_as_sf(table,coords=c('X','Y'),crs=crs_string)
    
    # Add snapped_sf as a new column in the data.table
    Data = Data[, Location:=table]
    
    # Adjust table to output format
    adjust_table = function(data) {
      Movement = copy(data)
      Movement = Movement[,c('ID','Date','Location','count')]
      Movement = unique(Movement,by=c('ID','Date'))
      Movement = dcast(Movement,formula = ID ~ Date, value.var = 'Location', fun.aggregate = NULL)
      date_order = order(as.Date(surveys,format="%Y-%m-%d"))
      surveys = surveys[date_order]
      setcolorder(Movement,c('ID',surveys))
      Movement <<- Movement
    }
    
    adjust_table(Data)
    
    # Define a function to calculate distances
    distance <- function(data) {
      Transport <- data.table()
      comment <- data[['ID']]
      data <- data[, !'ID', with = FALSE]
      data <- st_as_sf(data)
      
      num_cols <- ncol(data)
      
      for (i in 1:(num_cols - 1)) {
        for (j in (i + 1):num_cols) {
          col1 <- data[[i]]
          col2 <- data[[j]]
          new_col_name <- paste0(names(data)[i], "_", names(data)[j], "_Distance")
          
          # Initialize an empty vector to store distances
          distances <- numeric(length(col1))
          
          # Calculate distances for all pairs of values in col1 and col2
          for (k in 1:length(col1)) {
            distances[k] <- ifelse(st_is_empty(col1[k]) | st_is_empty(col2[k]), NA_real_, st_distance(col1[k], col2[k]))
          }
          
          # Add the distances to the Transport table
          Transport[, (new_col_name) := distances]
        }
      }
      
      # Combine comment and Transport into a single table
      Transport <<- cbind(comment, Transport)
    }
    distance(Movement)
    
    Transport = Transport[,TotalDistance:=(do.call(pmax,c(Transport[,-1],na.rm=T)))]
    
    transport_data(Transport)
    
    # Create the ggplot object
    river_map <- ggplot() +
      geom_sf(data = centerline) +
      geom_sf(data = sf_Data, aes(color = as.character(year(as.Date(Date))))) +
      scale_color_viridis(discrete = TRUE, option = "D") +
      labs(x = 'Easting (m)', y = 'Northing (m)', color = 'Year',title = 'Tracer Locations') +
      theme_bw()+theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
    
    # Convert ggplot to a Plotly plot
    plotly_obj(ggplotly(river_map, width = 800, height = 600))
    
  })
  
  # For rendering a table, you can use:
  output$table <- renderTable({
    transport_data()
  })
  
  # Render the Plotly plot
  output$plot <- renderPlotly({
    plotly_obj()
  })
  
  # Define a download handler for the download button
  output$download_transport <- downloadHandler(
    filename = function() {
      paste("Transport_Table_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(transport_data(), file)
    }
  )
}

shinyApp(ui, server)