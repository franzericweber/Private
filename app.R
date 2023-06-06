library(shiny)
library(shinyWidgets)

dataFilePath <- "reserved_tickets.csv"
reservedTickets <- data.frame(Vorname = character(), Nachname = character(), Nummer = integer(), stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("Bootsparty 08.07.2023, Christina und Franz "),
  sidebarLayout(
    sidebarPanel(
      h4("Hinweise"),
      p("Hallo Fans,"),
      p("wir würden euch bitten, statt Getränken oder Präsenten, eine kleine Spende für die Bootsparty in Betracht zu ziehen."),
      p("In diesem Zusamenhang würden wir uns über einen Betrag von 20 Euro pro Person sehr freuen, um die Kosten unserer gemeinsamen Reise stämmen zu können."),
      p("Gegebenenfalls steht dafür der QR code unten oder die E-Mail-Adresse fe.weber95@googlemail.com für die PayPal-Transaktion zur Verfügung."),
      tags$img(height=250, width=250, src='qrcode.png'),  # Hier wird das Bild eingefügt
      hr(),
      p("Tragt hier bitte euren Vornamen und den ersten Buchstaben eures Nachnamens ein. Danach nur noch bestätigen:"),
      textInput("Vorname", "Vorname:"),
      textInput("Nachname", "1. Buchstabe Nachname:"),
      actionButton("submit", "Ticket zum Glück"),
      p(" "),
      p("Auf die MS Saga passen maximal 60 Gäste. Also bringt bitte keine Gäste mit, von denen wir nichts wissen."),
      p("Treffpunkt: 15 Uhr Österreich Park, Bootstour 19-23 Uhr"),
      p(" "),
      p("Musikalische Begleitung:"),
      p("19.00 Uhr - 20.30 Uhr: DJ Seemannsgarn"),
      p("20.30 Uhr - 22:00 Uhr: Blackbeard808"),
      p("22.00 Uhr - 23:00 Uhr: The Sirens of Görlitzer Park"),
      p("Wir freuen uns riesig auf euch!!!"),
      
      h4("Ticket löschen"),
      numericInput("ticketNumber", "Nummer des Tickets:", value = 1, min = 1, max = nrow(reservedTickets)),
      actionButton("delete", "Ticket löschen")
    ),
    mainPanel(
      tableOutput("ticketTable"),
      img(height=300, width=500, src='boat.gif', align = "right")
    )
  )
)

server <- function(input, output, session) {
  # Load existing data from file if it exists
  if (file.exists(dataFilePath)) {
    reservedTickets <- read.csv(dataFilePath, stringsAsFactors = FALSE)
  }
  
  options(shiny.httpuv.request.timeout = 3024000000)  # time is in milliseconds (1000 is 1 second) Timeout auf 5 wochen Stunde (3600 Sekunden) setzen
  
  # Event handler for the "Reserve Ticket" button
  observeEvent(input$submit, {
    Vorname <- input$Vorname
    Nachname <- input$Nachname
    
    # Check if the Vorname and Nachname are provided
    if (nchar(Vorname) > 0 && nchar(Nachname) > 0) {
      # Add the ticket reservation to the dataframe with the next number
      newTicket <- data.frame(Vorname = Vorname, Nachname = Nachname, Nummer = nrow(reservedTickets) + 1, stringsAsFactors = FALSE)
      reservedTickets <- rbind(reservedTickets, newTicket)
      
      # Clear the name and Nachname inputs
      updateTextInput(session, "Vorname", value = "")
      updateTextInput(session, "Nachname", value = "")
      
      # Save the updated data to file
      write.csv(reservedTickets, dataFilePath, row.names = FALSE)
      
      # Reload the session to update the displayed table
      session$reload()
    }
  })
  
  # Event handler for the "Delete Ticket" button
  observeEvent(input$delete, {
    # Get the selected ticket number
    selectedNum <- as.numeric(input$ticketNumber)
    
    # Remove the selected ticket from the dataframe
    reservedTickets <- reservedTickets[-selectedNum, ]
    
    # Save the updated data to file
    write.csv(reservedTickets, dataFilePath, row.names = FALSE)
    
    # Reload the session to update the displayed table
    session$reload()
  })
  
  # Render the table showing the reserved tickets
  output$ticketTable <- renderTable({
    reservedTickets
  })
  #Stop the app timing out
  autoInvalidate <- reactiveTimer(10000)
  observe({
    autoInvalidate()
    cat(".")
  })
}

shinyApp(ui, server)
