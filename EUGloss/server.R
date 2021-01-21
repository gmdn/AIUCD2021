#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    output$data_source <- renderTable({
        
        dataset %>%
            filter(language == input$src_lang) %>%
            select(codes, language, technical) %>%
            top_n(n = 10)

    })

    output$data_target <- renderTable({
        
        dataset %>%
            filter(language == input$trgt_lang) %>%
            select(codes, language, technical) %>%
            top_n(n = 10)
        
    })
    
    observe({

        req(input$src_lang)

        lang_choices <- dataset %>%
            filter(language == input$src_lang) %>%
            pull(search)

        updatePickerInput(
            session = session,
            inputId = "src_search",
            choices = lang_choices
        )
    })

        
    output$choice_src_pop <- renderText({
        
        req(input$src_search, input$src_lang)
        
        term <- dataset %>%
            filter(search == input$src_search) %>%
            pull(popular)
        
        #print(term)
        
        return(term)
        
    })

    output$choice_trgt_pop <- renderText({
        
        req(input$src_search, input$src_lang, input$trgt_lang)
        
        code <- dataset %>%
            filter(search == input$src_search) %>%
            pull(codes)
        
        dataset %>%
            filter(codes == code & language == input$trgt_lang) %>% # & language == input$trgt_lang) %>%
            pull(popular)
        
    })
    
    output$choice_src_tech <- renderText({
        
        req(input$src_search, input$src_lang)
        
        term <- dataset %>%
            filter(search == input$src_search) %>%
            pull(technical)
        
        #print(term)
        
        return(term)
        
    })
    
    
    output$choice_trgt_tech <- renderText({
        
        req(input$src_search, input$src_lang, input$trgt_lang)
        
        code <- dataset %>%
            filter(search == input$src_search) %>%
            pull(codes)
        
        dataset %>%
            filter(codes == code & language == input$trgt_lang) %>% # & language == input$trgt_lang) %>%
            pull(technical)
        
    })
    
    term_selected <- reactive({
        
        # check conditions
        req(input$src_search, input$src_lang)
        
        # get term record
        term <- dataset %>%
            filter(search == input$src_search)
        
        
    })
    
    term_download <- reactive({
        
        # check conditions
        req(input$src_search, input$src_lang)
        
        # get source term record
        term_sel <- term_selected()
        
        # get term records for each language
        term <- dataset %>%
            filter(codes == term_sel$codes)
        
    })
    
    
    # show xml
    output$tbx_to_text <- renderText({
        
        # check conditions
        req(input$src_search, input$src_lang)
        
        # get selected term (to download)
        term <- term_download()
        
        #print(term)
        
        # create tbx
        tbx <- create_tbx(term)
        
        # write on file
        file_temp <- tempfile(tmpdir = "./temp", fileext = ".tbx")
        write_xml(x = tbx, file = file_temp)
        
        #write_file("<xml>pippo,/xml>", file_temp)
        
        withProgress(message = "Preparing TBX", value = 0, {
            
            for (i in 1:10) {
                
                # Increment the progress bar, and update the detail text.
                incProgress(1/10, detail = "Reading...")
                
                # Pause for 0.1 seconds to simulate a long computation.
                Sys.sleep(0.1)
            }
            
        })
        
        tbx_txt <- read_file(file_temp)
        
        Sys.sleep(0.1)
        
        # read and delete temp file
        
        file.remove(file_temp)
        
        #return("ciao")
        return(tbx_txt)
        
    })
    
    output$get_the_item <- renderUI({
        
        # check conditions
        req(input$src_search, input$src_lang)
        
        downloadButton("downloadData", label = "Download TBX")
    })
    
    # download xml
    output$downloadData <- downloadHandler(
        
        # build filename
        filename = function() {
            paste('EU_GLOSS_TriMED_', term_selected()$technical, '.tbx', sep='')
        },
        content = function(con) {
            term <- term_download()
            tbx <- create_tbx(term)
            write_xml(tbx, con)
        }
    )
    
    
})
