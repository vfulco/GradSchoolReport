grad <- render('Gradreport.Rmd',
               pdf_document(),
               output_file = paste0("temp/All/",  "Gradreport.pdf"),
               params = list(degree = unique(program$DEGR),
                             major = unique(program$MAJOR))) %>% unlist %>% basename
files <- gathered() %>% dlply(.(MAJOR, DEGR),function(program){
  render('programs.Rmd',
         pdf_document(),
         output_file = paste0("temp/All/", unique(program$MAJOR), "_", unique(program$DEGR), ".pdf"),
         params = list(degree = unique(program$DEGR),
                       major = unique(program$MAJOR)))
}) %>% unlist %>% basename
