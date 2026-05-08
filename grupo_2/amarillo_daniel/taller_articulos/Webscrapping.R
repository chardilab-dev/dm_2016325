paquetes <- c(
  "rvest", "xml2", "httr2", "dplyr", "stringr",
  "purrr", "tibble", "janitor", "readr", "knitr"
)

# Verificamos qué paquetes faltan
instalados <- rownames(installed.packages())
pendientes <- setdiff(paquetes, instalados)

if (length(pendientes) > 0) {
  install.packages(pendientes)
}

# Cargamos los paquetes sin mostrar mensajes
lapply(paquetes, library, character.only = TRUE)
  
url <- "https://www.akjournals.com/view/journals/2006/14/1/2006.14.issue-1.xml"

issues_url <- c("https://www.akjournals.com/view/journals/2006/14/1/2006.14.issue-1.xml"
                ,"https://www.akjournals.com/view/journals/2006/14/2/2006.14.issue-2.xml"
                ,"https://www.akjournals.com/view/journals/2006/14/3/2006.14.issue-3.xml"
                ,"https://www.akjournals.com/view/journals/2006/14/4/2006.14.issue-4.xml"
                ,"https://www.akjournals.com/view/journals/2006/15/1/2006.15.issue-1.xml")

crear_nodo <- function(url){
# Definimos un user-agent similar al de un navegador real
user_agent_navegador <- paste(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
  "AppleWebKit/537.36 (KHTML, like Gecko)",
  "Chrome/122.0.0.0 Safari/537.36"
)

# Construimos la solicitud HTTP y añadimos encabezados para 
# que parezca una visita normal

respuesta <- request(url) |>
  req_user_agent(user_agent_navegador) |>
  req_headers(`accept-language` = "en-US,en;q=0.9") |>
  req_perform()

# Convertimos el cuerpo de la respuesta en un documento HTML
# analizable con rvest/xml2
nodo <- respuesta |>
  resp_body_html()
nodo
}

#TITULO
#################

extraccion_general <- function(nodo) {
  # Extraemos el títulos de los articulos
  titulo <- nodo |>
    html_elements("[data-testid='block-primitivetitle']") |>
    html_text2()
  
  issue <- titulo[length(titulo)]
  titulo <- titulo[-length(titulo)]
    # Extraemos el enlace relativo doi del articulo
  doi <- nodo |> 
    html_elements( " a,[target='_blank'], .c-Button--link") |>
    html_text()
  doi <- doi[grep(pattern="https://doi*",doi)]
  #Creamos una tabla con la información general de cada journal 
  tibble(
    titulo = titulo,
    doi = doi,
    issue = issue
  )
}


generalities <- data.frame()
for (i in issues_url) {
  nodo = crear_nodo(i)
  df <- extraccion_general(nodo)
  generalities <- rbind(generalities,df)
}

doi_prueba <- generalities$doi[1]
pagina <- crear_nodo(doi_prueba)

 #EXTRACCIÓN DOI (LLAVE EN COMUN CON GENERALITIES)
doi <- pagina |> 
  html_elements( " a,[target='_blank'], .c-Button--link") |>
  html_text()
doi <- doi[grep(pattern="https://doi*",doi)][1]

 #AUTORES 
autores_ex <- pagina |> 
  html_elements("[data-testid='author-name']") |>
  html_text()
autores <- paste(autores_ex,collapse = ",")
 
 #ABSTRACT
 raw_abstract <- pagina |> html_elements(".abstract.border-bottom.border-bottom-solid.border-bottom-medium") |>
   html_text2()
 a<- unlist(strsplit(raw_abstract, split = "\n"))
 Backround_and_aims <- a[4]
 Methods <- a[8]
 Results <- a[12]
 Conclusion <- a[16]

 fecha <- pagina |> 
   html_elements(".printpubdate.c-List__items") |>
   html_text2() |>
   str_remove_all("\t|\n") |>
   str_remove("^[^0-9]+")
 
 #URL 
 URL <- pagina |> 
   html_elements('[rel="canonical"], href')|>
   html_attr("href")
 
 ref_ex <- pagina |> 
   html_elements(".citationText.text-body1") |>
   html_text()
 #SEPARADOR PROPIO /--/
 referencias <- paste(ref_ex,collapse = "/--/")
 
 
 # CITAS
 dyn_cit = read_html_live(URL)
 dyn_cit$click("[data-tab-id='citedBy-30734']")
 dyn_cit |> html_elements("#citedByWidget") |>
   html_children("p")
 on.exit(dyn_cit$session$close())
