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
                ,"https://www.akjournals.com/view/journals/2006/14/2/2006.14.issue-2.xml)"
                ,"https://www.akjournals.com/view/journals/2006/14/3/2006.14.issue-3.xml"
                ,"https://www.akjournals.com/view/journals/2006/14/4/2006.14.issue-4.xml")

# Definimos un user-agent similar al de un navegador real
user_agent_navegador <- paste(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
  "AppleWebKit/537.36 (KHTML, like Gecko)",
  "Chrome/122.0.0.0 Safari/537.36"
)

# Construimos la solicitud HTTP y añadimos encabezados para 
# que parezca una visita normal
respuesta <- request(url) %>%
  req_user_agent(user_agent_navegador) %>%
  req_headers(`accept-language` = "en-US,en;q=0.9") %>%
  req_perform()

# Convertimos el cuerpo de la respuesta en un documento HTML
# analizable con rvest/xml2
pagina <- respuesta %>%
  resp_body_html()

#################
#TITULO
#################
titulo <- pagina %>%
  html_elements("[data-testid='block-primitivetitle']") %>%
  html_text()
titulo <- titulo[length(titulo)]

extraccion_general <- function(nodo) {
  # Extraemos el títulos de los articulos
  titulo <- nodo %>%
    html_elements("[data-testid='block-primitivetitle']") %>%
    html_text2()
  
  issue <- titulo[length(titulo)]
  titulo <- titulo[-length(titulo)]
    # Extraemos el enlace relativo hacia la página del producto
  doi <- nodo |> 
    html_elements( " a,[target='_blank'], .c-Button--link") |>
    html_text()
  doi <- doi[grep(pattern="https://doi*",doi)]
  
  tibble(
    titulo = titulo,
    doi = doi,
    issue = issue
  )
}

df = extraccion_general(pagina)
