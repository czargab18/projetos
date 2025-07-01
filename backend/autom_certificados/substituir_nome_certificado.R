library(officer)

nome_participante <- "Maria da Silva"
pptx <- read_pptx("certificado_modelo.pptx")

# Substituir texto no primeiro slide, em todas as caixas
pptx <- pptx %>% 
  ph_with(value = nome_participante, location = ph_location_type(type = "body"))

print(pptx, target = "certificado_maria.pptx")