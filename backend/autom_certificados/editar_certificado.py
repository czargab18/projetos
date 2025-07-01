
def lerpptx(caminho: str = "", nomearquivo: str = ""):
    from pptx import Presentation
    import os

    if caminho != "" and nomearquivo != "":
        caminhoarquivo = os.path.join(caminho, f"{nomearquivo}.pptx")
        prs = Presentation(caminhoarquivo)
        conteudo = ""
        for slide in prs.slides:
            for shape in slide.shapes:
                # Verifica se o shape tem o atributo text_frame
                if hasattr(shape, 'text_frame'):
                    text_frame = getattr(shape, 'text_frame', None)
                    if text_frame is not None:
                        for paragraph in text_frame.paragraphs:
                            conteudo += paragraph.text + "\n"
        return conteudo
    return ""


print(lerpptx(caminho="./", nomearquivo="certificado"))


# from pptx import Presentation

# def substituir_nome(input_pptx, output_pptx, nome_participante):
#     prs = Presentation(input_pptx)
#     for slide in prs.slides:
#         for shape in slide.shapes:
#             if shape.has_text_frame:
#                 for paragraph in shape.text_frame.paragraphs:
#                     for run in paragraph.runs:
#                         if "NOME_DO_PARTICIPANTE" in run.text:
#                             run.text = run.text.replace("NOME_DO_PARTICIPANTE", nome_participante)
#     prs.save(output_pptx)

# # Exemplo de uso:
# substituir_nome("certificado_modelo.pptx", "certificado_maria.pptx", "Maria da Silva")




# from pptx import Presentation

# def substituir_nome(input_pptx, output_pptx, nome_participante):
#     prs = Presentation(input_pptx)
#     for slide in prs.slides:
#         for shape in slide.shapes:
#             if shape.has_text_frame:
#                 for paragraph in shape.text_frame.paragraphs:
#                     for run in paragraph.runs:
#                         if "NOME_DO_PARTICIPANTE" in run.text:
#                             run.text = run.text.replace("NOME_DO_PARTICIPANTE", nome_participante)
#     prs.save(output_pptx)

# # Exemplo de uso:
# substituir_nome("certificado_modelo.pptx", "certificado_maria.pptx", "Maria da Silva")