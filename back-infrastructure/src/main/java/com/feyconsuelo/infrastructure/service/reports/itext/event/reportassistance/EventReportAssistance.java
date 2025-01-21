package com.feyconsuelo.infrastructure.service.reports.itext.event.reportassistance;

import com.feyconsuelo.application.service.report.EventReportService;
import com.feyconsuelo.domain.exception.FeYConsueloException;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.lowagie.text.Chunk;
import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.Font;
import com.lowagie.text.Paragraph;
import com.lowagie.text.Phrase;
import com.lowagie.text.Rectangle;
import com.lowagie.text.pdf.PdfPCell;
import com.lowagie.text.pdf.PdfPTable;
import com.lowagie.text.pdf.PdfWriter;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.awt.*;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Base64;
import java.util.List;

@Service
public class EventReportAssistance implements EventReportService {

    private static final Font TITLE_FONT = new Font(Font.HELVETICA, 14, Font.BOLD);
    private static final Font SECTION_FONT = new Font(Font.HELVETICA, 11, Font.BOLD);
    private static final Font VOICE_FONT = new Font(Font.HELVETICA, 8, Font.BOLD);
    private static final Font MUSICIAN_FONT = new Font(Font.HELVETICA, 7);

    public String getEventReportAssistance(
            final String title,
            final Boolean displacementBus,
            final List<MusicianResponse> musiciansAssistBus,
            final List<MusicianResponse> musiciansAssistOther,
            final List<MusicianResponse> musiciansNotAssists,
            final String base64Image, // Imagen de marca de agua opcional
            final String base64HeaderImage // Imagen de cabecera opcional
    ) throws IOException {

        try (final ByteArrayOutputStream baos = new ByteArrayOutputStream()) {
            final Document document = new Document(new Rectangle(595, 842), 36, 36, 36, 36); // A4 con márgenes
            final PdfWriter writer = PdfWriter.getInstance(document, baos);

            // Configurar el evento de página para la marca de agua
            if (base64Image != null && !base64Image.isEmpty()) {
                writer.setPageEvent(new EventReportAssistanceWatermarkPageEvent(base64Image));
            }

            document.open();

            // Agregar imagen de cabecera si está presente
            this.addHeaderImage(document, base64HeaderImage);

            // Agregar título principal
            this.addTitleWithUnderline(document, title);

            if (Boolean.TRUE.equals(displacementBus)) {
                // Sección: Asisten en bus
                if (Boolean.FALSE.equals(CollectionUtils.isEmpty(musiciansAssistBus))) {
                    this.addSection(document, "Asisten en bus", musiciansAssistBus, Boolean.TRUE);
                }

                // Sección: Asisten por su cuenta
                if (Boolean.FALSE.equals(CollectionUtils.isEmpty(musiciansAssistOther))) {
                    this.addSection(document, "Asisten por su cuenta", musiciansAssistOther, Boolean.FALSE);
                }

                // Sección: No asisten
                if (Boolean.FALSE.equals(CollectionUtils.isEmpty(musiciansNotAssists))) {
                    this.addSection(document, "No asisten", musiciansNotAssists, Boolean.FALSE);
                }

            } else {
                // Sección: Asisten
                if (Boolean.FALSE.equals(CollectionUtils.isEmpty(musiciansAssistOther))) {
                    this.addSection(document, "Asisten", musiciansAssistOther, Boolean.FALSE);
                }

                // Sección: No asisten
                if (Boolean.FALSE.equals(CollectionUtils.isEmpty(musiciansNotAssists))) {
                    this.addSection(document, "No asisten", musiciansNotAssists, Boolean.FALSE);
                }
            }

            document.close();

            return Base64.getEncoder().encodeToString(baos.toByteArray());
        } catch (final DocumentException e) {
            throw new FeYConsueloException("Error al generar el PDF", e);
        }
    }

    private void addTitleWithUnderline(final Document document, final String title) throws DocumentException {
        // Crear un Chunk con el título subrayado
        final Chunk titleChunk = new Chunk(title, TITLE_FONT);
        titleChunk.setUnderline(0.1f, -2f); // Subrayado: grosor de la línea (0.1f) y posición vertical (-2f)

        // Agregar el título al documento
        final Paragraph paragraphTitle = new Paragraph(titleChunk);
        paragraphTitle.setAlignment(Element.ALIGN_CENTER); // Centrado
        paragraphTitle.setSpacingAfter(5); // Espaciado inferior
        document.add(paragraphTitle);
    }

    private void addHeaderImage(final Document document, final String base64HeaderImage) throws IOException, DocumentException {
        if (base64HeaderImage != null && !base64HeaderImage.isEmpty()) {
            final byte[] imageBytes = Base64.getDecoder().decode(base64HeaderImage);
            final com.lowagie.text.Image headerImage = com.lowagie.text.Image.getInstance(imageBytes);

            // Escalar la imagen para ajustarla al ancho de la página
            headerImage.scaleToFit(500, 120);
            headerImage.setAlignment(Element.ALIGN_CENTER);

            // Agregar la imagen al documento
            document.add(headerImage);

            // Espaciado debajo de la imagen
            document.add(new Paragraph("\n", new Font(Font.HELVETICA, 5)));
        }
    }

    @SuppressWarnings("java:S3776")
    private void addSection(final Document document,
                            final String title,
                            final List<MusicianResponse> musicians,
                            final Boolean drawCheckbox) throws DocumentException {

        // Título de la sección
        final Paragraph sectionTitle = new Paragraph(title + " (" + musicians.size() + ")", SECTION_FONT);
        sectionTitle.setSpacingBefore(0);
        sectionTitle.setSpacingAfter(0);
        document.add(sectionTitle);

        // Crear la tabla para la sección
        final PdfPTable table;
        if (Boolean.TRUE.equals(drawCheckbox)) {
            table = new PdfPTable(6); // Tres columnas
        } else {
            table = new PdfPTable(3); // Tres columnas
        }
        table.setWidthPercentage(100);
        table.setSpacingBefore(5);
        table.setSpacingAfter(5);
        if (Boolean.TRUE.equals(drawCheckbox)) {
            table.setWidths(new float[]{1, 12, 1, 12, 1, 12}); // Ancho igual para las columnas
        } else {
            table.setWidths(new float[]{1, 1, 1}); // Ancho igual para las columnas
        }

        Long previousVoice = null;
        int columnCounter = 0;
        final int moduleColumnCounter;
        if (Boolean.TRUE.equals(drawCheckbox)) {
            moduleColumnCounter = 6;
        } else {
            moduleColumnCounter = 3;
        }

        for (final MusicianResponse musician : musicians) {
            // Detectar cambio de voz
            if (previousVoice == null || !previousVoice.equals(musician.getVoice().getId())) {
                // Completar la fila actual si no está vacía
                while (columnCounter % moduleColumnCounter != 0) {
                    table.addCell(""); // Celdas vacías para completar la fila
                    columnCounter++;
                }

                // Agregar el encabezado de la nueva voz
                final PdfPCell voiceCell = new PdfPCell(new Phrase(musician.getVoice().getName(), VOICE_FONT));
                if (Boolean.TRUE.equals(drawCheckbox)) {
                    voiceCell.setColspan(6); // Ocupa las tres columnas
                } else {
                    voiceCell.setColspan(3); // Ocupa las tres columnas
                }

                voiceCell.setPaddingTop(2);
                voiceCell.setPaddingBottom(4);
                voiceCell.setPaddingLeft(5);
                voiceCell.setHorizontalAlignment(Element.ALIGN_LEFT);
                voiceCell.setBackgroundColor(new Color(224, 224, 224));
                table.addCell(voiceCell);

                previousVoice = musician.getVoice().getId();
                columnCounter = 0; // Reiniciar el contador de columnas
            }

            // Agregar músico a la celda
            if (Boolean.TRUE.equals(drawCheckbox)) {
                final PdfPCell boxCell = new PdfPCell();
                // boxCell.setFixedHeight(20); // Altura fija del cuadro
                boxCell.setBorderWidth(1); // Grosor del borde
                boxCell.setHorizontalAlignment(Element.ALIGN_CENTER);
                boxCell.setVerticalAlignment(Element.ALIGN_MIDDLE); // Centrado vertical del cuadro
                table.addCell(boxCell);
                columnCounter++;
            }

            final PdfPCell musicianCell = new PdfPCell(new Phrase(musician.getName() + " " + musician.getSurname(), MUSICIAN_FONT));
            musicianCell.setPaddingTop(2);
            musicianCell.setPaddingBottom(4);
            if (Boolean.TRUE.equals(drawCheckbox)) {
                musicianCell.setPaddingLeft(3);
            } else {
                musicianCell.setPaddingLeft(5);
            }
            musicianCell.setHorizontalAlignment(Element.ALIGN_LEFT);
            musicianCell.setVerticalAlignment(Element.ALIGN_MIDDLE);
            table.addCell(musicianCell);

            columnCounter++;

            // Si alcanzamos tres columnas, iniciar una nueva fila
            if (columnCounter % moduleColumnCounter == 0) {
                columnCounter = 0;
            }
        }

        // Completar la fila final si no está llena
        while (columnCounter % moduleColumnCounter != 0) {
            table.addCell(""); // Celdas vacías para completar la fila
            columnCounter++;
        }

        // Agregar la tabla al documento
        document.add(table);
    }


}
