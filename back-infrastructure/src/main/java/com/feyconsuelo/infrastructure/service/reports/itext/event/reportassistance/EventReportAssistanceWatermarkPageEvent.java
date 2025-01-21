package com.feyconsuelo.infrastructure.service.reports.itext.event.reportassistance;

import com.feyconsuelo.domain.exception.FeYConsueloException;
import com.lowagie.text.Image;
import com.lowagie.text.Rectangle;
import com.lowagie.text.pdf.PdfContentByte;
import com.lowagie.text.pdf.PdfGState;
import com.lowagie.text.pdf.PdfPageEventHelper;
import com.lowagie.text.pdf.PdfWriter;

import java.io.IOException;
import java.util.Base64;


public class EventReportAssistanceWatermarkPageEvent extends PdfPageEventHelper {
    private final Image watermarkImage;

    public EventReportAssistanceWatermarkPageEvent(final String base64Image) throws IOException {
        // Decodificar la imagen Base64 y prepararla como marca de agua
        final byte[] imageBytes = Base64.getDecoder().decode(base64Image);
        this.watermarkImage = Image.getInstance(imageBytes);

        // Escalar la imagen para un tamaño razonable
        this.watermarkImage.scaleToFit(550, 550); // Ajusta según sea necesario
    }

    @Override
    public void onEndPage(final PdfWriter writer, final com.lowagie.text.Document document) {
        if (this.watermarkImage != null) {
            // Obtener el tamaño de la página actual
            final Rectangle pageSize = document.getPageSize();

            // Configurar la posición centrada de la marca de agua
            final float x = (pageSize.getWidth() - this.watermarkImage.getScaledWidth()) / 2;
            final float y = (pageSize.getHeight() - this.watermarkImage.getScaledHeight()) / 2;
            this.watermarkImage.setAbsolutePosition(x, y - 50);

            // Obtener el contenido de fondo de la página
            final PdfContentByte content = writer.getDirectContentUnder();

            // Configurar transparencia
            final PdfGState gState = new PdfGState();
            gState.setFillOpacity(0.1f); // 10% de opacidad (más transparente)
            content.setGState(gState);

            // Dibujar la imagen
            try {
                content.addImage(this.watermarkImage);
            } catch (final Exception e) {
                throw new FeYConsueloException("Error al agregar la marca de agua", e);
            }
        }
    }
}
