package com.feyconsuelo.infrastructure.service.image;

import com.feyconsuelo.application.service.image.ResizeImageService;
import com.feyconsuelo.domain.exception.FeYConsueloException;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import javax.imageio.IIOImage;
import javax.imageio.ImageIO;
import javax.imageio.ImageWriteParam;
import javax.imageio.ImageWriter;
import javax.imageio.stream.ImageOutputStream;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.Base64;

@Component
@RequiredArgsConstructor
public class ResizeImageServiceImpl implements ResizeImageService {

    private BufferedImage resizeWithHighQuality(final BufferedImage originalImage, final int width, final int height) {
        final BufferedImage resizedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);

        final Graphics2D g2d = resizedImage.createGraphics();
        g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC);  // Interpolación bicúbica
        g2d.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);  // Renderización de alta calidad
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);  // Antialiasing

        g2d.drawImage(originalImage, 0, 0, width, height, null);
        g2d.dispose();

        return resizedImage;
    }

    @Override
    public String resizeImage(final String base64Image, final int targetSmallestSide, final float quality) {
        // 1. Decodificar Base64 a bytes
        final byte[] imageBytes = Base64.getDecoder().decode(base64Image);

        try (final ByteArrayInputStream bis = new ByteArrayInputStream(imageBytes);
             final ByteArrayOutputStream baos = new ByteArrayOutputStream()
        ) {
            // 2. Convertir los bytes en un BufferedImage
            final BufferedImage originalImage = ImageIO.read(bis);

            // 3. Calcular el nuevo tamaño manteniendo la relación de aspecto
            final int originalWidth = originalImage.getWidth();
            final int originalHeight = originalImage.getHeight();

            final int newWidth;
            final int newHeight;

            // Determinar si el ancho o la altura es el lado más pequeño
            if (originalWidth < originalHeight) {
                // El ancho es el lado más pequeño, ajustamos el ancho al tamaño objetivo
                newWidth = targetSmallestSide;
                newHeight = (int) ((double) originalHeight / originalWidth * newWidth);
            } else {
                // La altura es el lado más pequeño, ajustamos la altura al tamaño objetivo
                newHeight = targetSmallestSide;
                newWidth = (int) ((double) originalWidth / originalHeight * newHeight);
            }

            // 4. Redimensionar la imagen manteniendo la relación de aspecto
            final BufferedImage resizedImage = this.resizeWithHighQuality(originalImage, newWidth, newHeight);

            // 5. Ajustar la compresión y guardar la imagen con un nivel de calidad reducido (para JPEG)
            final ImageWriter jpgWriter = ImageIO.getImageWritersByFormatName("jpg").next();
            final ImageWriteParam jpgWriteParam = jpgWriter.getDefaultWriteParam();

            // Solo para JPEG: habilitar compresión y establecer el nivel de calidad
            if (jpgWriteParam.canWriteCompressed()) {
                jpgWriteParam.setCompressionMode(ImageWriteParam.MODE_EXPLICIT);
                jpgWriteParam.setCompressionQuality(quality);  // calidad entre 0 (peor) y 1 (mejor)
            }

            // Escribir la imagen redimensionada y comprimida
            try (final ImageOutputStream ios = ImageIO.createImageOutputStream(baos)) {
                jpgWriter.setOutput(ios);
                jpgWriter.write(null, new IIOImage(resizedImage, null, null), jpgWriteParam);
            }

            jpgWriter.dispose();

            // Convertir la imagen comprimida de vuelta a Base64
            final byte[] resizedImageBytes = baos.toByteArray();
            return Base64.getEncoder().encodeToString(resizedImageBytes);
        } catch (final Exception e) {
            throw new FeYConsueloException(e);
        }
    }
}
