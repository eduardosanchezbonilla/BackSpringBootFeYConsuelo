package com.feyconsuelo.application.usecase.image;

import com.feyconsuelo.application.service.image.ResizeImageService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
@Slf4j
public class ResizeImageImpl {

    private final ResizeImageService resizeImageService;

    public String resizeImage(final String image, int targetSmallestSide, float quality) {

        try {
            return this.resizeImageService.resizeImage(image, targetSmallestSide, quality);
        } catch (final Exception e) {
            log.error("Error resizing image: {}", e.getMessage());
            return image;
        }

    }

    public String resizeImage(final String image) {
        try {
            return this.resizeImageService.resizeImage(image, 100, 0.7f);
        } catch (final Exception e) {
            log.error("Error resizing image: {}", e.getMessage());
            return image;
        }
    }
}
