package com.feyconsuelo.application.service.image;

public interface ResizeImageService {

    String resizeImage(String image, int targetSmallestSide, float quality);

}
