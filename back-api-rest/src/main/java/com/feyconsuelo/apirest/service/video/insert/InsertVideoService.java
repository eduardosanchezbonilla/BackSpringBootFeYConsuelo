package com.feyconsuelo.apirest.service.video.insert;

import com.feyconsuelo.apirest.converter.video.VideoRequestDtoToVideoRequestConverter;
import com.feyconsuelo.domain.usecase.video.InsertVideo;
import com.feyconsuelo.openapi.model.VideoRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class InsertVideoService {

    private final InsertVideo insertVideo;

    private final VideoRequestDtoToVideoRequestConverter videoRequestDtoToVideoRequestConverter;

    public ResponseEntity<Void> insertVideo(final VideoRequestDto videoRequestDto) {
        this.insertVideo.execute(
                this.videoRequestDtoToVideoRequestConverter.convert(videoRequestDto)
        );
        return ResponseEntity.status(HttpStatus.CREATED).build();
    }
}
