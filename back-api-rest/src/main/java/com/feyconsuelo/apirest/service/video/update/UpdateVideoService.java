package com.feyconsuelo.apirest.service.video.update;

import com.feyconsuelo.apirest.converter.video.VideoRequestDtoToVideoRequestConverter;
import com.feyconsuelo.domain.usecase.video.UpdateVideo;
import com.feyconsuelo.openapi.model.VideoRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class UpdateVideoService {

    private final UpdateVideo updateVideo;

    private final VideoRequestDtoToVideoRequestConverter videoRequestDtoToVideoRequestConverter;

    public ResponseEntity<Void> updateVideo(final Long videoId,
                                            final VideoRequestDto videoRequestDto) {
        this.updateVideo.execute(
                videoId,
                this.videoRequestDtoToVideoRequestConverter.convert(videoRequestDto)
        );
        return ResponseEntity.status(HttpStatus.OK).build();

    }
}
