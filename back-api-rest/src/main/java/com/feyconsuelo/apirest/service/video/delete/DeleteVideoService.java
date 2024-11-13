package com.feyconsuelo.apirest.service.video.delete;

import com.feyconsuelo.domain.usecase.video.DeleteVideo;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class DeleteVideoService {

    private final DeleteVideo deleteVideo;

    public ResponseEntity<Void> deleteVideo(final Long videoId) {
        this.deleteVideo.execute(videoId);
        return ResponseEntity.status(HttpStatus.OK).build();
    }

}
