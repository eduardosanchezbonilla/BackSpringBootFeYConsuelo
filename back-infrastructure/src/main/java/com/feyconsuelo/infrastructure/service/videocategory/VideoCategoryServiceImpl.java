package com.feyconsuelo.infrastructure.service.videocategory;

import com.feyconsuelo.application.service.videocategory.VideoCategoryService;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.videocategory.VideoCategoryRequest;
import com.feyconsuelo.domain.model.videocategory.VideoCategoryResponse;
import com.feyconsuelo.infrastructure.converter.videocategory.VideoCategoryEntityListToVideoCategoryResponseListConverter;
import com.feyconsuelo.infrastructure.converter.videocategory.VideoCategoryEntityToVideoCategoryResponseConverter;
import com.feyconsuelo.infrastructure.converter.videocategory.VideoCategoryRequestToVideoCategoryEntityConverter;
import com.feyconsuelo.infrastructure.entities.videocategory.VideoCategoryEntity;
import com.feyconsuelo.infrastructure.repository.VideoCategoryRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Slf4j
@Service
@RequiredArgsConstructor
public class VideoCategoryServiceImpl implements VideoCategoryService {

    private final VideoCategoryRepository videoCategoryRepository;
    private final VideoCategoryRequestToVideoCategoryEntityConverter videoCategoryRequestToVideoCategoryEntityConverter;
    private final VideoCategoryEntityListToVideoCategoryResponseListConverter videoCategoryEntityListToVideoCategoryResponseListConverter;
    private final VideoCategoryEntityToVideoCategoryResponseConverter videoCategoryEntityToVideoCategoryResponseConverter;

    @Override
    public void delete(final Long videoCategoryId) {
        this.videoCategoryRepository.deleteById(videoCategoryId);
    }

    @Override
    public void logicalDelete(final Long videoCategoryId) {
        final var voice = this.videoCategoryRepository.findVideoCategoryActiveById(videoCategoryId);

        if (voice.isEmpty()) {
            throw new NotFoundException("No existe la categoria que desea eliminar");
        }

        this.videoCategoryRepository.save(this.videoCategoryRequestToVideoCategoryEntityConverter.deleteEntity(voice.get()));
    }

    @Override
    public List<VideoCategoryResponse> getAll() {
        final List<VideoCategoryEntity> videoCategoryEntityList = this.videoCategoryRepository.findAllActives();
        return this.videoCategoryEntityListToVideoCategoryResponseListConverter.convert(videoCategoryEntityList);
    }

    @Override
    public Optional<VideoCategoryResponse> get(final Long videoCategoryId) {
        final var videoCategory = this.videoCategoryRepository.findVideoCategoryActiveById(videoCategoryId);
        return videoCategory.map(this.videoCategoryEntityToVideoCategoryResponseConverter::convert);
    }

    @Override
    public void insert(final VideoCategoryRequest videoCategoryRequest) {
        this.videoCategoryRepository.save(
                this.videoCategoryRequestToVideoCategoryEntityConverter.convert(videoCategoryRequest)
        );
    }

    @Override
    public void update(final Long videoCategoryId, final VideoCategoryRequest videoCategoryRequest) {
        final var videoCategory = this.videoCategoryRepository.findVideoCategoryActiveById(videoCategoryId);

        if (videoCategory.isEmpty()) {
            throw new NotFoundException("No existe la categoria que desea modificar");
        }

        this.videoCategoryRepository.save(
                this.videoCategoryRequestToVideoCategoryEntityConverter.updateEntity(videoCategory.get(), videoCategoryRequest)
        );
    }

}
