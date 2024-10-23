package com.feyconsuelo.infrastructure.service.voice;

import com.feyconsuelo.application.service.voice.VoiceService;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.voice.VoiceRequest;
import com.feyconsuelo.domain.model.voice.VoiceResponse;
import com.feyconsuelo.infrastructure.converter.voice.VoiceEntityListToVoiceResponseListConverter;
import com.feyconsuelo.infrastructure.converter.voice.VoiceEntityToVoiceResponseConverter;
import com.feyconsuelo.infrastructure.converter.voice.VoiceRequestToVoiceEntityConverter;
import com.feyconsuelo.infrastructure.entities.voice.VoiceEntity;
import com.feyconsuelo.infrastructure.repository.VoiceRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Slf4j
@Service
@RequiredArgsConstructor
public class VoiceServiceImpl implements VoiceService {

    private final VoiceRepository voiceRepository;
    private final VoiceRequestToVoiceEntityConverter voiceRequestToVoiceEntityConverter;
    private final VoiceEntityListToVoiceResponseListConverter voiceEntityListToVoiceResponseListConverter;
    private final VoiceEntityToVoiceResponseConverter voiceEntityToVoiceResponseConverter;

    @Override
    public void delete(final Long voiceId) {
        this.voiceRepository.deleteById(voiceId);
    }

    @Override
    public void logicalDelete(final Long voiceId) {

        final var voice = this.voiceRepository.findVoiceActiveById(voiceId);

        if (voice.isEmpty()) {
            throw new NotFoundException("No existe la voz qeu desea eliminar");
        }

        this.voiceRepository.save(this.voiceRequestToVoiceEntityConverter.deleteEntity(voice.get()));
    }

    @Override
    public List<VoiceResponse> getAll() {
        final List<VoiceEntity> voices = this.voiceRepository.findAllActives();
        return this.voiceEntityListToVoiceResponseListConverter.convert(voices);
    }

    @Override
    public Optional<VoiceResponse> get(final Long voiceId) {
        final var voice = this.voiceRepository.findVoiceActiveById(voiceId);
        return voice.map(voiceEntity -> this.voiceEntityToVoiceResponseConverter.convert(voiceEntity, Boolean.TRUE));
    }

    @Override
    public void insert(final VoiceRequest voiceRequest) {
        this.voiceRepository.save(
                this.voiceRequestToVoiceEntityConverter.convert(voiceRequest)
        );
    }

    @Override
    public void update(final Long voiceId,
                       final VoiceRequest voiceRequest) {

        final var voice = this.voiceRepository.findVoiceActiveById(voiceId);

        if (voice.isEmpty()) {
            throw new NotFoundException("No existe la voz que desea modificar");
        }

        voice.get().setOrder(voiceRequest.getOrder());
        voice.get().setName(voiceRequest.getName());
        voice.get().setImage(voiceRequest.getImage());
        this.voiceRepository.save(
                this.voiceRequestToVoiceEntityConverter.updateEntity(voice.get(), voiceRequest)
        );
    }

}
