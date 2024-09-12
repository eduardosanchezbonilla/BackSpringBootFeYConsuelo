package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.voice.VoiceEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;

public interface VoiceRepository extends JpaRepository<VoiceEntity, Long> {

    @Query("""
             SELECT voiceRequest
             FROM VoiceEntity voiceRequest
             WHERE voiceRequest.deleteDate Is Null
             ORDER BY voiceRequest.id
            """)
    List<VoiceEntity> findAllActives();

    @Query("""
             SELECT voiceRequest
             FROM VoiceEntity voiceRequest
             WHERE voiceRequest.deleteDate Is Null
               And voiceRequest.id = :voiceId
            """)
    Optional<VoiceEntity> findVoiceActiveById(Long voiceId);

}
