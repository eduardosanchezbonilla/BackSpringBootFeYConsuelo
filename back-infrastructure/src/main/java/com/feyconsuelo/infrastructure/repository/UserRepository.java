package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.user.UserEntity;
import com.feyconsuelo.infrastructure.entities.user.UserMusicianEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;

public interface UserRepository extends JpaRepository<UserEntity, String> {

    @Query("""
             SELECT userEntity
             FROM UserEntity userEntity
             WHERE userEntity.deleteDate Is Null
             ORDER BY userEntity.username
            """)
    List<UserEntity> findAllActives();

    @Query("""
             SELECT userEntity
             FROM UserEntity userEntity
             WHERE userEntity.deleteDate Is Null
               And userEntity.username = :username
            """)
    Optional<UserEntity> findUserActiveByUserName(String username);

    @Query("""
             SELECT new com.feyconsuelo.infrastructure.entities.user.UserMusicianEntity(userEntity, musicianEntity)
             FROM UserEntity userEntity
             LEFT JOIN MusicianEntity musicianEntity ON upper(userEntity.username) = upper(musicianEntity.dni)
             WHERE userEntity.deleteDate Is Null
             ORDER BY userEntity.username
            """)
    List<UserMusicianEntity> findAllActivesWithMusicianData();
}
