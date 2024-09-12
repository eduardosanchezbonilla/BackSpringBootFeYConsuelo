package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.user.UserEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;

public interface UserRepository extends JpaRepository<UserEntity, String> {

    @Query("""
             SELECT userRequest
             FROM UserEntity userRequest
             WHERE userRequest.deleteDate Is Null
             ORDER BY userRequest.username
            """)
    List<UserEntity> findAllActives();

    @Query("""
             SELECT userRequest
             FROM UserEntity userRequest
             WHERE userRequest.deleteDate Is Null
               And userRequest.username = :username
            """)
    Optional<UserEntity> findUserActiveByUserName(String username);
}
