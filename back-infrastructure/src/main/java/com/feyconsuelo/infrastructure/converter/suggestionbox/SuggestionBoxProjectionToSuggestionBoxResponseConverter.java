package com.feyconsuelo.infrastructure.converter.suggestionbox;

import com.feyconsuelo.domain.model.suggestionbox.SuggestionBoxResponse;
import com.feyconsuelo.domain.model.user.UserResponse;
import com.feyconsuelo.infrastructure.entities.suggestionbox.SuggestionBoxProjection;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class SuggestionBoxProjectionToSuggestionBoxResponseConverter {

    public SuggestionBoxResponse convert(final SuggestionBoxProjection suggestionBoxProjection) {
        return SuggestionBoxResponse.builder()
                .id(suggestionBoxProjection.getId())
                .user(
                        UserResponse.builder()
                                .username(suggestionBoxProjection.getUsername())
                                //.password(suggestionBoxProjection.getPassword())
                                /*.roles(
                                        CollectionUtils.isEmpty(userEntity.getRoles()) ?
                                                List.of() :
                                                userEntity.getRoles().stream()
                                                        .map(role -> role.getId().getRole())
                                                        .toList()
                                )*/
                                //.deletedDate(suggestionBoxProjection.getDeleteDate())
                                //.passwordExpired(userEntity.getPasswordExpired())
                                .dni(suggestionBoxProjection.getDni())
                                .name(suggestionBoxProjection.getName())
                                .surname(suggestionBoxProjection.getSurname())
                                .direction(suggestionBoxProjection.getDirection())
                                .municipality(suggestionBoxProjection.getMunicipality())
                                .province(suggestionBoxProjection.getProvince())
                                .email(suggestionBoxProjection.getEmail())
                                .description(suggestionBoxProjection.getDescription())
                                .image(suggestionBoxProjection.getImage())
                                //.firebaseToken(userEntity.getFirebaseToken())
                                //.lastAccessDate(userEntity.getLastAccessDate())
                                .phoneNumber(suggestionBoxProjection.getPhoneNumber())
                                .build()
                )
                .suggestion(suggestionBoxProjection.getSuggestion())
                .readed(suggestionBoxProjection.getReaded())
                .deleteDate(suggestionBoxProjection.getDeleteDate())
                .creationDate(suggestionBoxProjection.getCreationDate())
                .build();
    }

}
