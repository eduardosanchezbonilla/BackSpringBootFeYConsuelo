package com.feyconsuelo.infrastructure.converter.contactrequest;

import com.feyconsuelo.domain.model.contactrequest.ContactResponse;
import com.feyconsuelo.infrastructure.entities.contactrequest.ContactRequestEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class ContactRequestEntityToContactResponseConverter {

    public ContactResponse convert(final ContactRequestEntity contactRequestEntity) {
        return ContactResponse.builder()
                .id(contactRequestEntity.getId())
                .name(contactRequestEntity.getName())
                .phoneNumber(contactRequestEntity.getPhoneNumber())
                .email(contactRequestEntity.getEmail())
                .message(contactRequestEntity.getMessage())
                .readed(contactRequestEntity.getReaded())
                .createdDate(contactRequestEntity.getCreatedDateContactRequest())
                .build();
    }


}
