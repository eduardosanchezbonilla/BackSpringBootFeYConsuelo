package com.feyconsuelo.infrastructure.converter.contactrequest;

import com.feyconsuelo.domain.model.contactrequest.ContactRequest;
import com.feyconsuelo.infrastructure.entities.contactrequest.ContactRequestEntity;
import com.feyconsuelo.infrastructure.service.security.user.TokenInfoExtractorServiceImpl;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class ContactRequestToContactRequestEntityConverter {

    private final TokenInfoExtractorServiceImpl tokenInfoExtractorService;

    public ContactRequestEntity convert(final ContactRequest contactRequest) {
        return ContactRequestEntity.builder()
                .name(contactRequest.getName())
                .phoneNumber(contactRequest.getPhoneNumber())
                .email(contactRequest.getEmail())
                .message(contactRequest.getMessage())
                .readed(contactRequest.getReaded())
                .updateUserContactRequest(this.tokenInfoExtractorService.getUsername())
                .build();
    }

    public ContactRequestEntity markReadUnreadEntity(final ContactRequestEntity contactRequestEntity,
                                                     final ContactRequest contactRequest
    ) {
        contactRequestEntity.setReaded(contactRequest.getReaded());
        contactRequestEntity.setUpdateUserContactRequest(this.tokenInfoExtractorService.getUsername());

        return contactRequestEntity;
    }

}
