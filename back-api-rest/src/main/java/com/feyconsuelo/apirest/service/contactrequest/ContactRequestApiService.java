package com.feyconsuelo.apirest.service.contactrequest;

import com.feyconsuelo.apirest.service.contactrequest.insert.InsertContactRequestService;
import com.feyconsuelo.apirest.service.contactrequest.markreadunread.MarkReadUnreadContactRequestService;
import com.feyconsuelo.apirest.service.contactrequest.query.GetContactRequestService;
import com.feyconsuelo.domain.model.contactrequest.ContactRequest;
import com.feyconsuelo.openapi.api.ContactRequestControllerApiDelegate;
import com.feyconsuelo.openapi.model.ContactRequestDto;
import com.feyconsuelo.openapi.model.ContactResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class ContactRequestApiService implements ContactRequestControllerApiDelegate {

    private final InsertContactRequestService insertContactRequestService;
    private final GetContactRequestService getContactRequestService;
    private final MarkReadUnreadContactRequestService markReadUnreadContactRequestService;

    @Override
    public ResponseEntity<Void> insertContactRequestPetition(final ContactRequestDto contactRequestDto) {
        return this.insertContactRequestService.insertContactRequest(
                ContactRequest.builder()
                        .name(contactRequestDto.getName())
                        .phoneNumber(contactRequestDto.getPhoneNumber())
                        .email(contactRequestDto.getEmail())
                        .message(contactRequestDto.getMessage())
                        .readed(contactRequestDto.getReaded())
                        .build()
        );
    }

    @Override
    public ResponseEntity<List<ContactResponseDto>> getAllContactRequest(final Boolean all) {
        return this.getContactRequestService.getAllContactRequests(all);
    }

    @Override
    public ResponseEntity<Void> markReadUnreadContactRequest(final ContactRequestDto contactRequestDto) {
        return this.markReadUnreadContactRequestService.markReadUnread(
                ContactRequest.builder()
                        .id(contactRequestDto.getId())
                        .name(contactRequestDto.getName())
                        .phoneNumber(contactRequestDto.getPhoneNumber())
                        .email(contactRequestDto.getEmail())
                        .message(contactRequestDto.getMessage())
                        .readed(contactRequestDto.getReaded())
                        .build()
        );
    }

}
