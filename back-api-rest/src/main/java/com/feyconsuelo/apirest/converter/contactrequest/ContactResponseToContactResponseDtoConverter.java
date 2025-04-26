package com.feyconsuelo.apirest.converter.contactrequest;

import com.feyconsuelo.application.service.utils.DateService;
import com.feyconsuelo.domain.model.contactrequest.ContactResponse;
import com.feyconsuelo.openapi.model.ContactResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.time.format.DateTimeFormatter;

@Slf4j
@Component
@RequiredArgsConstructor
public class ContactResponseToContactResponseDtoConverter {

    private static final String TIME_FORMAT = "dd/MM/yyyy HH:mm";
    private final DateService dateService;

    public ContactResponseDto convert(final ContactResponse contactResponse) {
        return ContactResponseDto.builder()
                .id(contactResponse.getId())
                .name(contactResponse.getName())
                .phoneNumber(contactResponse.getPhoneNumber())
                .email(contactResponse.getEmail())
                .message(contactResponse.getMessage())
                .readed(contactResponse.getReaded())
                .createdTime(this.dateService.dateToString(contactResponse.getCreatedDate(), DateTimeFormatter.ofPattern(TIME_FORMAT)))
                .build();
    }

}
