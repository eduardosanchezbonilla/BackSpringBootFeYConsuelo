package com.feyconsuelo.infrastructure.converter.contactrequest;

import com.feyconsuelo.domain.model.contactrequest.ContactResponse;
import com.feyconsuelo.infrastructure.entities.contactrequest.ContactRequestEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class ContactRequestEntityListToContactResponseListConverter {

    private final ContactRequestEntityToContactResponseConverter contactRequestEntityToContactResponseConverter;

    public List<ContactResponse> convert(final List<ContactRequestEntity> contactRequestEntityList) {
        if (CollectionUtils.isEmpty(contactRequestEntityList)) {
            return List.of();
        }
        return contactRequestEntityList.stream()
                .map(this.contactRequestEntityToContactResponseConverter::convert)
                .toList();
    }
}
