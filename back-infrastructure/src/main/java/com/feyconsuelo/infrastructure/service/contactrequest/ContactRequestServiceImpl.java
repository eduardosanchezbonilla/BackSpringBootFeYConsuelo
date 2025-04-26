package com.feyconsuelo.infrastructure.service.contactrequest;

import com.feyconsuelo.application.service.contactrequest.ContactRequestService;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.contactrequest.ContactRequest;
import com.feyconsuelo.domain.model.contactrequest.ContactResponse;
import com.feyconsuelo.infrastructure.converter.contactrequest.ContactRequestEntityListToContactResponseListConverter;
import com.feyconsuelo.infrastructure.converter.contactrequest.ContactRequestToContactRequestEntityConverter;
import com.feyconsuelo.infrastructure.entities.contactrequest.ContactRequestEntity;
import com.feyconsuelo.infrastructure.repository.ContactRequestRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class ContactRequestServiceImpl implements ContactRequestService {

    private final ContactRequestRepository contactRequestRepository;
    private final ContactRequestToContactRequestEntityConverter contactRequestToContactRequestEntityConverter;
    private final ContactRequestEntityListToContactResponseListConverter contactRequestEntityListToContactResponseListConverter;

    @Override
    public void insert(final ContactRequest contactRequest) {
        this.contactRequestRepository.save(
                this.contactRequestToContactRequestEntityConverter.convert(contactRequest)
        );
    }

    @Override
    public List<ContactResponse> getAllContactRequest() {
        final List<ContactRequestEntity> contactRequestEntityList = this.contactRequestRepository.findAllActives();
        return this.contactRequestEntityListToContactResponseListConverter.convert(contactRequestEntityList);
    }

    @Override
    public void markReadUnread(final ContactRequest contactRequest) {
        final var contactRequestEntity = this.contactRequestRepository.findActiveById(contactRequest.getId());

        if (contactRequestEntity.isEmpty()) {
            throw new NotFoundException("No existe el registro que intenta actualizar");
        }

        this.contactRequestRepository.save(
                this.contactRequestToContactRequestEntityConverter.markReadUnreadEntity(
                        contactRequestEntity.get(),
                        contactRequest
                )
        );
    }

}
