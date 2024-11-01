package com.feyconsuelo.application.service.userpartiturerequest;

import com.feyconsuelo.domain.model.userpartiturerequest.UserRequestPartitureRequest;
import com.feyconsuelo.domain.model.userpartiturerequest.UserRequestPartitureResponse;

import java.util.List;

public interface UserPartitureRequestService {

    void insert(UserRequestPartitureRequest userRequestPartitureRequest);

    void logicalDelete(UserRequestPartitureRequest userRequestPartitureRequest);

    List<UserRequestPartitureResponse> getAllUserPartitureRequest();

    List<UserRequestPartitureResponse> getAllUserPartitureRequestByUser(String username);

    void markReadUnread(UserRequestPartitureRequest userRequestPartitureRequest);

}
