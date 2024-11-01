package com.feyconsuelo.domain.usecase.userpartiturerequest;

import com.feyconsuelo.domain.model.userpartiturerequest.UserRequestPartitureGroupByUserResponse;

import java.util.List;

public interface GetAllRequestPartitureGroupByUser {

    List<UserRequestPartitureGroupByUserResponse> execute(Boolean all);

}
