package com.feyconsuelo.domain.model.userpartiturerequest;

import com.feyconsuelo.domain.model.user.UserResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode
public class UserRequestPartitureGroupByUserResponse {
    private UserResponse user;
    private List<UserRequestPartitureResponse> request;

}
