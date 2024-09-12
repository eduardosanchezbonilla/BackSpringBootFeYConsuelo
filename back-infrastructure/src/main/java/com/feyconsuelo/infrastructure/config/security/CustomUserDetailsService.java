package com.feyconsuelo.infrastructure.config.security;

import com.feyconsuelo.domain.model.user.UserResponse;
import com.feyconsuelo.infrastructure.service.user.UserServiceImpl;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Service
@RequiredArgsConstructor
public class CustomUserDetailsService implements UserDetailsService {

    private final UserServiceImpl userService;

    @Override
    public UserDetails loadUserByUsername(final String username) throws UsernameNotFoundException {
        final UserResponse userResponse = this.userService.get(username)
                .orElseThrow(() -> new UsernameNotFoundException("User Not Found"));


        return new org.springframework.security.core.userdetails.User(
                userResponse.getUsername(),
                userResponse.getPassword(),
                CollectionUtils.isEmpty(userResponse.getRoles()) ?
                        List.of() :
                        //user.getRoles().stream().map(SimpleGrantedAuthority::new).collect(Collectors.toList())
                        userResponse.getRoles().stream().map(role -> new SimpleGrantedAuthority("ROLE_" + role)).toList()
        );
    }
}