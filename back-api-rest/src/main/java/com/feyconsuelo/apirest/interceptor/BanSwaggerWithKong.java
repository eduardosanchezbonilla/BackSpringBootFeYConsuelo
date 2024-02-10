package com.feyconsuelo.apirest.interceptor;

import com.feyconsuelo.apirest.dto.common.Headers;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.StringUtils;
import org.springframework.web.servlet.HandlerInterceptor;

public class BanSwaggerWithKong implements HandlerInterceptor {

    @Autowired
    private Headers headers;

    @Override
    public boolean preHandle(final HttpServletRequest request,
                             final HttpServletResponse response, final Object handler) throws Exception {

        if (StringUtils.hasText(this.headers.getConsumerId())) {
            response.getWriter().write("Unauthorized, you should use https://madiva.com/apis/directory");
            response.setStatus(401);
            return false;
        }
        return true;
    }
}