package com.feyconsuelo.apirest.interceptor;

import com.feyconsuelo.apirest.dto.common.Headers;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.web.servlet.HandlerInterceptor;

public class ClientRequestInterceptor implements HandlerInterceptor {

    private final Headers headers;

    public ClientRequestInterceptor(final Headers headers) {
        this.headers = headers;
    }

    @Override
    public boolean preHandle(final HttpServletRequest request,
                             final HttpServletResponse response,
                             final Object handler
    ) throws Exception {
        final String host = request.getHeader(Headers.HOST_HEADER_NAME);
        final String consumerId = request.getHeader(Headers.COMSUMER_ID_HEADER_NAME);
        final String consumerCustomId = request.getHeader(Headers.COMSUMER_CUSTOM_ID_HEADER_NAME);
        final String consumerUsername = request.getHeader(Headers.COMSUMER_USERNAME_HEADER_NAME);
        final String forwardedPhoto = request.getHeader(Headers.X_FORWARDED_PROTO);
        final String forwardedHost = request.getHeader(Headers.X_FORWARDED_HOST);
        final String forwardedPort = request.getHeader(Headers.X_FORWARDED_PORT);
        final String clientIp = request.getHeader(Headers.X_FORWARDED_FOR);
        final String realIp = request.getHeader(Headers.X_REAL_IP);
        final String saveConsulta = request.getHeader(Headers.X_MADIVA_SAVE_CONSULTA);

        this.headers.setHost(host);
        this.headers.setConsumerId(consumerId);
        this.headers.setConsumerCustomId(consumerCustomId);
        this.headers.setConsumerUsername(consumerUsername);
        this.headers.setForwardedProto(forwardedPhoto);
        this.headers.setForwardedHost(forwardedHost);
        this.headers.setForwardedPort(forwardedPort);
        this.headers.setClientIp(clientIp);
        this.headers.setRealIp(realIp);
        this.headers.setSaveConsulta(saveConsulta);

        return true;
    }
}