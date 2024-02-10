package com.feyconsuelo.apirest.dto.common;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class Headers {
    public static final String HOST_HEADER_NAME = "host";
    public static final String COMSUMER_ID_HEADER_NAME = "x-consumer-id";
    public static final String COMSUMER_CUSTOM_ID_HEADER_NAME = "x-consumer-custom-id";
    public static final String COMSUMER_USERNAME_HEADER_NAME = "x-consumer-username";
    public static final String COMSUMER_GROUPS = "x-consumer-groups";
    public static final String X_FORWARDED_PROTO = "x-forwarded-proto";
    public static final String X_FORWARDED_HOST = "x-forwarded-host";
    public static final String X_FORWARDED_PORT = "x-forwarded-port";
    public static final String X_FORWARDED_FOR = "x-forwarded-for";
    public static final String X_REAL_IP = "x-real-ip";
    public static final String X_MADIVA_SAVE_CONSULTA = "x-madiva-save-consulta";

    private String consumerId;
    private String consumerCustomId;
    private String consumerUsername;
    private String userName;
    private String companyName;
    private String consumerGroups;
    private String clientIp;
    private String forwardedProto;
    private String host;
    private String forwardedHost;
    private String forwardedPort;
    private String realIp;
    private String saveConsulta;

}
