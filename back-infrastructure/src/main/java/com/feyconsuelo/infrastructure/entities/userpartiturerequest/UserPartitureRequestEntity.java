package com.feyconsuelo.infrastructure.entities.userpartiturerequest;

import com.feyconsuelo.infrastructure.entities.user.UserEntity;
import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EntityListeners;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import java.io.Serial;
import java.io.Serializable;
import java.time.LocalDateTime;

@Entity
@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@EntityListeners(AuditingEntityListener.class)
@Table(schema = "feyconsuelo", name = "user_partiture_request")
public class UserPartitureRequestEntity implements Serializable {
    @Serial
    private static final long serialVersionUID = 2438351663350375592L;

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;

    @OneToOne(cascade = {CascadeType.PERSIST, CascadeType.MERGE})
    @JoinColumn(name = "username", referencedColumnName = "username")
    private UserEntity user;

    @Column(name = "partiture_request", nullable = false)
    private String partitureRequest;

    @Column(name = "readed", nullable = false)
    private Boolean readed;

    @Column(name = "update_user", nullable = false)
    private String updateUserPartitureRequest;

    @Temporal(TemporalType.TIMESTAMP)
    @Column(name = "update_date", nullable = false)
    @LastModifiedDate
    private LocalDateTime updateDatePartitureRequest;

    @Temporal(TemporalType.TIMESTAMP)
    @Column(name = "delete_date")
    private LocalDateTime deleteDatePartitureRequest;

    @Temporal(TemporalType.TIMESTAMP)
    @Column(name = "creation_date", nullable = false, updatable = false)
    @CreatedDate
    private LocalDateTime createdDatePartitureRequest;

}
