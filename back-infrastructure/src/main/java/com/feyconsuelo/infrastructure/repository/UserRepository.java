package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.user.UserEntity;
import com.feyconsuelo.infrastructure.entities.user.UserMusicianEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;

public interface UserRepository extends JpaRepository<UserEntity, String> {

    @Query("""
             SELECT userEntity
             FROM UserEntity userEntity
             WHERE userEntity.deleteDate Is Null
             ORDER BY userEntity.username
            """)
    List<UserEntity> findAllActives();

    @Query("""
             SELECT userEntity
             FROM UserEntity userEntity
             WHERE userEntity.deleteDate Is Null
               And userEntity.username = :username
            """)
    Optional<UserEntity> findUserActiveByUserName(String username);

    @Query(value = """
             select
                    u.username as usernameUser,
                    u.password as passwordUser,
                    (
                     Select array_agg(distinct ur.role)
                     From feyconsuelo.user_roles ur
                     Where u.username=ur.username
                    ) as rolesUser,      
                     u.creation_date as createdDateUser,
                     u.update_date as modifiedDateUser,
                     u.delete_date as deleteDateUser,
                     u.description as descriptionUser,
                     u.direction as directionUser,
                     u.dni as dniUser,
                     u.email as emailUser,
                     u.firebase_token as firebaseTokenUser,      
                     u.image_thumbnail as imageThumbnailUser,
                     u.last_access_date as lastAccessDateUser,       
                     u.municipality as municipalityUser,
                     u.name as nameUser,
                     u.password_expired as passwordExpiredUser,
                     u.phonenumber as phoneNumberUser,
                     u.province as provinceUser,
                     u.surname as surnameUser,
                     m.id as idMusician,
                     m.dni as dniMusician,
                     m.name as nameMusician,
                     m.surname as surnameMusician,
                     m.direction as directionMusician,
                     m.municipality as municipalityMusician,
                     m.province as provinceMusician,
                     m.email as emailMusician,
                     v.id as idVoice,
                     v.creation_date as createdDateVoice,
                     v.delete_date as deleteDateVoice,
                     v.image as imageVoice,
                     v.update_date as modifiedDateVoice,
                     v.update_user as modifiedUserVoice,
                     v.name as nameVoice,
                     v.voice_order as orderVoice,
                     m.update_user as modifiedUserMusician,
                     m.creation_date as createdDateMusician, 
                     m.delete_date as deleteDateMusician,
                     m.update_date as modifiedDateMusician,
                     m.birth_date as birthDateMusician,
                     m.registration_date as registrationDateMusician,
                     m.inventory_observations as inventoryObservationsMusician,
                     m.image_thumbnail as imageThumbnailMusician,
                     m.phonenumber as phoneNumberMusician,
                     m.unregistration_date as unregistrationDateMusician,
                     m.date_last_notification_non_assists_streak_rehearsals as dateLastNotificationNonAssistsStreakRehearsalsMusician,
                     m.unregistred as unregistredMusician,
                     m.observations as observationsMusician       
                 from
                     feyconsuelo.user u
                 left join
                     feyconsuelo.musician m
                         on upper(u.username)=upper(m.dni)
                 left join
                     feyconsuelo.voice v
                         on v.id=m.voice_id    
                 where
                     u.delete_date is null
                     and m.delete_date is null
                 order by
                     u.username
            """,
            nativeQuery = true
    )
    List<UserMusicianEntity> findAllActivesWithMusicianData();

}
