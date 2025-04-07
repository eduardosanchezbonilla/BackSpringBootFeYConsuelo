package com.feyconsuelo.infrastructure.converter.user;

import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.model.user.UserMusicianResponse;
import com.feyconsuelo.domain.model.user.UserResponse;
import com.feyconsuelo.domain.model.voice.VoiceResponse;
import com.feyconsuelo.infrastructure.entities.user.UserMusicianEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class UserMusicianEntityToUserMusicianResponseConverter {

    public UserMusicianResponse convert(final UserMusicianEntity userMusicianEntity) {
        return UserMusicianResponse.builder()
                .userResponse(
                        UserResponse.builder()
                                .username(userMusicianEntity.getusernameUser())
                                .password(userMusicianEntity.getpasswordUser())
                                .roles(userMusicianEntity.getrolesUser())
                                .deletedDate(userMusicianEntity.getdeleteDateUser())
                                .passwordExpired(userMusicianEntity.getpasswordExpiredUser())
                                .dni(userMusicianEntity.getdniUser())
                                .name(userMusicianEntity.getnameUser())
                                .surname(userMusicianEntity.getsurnameUser())
                                .direction(userMusicianEntity.getdirectionUser())
                                .municipality(userMusicianEntity.getmunicipalityUser())
                                .province(userMusicianEntity.getprovinceUser())
                                .email(userMusicianEntity.getemailUser())
                                .description(userMusicianEntity.getdescriptionUser())
                                .image(userMusicianEntity.getimageThumbnailUser())
                                .firebaseToken(userMusicianEntity.getfirebaseTokenUser())
                                .lastAccessDate(userMusicianEntity.getlastAccessDateUser())
                                .phoneNumber(userMusicianEntity.getphoneNumberUser())
                                .build()
                )
                .musicianResponse(
                        userMusicianEntity.getidMusician() == null ? null :
                                MusicianResponse.builder()
                                        .id(userMusicianEntity.getidMusician())
                                        .dni(userMusicianEntity.getdniMusician())
                                        .name(userMusicianEntity.getnameMusician())
                                        .surname(userMusicianEntity.getsurnameMusician())
                                        .direction(userMusicianEntity.getdirectionMusician())
                                        .municipality(userMusicianEntity.getmunicipalityMusician())
                                        .province(userMusicianEntity.getprovinceMusician())
                                        .email(userMusicianEntity.getemailMusician())
                                        .voice(
                                                VoiceResponse.builder()
                                                        .id(userMusicianEntity.getidVoice())
                                                        .order(userMusicianEntity.getorderVoice())
                                                        .name(userMusicianEntity.getnameVoice())
                                                        .image(null)
                                                        .deleteDate(userMusicianEntity.getdeleteDateVoice())
                                                        .build()
                                        )
                                        .image(userMusicianEntity.getimageThumbnailMusician())
                                        .deleteDate(userMusicianEntity.getdeleteDateMusician())
                                        .birthDate(userMusicianEntity.getbirthDateMusician())
                                        .registrationDate(userMusicianEntity.getregistrationDateMusician())
                                        .unregistrationDate(userMusicianEntity.getunregistrationDateMusician())
                                        .unregistred(this.getUnregistredMusician(userMusicianEntity))
                                        .dateLastNotificationNonAssistsStreakRehearsals(userMusicianEntity.getdateLastNotificationNonAssistsStreakRehearsalsMusician())
                                        .inventoryObservations(userMusicianEntity.getinventoryObservationsMusician())
                                        .phoneNumber(userMusicianEntity.getphoneNumberMusician())
                                        .observations(userMusicianEntity.getobservationsMusician())
                                        .build()
                )
                .build();
    }

    private Boolean getUnregistredMusician(final UserMusicianEntity userMusicianEntity) {
        return userMusicianEntity.getunregistredMusician() == null ? Boolean.FALSE : userMusicianEntity.getunregistredMusician();
    }

}
