package com.feyconsuelo.infrastructure.entities.user;

import java.time.LocalDateTime;
import java.util.List;


public interface UserMusicianEntity {

    String getusernameUser();

    String getpasswordUser();

    List<String> getrolesUser();

    LocalDateTime getcreatedDateUser();

    LocalDateTime getmodifiedDateUser();

    LocalDateTime getdeleteDateUser();

    String getdescriptionUser();

    String getdirectionUser();

    String getdniUser();

    String getemailUser();

    List<String> getfirebaseTokenUser();

    String getimageThumbnailUser();

    LocalDateTime getlastAccessDateUser();

    String getmunicipalityUser();

    String getnameUser();

    Boolean getpasswordExpiredUser();

    String getphoneNumberUser();

    String getprovinceUser();

    String getsurnameUser();

    Long getidMusician();

    String getdniMusician();

    String getnameMusician();

    String getsurnameMusician();

    String getdirectionMusician();

    String getmunicipalityMusician();

    String getprovinceMusician();

    String getemailMusician();

    String getmodifiedUserMusician();

    LocalDateTime getcreatedDateMusician();

    LocalDateTime getdeleteDateMusician();

    LocalDateTime getmodifiedDateMusician();

    LocalDateTime getbirthDateMusician();

    LocalDateTime getregistrationDateMusician();

    String getinventoryObservationsMusician();

    String getimageThumbnailMusician();

    String getphoneNumberMusician();

    LocalDateTime getunregistrationDateMusician();

    LocalDateTime getdateLastNotificationNonAssistsStreakRehearsalsMusician();

    Boolean getunregistredMusician();

    String getobservationsMusician();

    Long getidVoice();

    LocalDateTime getcreatedDateVoice();

    LocalDateTime getdeleteDateVoice();

    LocalDateTime getmodifiedDateVoice();

    String getmodifiedUserVoice();

    String getimageVoice();

    String getnameVoice();

    Integer getorderVoice();
}
