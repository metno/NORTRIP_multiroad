subroutine lambert2lb2_uEMEP(x,y,gl,gb,projection_attributes)
  
    implicit none
    double precision, intent(in) :: projection_attributes(10)
    real, intent(in) ::x,y
    real, intent(out)::gl,gb
    real ::r,t
    real ::PI
    real :: earth_radius,k,F,y0
    real deg2rad,rad2deg,k_lambert,lat0_lambert
    real lat0
    real lat_stand1_lambert,lat_stand2_lambert,lon0,lat0_in

    
    lat_stand1_lambert=projection_attributes(1)
    lat_stand2_lambert=projection_attributes(2)
    lon0=projection_attributes(3)
    lat0_in=projection_attributes(4)
    earth_radius = projection_attributes(5)

    PI=3.14159265358979323
    deg2rad=PI/180.
    rad2deg=180./PI

    if (lat_stand1_lambert.eq.lat_stand2_lambert) then
        k_lambert=sin(PI/180.*lat0_in)
    else
        k_lambert = log(cos(deg2rad*lat_stand1_lambert)/cos(deg2rad*lat_stand2_lambert))/&
        (log(tan(0.25*PI+0.5*deg2rad*lat_stand2_lambert)/tan(0.25*PI+0.5*deg2rad*lat_stand1_lambert)))
    endif

    lat0_lambert = rad2deg*asin(k_lambert)  
    lat0=lat0_in
    k=k_lambert

    F = earth_radius*cos(PI/180.*lat_stand1_lambert) * (tan(PI/4.+PI/360.*lat_stand1_lambert)**k) /k
    !k = sin(PI/180.*lat0)
    !F = earth_radius*cos(PI/180.*lat0) * (tan(PI/4+PI/360.*lat0)**k) /k
    y0 = F*tan(PI/4.-PI/360.*lat0)**k   
    r = sqrt(x*x+(y0-y)*(y0-y))
    t = atan(x/(y0-y))
    gb = 2*180./PI*atan((F/r)**(1.0/k))-90.0
    gl = lon0 + 180./PI*t/k
    
end subroutine lambert2lb2_uEMEP

subroutine lb2lambert_uEMEP(x,y,gl,gb,lon0,lat0)
    
    implicit none
    real, intent(in) ::gl,gb,lon0,lat0
    real, intent(out)::x,y
    real ::r
    real ::PI
    real :: earth_radius,k,F,y0
    real rad2deg
    
    PI=3.14159265358979323
    earth_radius = 6371000.
    rad2deg=PI/180.
    
    k = sin(PI/180.*lat0)
    F = earth_radius*cos(PI/180.*lat0) * (tan(PI/4.+PI/360.*lat0)**k) /k
    y0 = F*tan(PI/4.-PI/360.*lat0)**k
    r = F*tan(PI/4.-PI/360.*gb)**k
    x = r*sin(PI/180.*k*(gl-lon0))
    y = y0 - r*cos(PI/180.*k*(gl-lon0))
    
    end subroutine lb2lambert_uEMEP
    
subroutine lb2lambert2_uEMEP(x,y,gl,gb,projection_attributes)
    
    implicit none
    double precision, intent(in) :: projection_attributes(10)
    real, intent(in) ::gl,gb
    real, intent(out)::x,y
    real ::r
    real ::PI
    real :: earth_radius,k,F,y0
    real deg2rad,rad2deg,k_lambert,lat0_lambert
    real lat0
    real lat_stand1_lambert,lat_stand2_lambert,lon0,lat0_in
    
    lat_stand1_lambert=projection_attributes(1)
    lat_stand2_lambert=projection_attributes(2)
    lon0=projection_attributes(3)
    lat0_in=projection_attributes(4)
    earth_radius = projection_attributes(5)
    PI=3.14159265358979323
    deg2rad=PI/180.
    rad2deg=180./PI
    
    if (lat_stand1_lambert.eq.lat_stand2_lambert) then
        k_lambert=sin(PI/180.*lat0_in)
    else
        k_lambert = log(cos(deg2rad*lat_stand1_lambert)/cos(deg2rad*lat_stand2_lambert))/&
        (log(tan(0.25*PI+0.5*deg2rad*lat_stand2_lambert)/tan(0.25*PI+0.5*deg2rad*lat_stand1_lambert)))
    endif
    
    lat0_lambert = rad2deg*asin(k_lambert)  
    !lat0=lat0_lambert
    lat0=lat0_in
    k=k_lambert
    !write(*,*) lat_stand1_lambert,lat_stand2_lambert,k,lat0_lambert,lat0_in
    !k = sin(PI/180.*lat0)
    F = earth_radius*cos(PI/180.*lat_stand1_lambert) * (tan(PI/4.+PI/360.*lat_stand1_lambert)**k) /k
    y0 = F*tan(PI/4.-PI/360.*lat0)**k
    r = F*tan(PI/4.-PI/360.*gb)**k
    x = r*sin(PI/180.*k*(gl-lon0))
    y = y0 - r*cos(PI/180.*k*(gl-lon0))
    
end subroutine lb2lambert2_uEMEP