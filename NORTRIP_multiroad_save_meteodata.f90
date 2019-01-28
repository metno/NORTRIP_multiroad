!NORTRIP_multiroad_save_meteodata.f90
    
    subroutine NORTRIP_multiroad_create_meteodata
    
    use NORTRIP_multiroad_index_definitions
    
    implicit none
    
    integer i,j,t,jj,ii,j_obs,j_mod
    integer unit_in
    integer, allocatable :: grid_index_rl(:,:)
    integer, allocatable :: grid_index_rl2(:,:)
    real meteo_temp(num_var_meteo)
    double precision time_temp,time_temp2
    integer exists
    integer date_nc(num_date_index,end_dim_nc(time_index))
    integer local_date_nc(num_date_index,end_dim_nc(time_index))
    integer start_time_index_nc,end_time_index_nc,hours_time_index_nc
    logical start_time_index_nc_found,end_time_index_nc_found
    integer out_of_range_count
    logical not_shown_once
    real adjust_lapse
    
    real dist,dist_min
    integer i_dist_min,j_dist_min
    integer k
    real, allocatable :: dist_array_nc(:,:)
    real, allocatable :: dist_array_nc2(:,:)
    real dgrid_lat,dgrid_lon
    real x_temp,y_temp
    real y_utm_temp,x_utm_temp
    
    integer x_index_temp,y_index_temp
    
    real weighting_nc,sum_weighting_nc
    real xpos_min,xpos_max,ypos_min,ypos_max
    real xpos_area_min,xpos_area_max,ypos_area_min,ypos_area_max
    integer i_nc,j_nc
    real xpos_limit,ypos_limit
    logical :: show_analysis=.false.
    logical some_meteo_nc2_available
    
    !Functions
    real DIRECTION
    double precision date_to_number
 
    !zip line commands
    !To extract
    !C:\Users\brucerd\Downloads\7za e -aoa NORTRIP_ALLROADS_2014110301_meteorology.zip
    !To compress
    !C:\Users\brucerd\Downloads\7za a -tzip test.zip NORTRIP_ALLROADS_metadata.txt (-sdel to delete the files)
    !Command line
    !CALL EXECUTE_COMMAND_LINE (command [,wait,exitstat,cmdstat,cmdmsg]). Set wait=T, 
    !7za.exe is palced in C:\Windows\SysWOW64\ and C:\Windows\System32\
    !Downloaded from http://originaldll.com/file/7za.exe/24132.html
    

	write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Creating multiroad meteorology file (NORTRIP_multiroad_create_meteodata)'
	write(unit_logfile,'(A)') '================================================================'
    
    allocate (meteo_output(num_var_meteo,n_hours_input,n_save_links))
    allocate (meteo_obs_ID_output(n_save_links))
        
    !Attribute a grid index to each road link
    allocate (grid_index_rl(2,n_roadlinks))
    allocate (grid_index_rl2(2,n_roadlinks))
    allocate (dist_array_nc(dim_length_nc(x_index),dim_length_nc(y_index)))
    allocate (dist_array_nc2(dim_length_nc2(x_index2),dim_length_nc2(y_index2)))
    
    out_of_range_count=0
    
    !Check to see if any alternative meteo data is available
    some_meteo_nc2_available=.false.
    if (replace_meteo_with_yr.eq.1) then
        do t=1,n_hours_input
            if (meteo_nc2_available(t)) some_meteo_nc2_available=.true.
        enddo
    endif
    
    
    write(unit_logfile,'(a)') ' Matching meteo grids to road links '

    do k=1,n_save_links
        i=save_links(k)
        !This actually means that we have an x,y coordinate system that is the same for both road links and meteogrids
        if (index(meteo_data_type,'bedre byluft').gt.0) then
            grid_index_rl(x_index,i)=1+int((inputdata_rl(x0_rl_index,i)-var1d_nc(x_index,dim_start_nc(x_index))-dgrid_nc(x_index)/2) &
                /dgrid_nc(x_index))
            grid_index_rl(y_index,i)=1+int((inputdata_rl(y0_rl_index,i)-var1d_nc(y_index,dim_start_nc(y_index))-dgrid_nc(y_index)/2) &
                /dgrid_nc(y_index))
            !write(unit_logfile,'(a12,4i12)') ' Index (i,j) = ',i,rl_id(i),grid_index_rl(x_index,i),grid_index_rl(y_index,i)
            if (grid_index_rl(x_index,i).gt.dim_length_nc(x_index).or.grid_index_rl(x_index,i).lt.dim_start_nc(x_index).or. &
                grid_index_rl(y_index,i).gt.dim_length_nc(y_index).or.grid_index_rl(y_index,i).lt.start_dim_nc(y_index)) then
                out_of_range_count=out_of_range_count+1           
            endif
        
            !If out of range set the meteo grid data used to the nearest edge grid
            if (grid_index_rl(x_index,i).gt.dim_length_nc(x_index).or.grid_index_rl(x_index,i).lt.dim_start_nc(x_index)) then
                !write(unit_logfile,'(a,4i12)') ' WARNING: Index out of range (i,j) = ',i,inputdata_int_rl(id_rl_index,i),grid_index_rl(x_index,i),grid_index_rl(y_index,i)
                grid_index_rl(x_index,i)=min(grid_index_rl(x_index,i),dim_length_nc(x_index))
                grid_index_rl(x_index,i)=max(grid_index_rl(x_index,i),dim_start_nc(x_index))            
            endif
            if (grid_index_rl(y_index,i).gt.dim_length_nc(y_index).or.grid_index_rl(y_index,i).lt.start_dim_nc(y_index)) then
                !write(unit_logfile,'(a,4i12)') ' WARNING: Index out of range (i,j) = ',i,inputdata_int_rl(id_rl_index,i),grid_index_rl(x_index,i),grid_index_rl(y_index,i)
                grid_index_rl(y_index,i)=min(grid_index_rl(y_index,i),dim_length_nc(y_index))
                grid_index_rl(y_index,i)=max(grid_index_rl(y_index,i),dim_start_nc(y_index))            
            endif
        
        !This actually means we do not have an x,y coodinate system and we 'approximate' the lat lon assuming the x,y grid is roughly in a N-S direction
        elseif (index(meteo_data_type,'arome').gt.0.or.index(meteo_data_type,'metcoop').gt.0) then
            !loop through all grids to find the nearest in lat lon
            !This method is very inneffective for large numbers of links. Another way must be found
            !i_dist_min=0
            !j_dist_min=0
            !dist_min=1.e32
            !do ii=1,dim_length_nc(x_index)
            !do jj=1,dim_length_nc(y_index)
            !    dist=sqrt((var2d_nc(lat_index,ii,jj)-inputdata_rl(lat0_rl_index,i))**2+(var2d_nc(lon_index,ii,jj)/cos(var2d_nc(lat_index,ii,jj)/180.*3.14159)-inputdata_rl(lon0_rl_index,i)/cos(inputdata_rl(lat0_rl_index,i)/180.*3.14159))**2)
            !    if (dist.lt.dist_min) then
            !        dist_min=dist
            !        i_dist_min=ii
            !        j_dist_min=jj
            !    endif              
            !enddo
            !enddo
            
            !grid_index_rl(x_index,i)=i_dist_min
            !grid_index_rl(y_index,i)=j_dist_min
            !write(*,*) i,grid_index_rl(x_index,i),grid_index_rl(y_index,i)
            !This method is also not very efficient. Still has to find the minimum in a loop for each road
                !grid_index_rl(x_index:y_index,i)=minloc((var2d_nc(lat_index,:,:)-inputdata_rl(lat0_rl_index,i))**2+(var2d_nc(lon_index,:,:)/cos(var2d_nc(lat_index,:,:)/180.*3.14159)-inputdata_rl(lon0_rl_index,i)/cos(inputdata_rl(lat0_rl_index,i)/180.*3.14159))**2)
                !write(*,*) k,i,grid_index_rl(x_index,i),grid_index_rl(y_index,i),110*minval(sqrt((var2d_nc(lat_index,:,:)-inputdata_rl(lat0_rl_index,i))**2+(var2d_nc(lon_index,:,:)/cos(var2d_nc(lat_index,:,:)/180.*3.14159)-inputdata_rl(lon0_rl_index,i)/cos(inputdata_rl(lat0_rl_index,i)/180.*3.14159))**2))          
            
            !This method should work for any roughly north south projection but is not 'exact'. Can be out by a grid
            !It estimates the lat-lon grid spacing at the lat lon position with two iterations
            !Better would have been to do the projection but this is considered good enough and more general
            !Estimate the grid size of the meteo grid in lat lon by taking the central position
            dgrid_lat=(var2d_nc(lat_index,dim_length_nc(x_index)/2,dim_length_nc(y_index))-var2d_nc(lat_index,dim_length_nc(x_index)/2,start_dim_nc(y_index)))/(dim_length_nc(y_index)-1)
            dgrid_lon=(var2d_nc(lon_index,dim_length_nc(x_index),dim_length_nc(y_index)/2)-var2d_nc(lon_index,start_dim_nc(x_index),dim_length_nc(y_index)/2))/(dim_length_nc(x_index)-1)
            
            grid_index_rl(x_index,i)=min(dim_length_nc(x_index),max(1,1+floor((inputdata_rl(lon0_rl_index,i)-var2d_nc(lon_index,start_dim_nc(x_index),dim_length_nc(y_index)/2))/dgrid_lon+0.5)))
            grid_index_rl(y_index,i)=min(dim_length_nc(y_index),max(1,1+floor((inputdata_rl(lat0_rl_index,i)-var2d_nc(lat_index,grid_index_rl(x_index,i),start_dim_nc(y_index)))/dgrid_lat+0.5)))
            grid_index_rl(x_index,i)=min(dim_length_nc(x_index),max(1,1+floor((inputdata_rl(lon0_rl_index,i)-var2d_nc(lon_index,start_dim_nc(x_index),grid_index_rl(y_index,i)))/dgrid_lon+0.5)))
            
            !write(*,*) i,grid_index_rl(x_index,i),grid_index_rl(y_index,i),dgrid_lon,dgrid_lat
            !Reestimate the lat lon grid size for the given position in the grid
            dgrid_lat=(var2d_nc(lat_index,grid_index_rl(x_index,i),dim_length_nc(y_index))-var2d_nc(lat_index,grid_index_rl(x_index,i),start_dim_nc(y_index)))/(dim_length_nc(y_index)-1)
            dgrid_lon=(var2d_nc(lon_index,dim_length_nc(x_index),grid_index_rl(y_index,i))-var2d_nc(lon_index,start_dim_nc(x_index),grid_index_rl(y_index,i)))/(dim_length_nc(x_index)-1)

            !Recalculate position at that point
            grid_index_rl(x_index,i)=min(dim_length_nc(x_index),max(1,1+floor((inputdata_rl(lon0_rl_index,i)-var2d_nc(lon_index,start_dim_nc(x_index),grid_index_rl(y_index,i)))/dgrid_lon+0.5)))
            grid_index_rl(y_index,i)=min(dim_length_nc(y_index),max(1,1+floor((inputdata_rl(lat0_rl_index,i)-var2d_nc(lat_index,grid_index_rl(x_index,i),start_dim_nc(y_index)))/dgrid_lat+0.5)))

            !write(*,*) i,grid_index_rl(x_index,i),grid_index_rl(y_index,i),dgrid_lon,dgrid_lat
            !Reestimate the lat lon grid size for the given position in the grid
            dgrid_lat=(var2d_nc(lat_index,grid_index_rl(x_index,i),dim_length_nc(y_index))-var2d_nc(lat_index,grid_index_rl(x_index,i),start_dim_nc(y_index)))/(dim_length_nc(y_index)-1)
            dgrid_lon=(var2d_nc(lon_index,dim_length_nc(x_index),grid_index_rl(y_index,i))-var2d_nc(lon_index,start_dim_nc(x_index),grid_index_rl(y_index,i)))/(dim_length_nc(x_index)-1)

            !Recalculate position at that point
            x_index_temp=min(dim_length_nc(x_index),max(1,1+floor((inputdata_rl(lon0_rl_index,i)-var2d_nc(lon_index,start_dim_nc(x_index),grid_index_rl(y_index,i)))/dgrid_lon+0.5)))
            y_index_temp=min(dim_length_nc(y_index),max(1,1+floor((inputdata_rl(lat0_rl_index,i)-var2d_nc(lat_index,grid_index_rl(x_index,i),start_dim_nc(y_index)))/dgrid_lat+0.5)))

            !write(*,*) i,grid_index_rl(x_index,i),grid_index_rl(y_index,i),dgrid_lon,dgrid_lat
            !grid_index_rl(x_index:y_index,i)=minloc((var2d_nc(lat_index,:,:)-inputdata_rl(lat0_rl_index,i))**2+(var2d_nc(lon_index,:,:)/cos(var2d_nc(lat_index,:,:)/180.*3.14159)-inputdata_rl(lon0_rl_index,i)/cos(inputdata_rl(lat0_rl_index,i)/180.*3.14159))**2)
            !write(*,*) 'OLD: ',i,grid_index_rl(x_index,i),grid_index_rl(y_index,i)
            
            call lb2lambert2_uEMEP(x_temp,y_temp,inputdata_rl(lon0_rl_index,i),inputdata_rl(lat0_rl_index,i),meteo_nc_projection_attributes)

            grid_index_rl(x_index,i)=1+floor((x_temp-var1d_nc(x_index,1))/dgrid_nc(x_index)+0.5)
            grid_index_rl(y_index,i)=1+floor((y_temp-var1d_nc(y_index,1))/dgrid_nc(y_index)+0.5)
            
            !call LL2UTM(1,utm_zone,var2d_nc(lat_index,grid_index_rl(x_index,i),grid_index_rl(y_index,i)),var2d_nc(lon_index,grid_index_rl(x_index,i),grid_index_rl(y_index,i)),y_utm_temp,x_utm_temp)
            
            !if (abs(grid_index_rl(y_index,i)-y_index_temp).ge.2.or.abs(grid_index_rl(x_index,i)-x_index_temp).ge.2) then
            !write(*,*) i,grid_index_rl(x_index,i)-x_index_temp,grid_index_rl(y_index,i)-y_index_temp,inputdata_rl(lon0_rl_index,i),inputdata_rl(lat0_rl_index,i)
            !write(*,*) x_utm_temp,y_utm_temp
            !write(*,*) var1d_nc(x_index,grid_index_rl(x_index,i)),var1d_nc(y_index,grid_index_rl(y_index,i)),var1d_nc(x_index,grid_index_rl(x_index,i))-599524.,var1d_nc(y_index,grid_index_rl(y_index,i))-7593704.
            !endif
            !if (abs(x_utm_temp-599524.).le.dgrid_nc(x_index)*.5.and.abs(y_utm_temp-7593704.).le.dgrid_nc(y_index)*.5) then
            !write(*,*) i,grid_index_rl(x_index,i)-x_index_temp,grid_index_rl(y_index,i)-y_index_temp,inputdata_rl(lon0_rl_index,i),inputdata_rl(lat0_rl_index,i)
            !write(*,*) 'FOUND: ',x_utm_temp-599524.,y_utm_temp-7593704.
            !stop
            !endif
            
            
            
            if (replace_meteo_with_yr.eq.1.and.some_meteo_nc2_available) then
                !i_dist_min=0
                !j_dist_min=0
                !dist_min=1.e32
                !do ii=1,dim_length_nc2(x_index2)
                !do jj=1,dim_length_nc2(y_index2)
                !    dist=sqrt((var2d_nc2(lat_index2,ii,jj)-inputdata_rl(lat0_rl_index,i))**2+(var2d_nc2(lon_index2,ii,jj)/cos(var2d_nc2(lat_index2,ii,jj)/180.*3.14159)-inputdata_rl(lon0_rl_index,i)/cos(inputdata_rl(lat0_rl_index,i)/180.*3.14159))**2)
                !    if (dist.lt.dist_min) then
                !        dist_min=dist
                !        i_dist_min=ii
                !        j_dist_min=jj
                !    endif
                !enddo
                !enddo
                
                !grid_index_rl2(x_index2,i)=i_dist_min
                !grid_index_rl2(y_index2,i)=j_dist_min
                
                
                !Alternative
                !grid_index_rl2(x_index2:y_index2,i)=minloc((var2d_nc2(lat_index2,:,:)-inputdata_rl(lat0_rl_index,i))**2+(var2d_nc2(lon_index2,:,:)/cos(var2d_nc2(lat_index2,:,:)/180.*3.14159)-inputdata_rl(lon0_rl_index,i)/cos(inputdata_rl(lat0_rl_index,i)/180.*3.14159))**2)
                !write(*,*) k,i,grid_index_rl2(x_index2,i),grid_index_rl2(y_index2,i),110*minval(sqrt((var2d_nc2(lat_index2,:,:)-inputdata_rl(lat0_rl_index,i))**2+(var2d_nc2(lon_index2,:,:)/cos(var2d_nc2(lat_index2,:,:)/180.*3.14159)-inputdata_rl(lon0_rl_index,i)/cos(inputdata_rl(lat0_rl_index,i)/180.*3.14159))**2))          
           
                call lb2lambert2_uEMEP(x_temp,y_temp,inputdata_rl(lon0_rl_index,i),inputdata_rl(lat0_rl_index,i),meteo_nc2_projection_attributes)

                grid_index_rl2(x_index2,i)=1+floor((x_temp-var1d_nc2(x_index2,1))/dgrid_nc2(x_index2)+0.5)
                grid_index_rl2(y_index2,i)=1+floor((y_temp-var1d_nc2(y_index2,1))/dgrid_nc2(y_index2)+0.5)
            

            endif
        else
            write(unit_logfile,'(a,a)') ' ERROR: meteo_data_type not properly defined = ',trim(meteo_data_type)
            stop 24
        endif   
        
        !write(*,*) k
    enddo
    
    if (out_of_range_count.gt.0) then
         write(unit_logfile,'(a,4i12)') ' WARNING: Number of road links outside of grid (NORTRIP_multiroad_save_meteodata) = ',out_of_range_count
    endif
    
    !Match the meteo netcdf times to the input time
    start_time_index_nc=start_dim_nc(time_index)
    end_time_index_nc=end_dim_nc(time_index)
    
    !NOTE: This is not optimal because of the round off errors. Should be relooked at
    do t=start_dim_nc(time_index),end_dim_nc(time_index)
        !Netcdf are in seconds since 1970
        !Round off errors in the time requires getting the value to the nearest hour
        !Errors involved

        date_nc(:,t)=0
        !Calculate the day
        time_temp=dble(int(var1d_nc(time_index,t)/sngl(seconds_in_hour*hours_in_day)+5./24./60.)) !Add 5 minutes for round off errors
        call number_to_date(time_temp,date_nc(:,t))
        !Calculate hour of the day
        date_nc(hour_index,t)=int((var1d_nc(time_index,t)-time_temp*sngl(seconds_in_hour*hours_in_day))/3600.+.5)

        !time_temp=int(var1d_nc(time_index,t)/sngl(seconds_in_hour))/sngl(hours_in_day)
        !call number_to_date(time_temp,date_nc(:,t))
        !write(*,'(i12,f24.2)') t,var1d_nc(time_index,t)
        !write(*,'(i12,f16.8)') int(var1d_nc(time_index,t)/sngl(seconds_in_hour)+.5),var1d_nc(time_index,t)/sngl(seconds_in_hour)
        !write(*,*) t,time_temp
        !write(*,*) date_nc(:,t)
                
    enddo

    !Meteo data in UTC. Adjust the time stamp to local time
    !DIFUTC_H is UTC relative to local, so negative if local time is ahead
    local_date_nc=date_nc
    !Do not convert meteorology to local data
    do t=start_dim_nc(time_index),end_dim_nc(time_index)
        !call incrtm(int(-DIFUTC_H),local_date_nc(1,t),local_date_nc(2,t),local_date_nc(3,t),local_date_nc(4,t))
        !write(*,*) local_date_nc(:,t)
    enddo
 
    
    write(unit_logfile,'(a32,6i6)') ' Start date meteo netcdf = ',date_nc(:,start_dim_nc(time_index))
    write(unit_logfile,'(a32,6i6)') ' End date meteo netcdf = ',date_nc(:,end_dim_nc(time_index))
    write(unit_logfile,'(a32,6i6)') ' Start date meteo local = ',local_date_nc(:,start_dim_nc(time_index))
    write(unit_logfile,'(a32,6i6)') ' End date meteo local = ',local_date_nc(:,end_dim_nc(time_index))
  
    !Find starting and finishing index
    
    start_time_index_nc_found=.false.
    end_time_index_nc_found=.false.
    do t=start_dim_nc(time_index),end_dim_nc(time_index)
        if (local_date_nc(year_index,t).eq.date_data(year_index,1) &
                .and.local_date_nc(month_index,t).eq.date_data(month_index,1) &
                .and.local_date_nc(day_index,t).eq.date_data(day_index,1) &
                .and.local_date_nc(hour_index,t).eq.date_data(hour_index,1)) then
                start_time_index_nc=t
                start_time_index_nc_found=.true.
        endif 
        if (local_date_nc(year_index,t).eq.date_data(year_index,n_hours_input) &
                .and.local_date_nc(month_index,t).eq.date_data(month_index,n_hours_input) &
                .and.local_date_nc(day_index,t).eq.date_data(day_index,n_hours_input) &
                .and.local_date_nc(hour_index,t).eq.date_data(hour_index,n_hours_input)) then
                end_time_index_nc=t
                end_time_index_nc_found=.true.
        endif 
    enddo
    hours_time_index_nc=end_time_index_nc-start_time_index_nc+1
    write(unit_logfile,'(a32,i6,a32,i6,a32,i6)') ' Start_time_index_nc= ',start_time_index_nc,' End_time_index_nc= ',end_time_index_nc,' Hours_nc= ',hours_time_index_nc
   
    if (start_time_index_nc.ne.2) then
        !write(*,'(A,I)') 'Wrong nc start index. Stopping:   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',start_time_index_nc
        !stop
    endif
    
   
    if (.not.start_time_index_nc_found.or..not.end_time_index_nc_found) then
        write(*,'(A)') ' ERROR: Input time start or stop date not found in meteo data. Stopping'
        write(unit_logfile,'(a32,6i6)') ' Start date input = ',start_date_input
        write(unit_logfile,'(a32,6i6)') ' End date input = ',end_date_input
        stop 25
    endif
    
    not_shown_once=.true.
    
    !write(unit_logfile,'(a)') ' Creating meteodata for NORTRIP '//trim(pathfilename_meteo)
    
    if (meteo_obs_data_available) then
        write(unit_logfile,'(a)') 'Replacing model values with observations (model,obs)'
        write(unit_logfile,'(10a20)') 'Temperature','Wind speed','Wind direction','Humidity','Precipitation','Shortwave','Longwave','Pressure','Surface_temperature','T_adjust_lapse'
    endif
    if (replace_meteo_with_yr.eq.1.and.some_meteo_nc2_available) then
        write(unit_logfile,'(a)') 'Replacing model temperatures with analysis temperatures (model,analysis)'
    endif
    if (replace_meteo_with_yr.eq.1.and..not.some_meteo_nc2_available) then
        write(unit_logfile,'(a)') 'No analysis meteo data available at all. Will not replace'
    endif
    
        
!Distribute meteo data to roadlinks. Saves all links or specified links.
    do j=1,n_save_links
        i=save_links(j)
        
        
        if ((inputdata_int_rl(savedata_rl_index,i).eq.1.and.use_only_special_links_flag.ge.1) &
            .or.(use_only_special_links_flag.eq.0).or.(use_only_special_links_flag.eq.2)) then
        
            !write(unit_logfile,'(a)') trim(meteo_obs_name(ii))
        
            do t=1,n_hours_input
            !do t=start_time_index_nc,end_time_index_nc
            j_mod=start_time_index_nc+t-1
            j_obs=start_time_index_meteo_obs+t-1
            time_temp=var1d_nc(time_index,j_mod)    !Not used here as this is the time stamp
            meteo_temp(temperature_index)=var3d_nc(temperature_index,grid_index_rl(x_index,i),grid_index_rl(y_index,i),j_mod)-273.15
            meteo_temp(speed_wind_index)=sqrt(var3d_nc(x_wind_index,grid_index_rl(x_index,i),grid_index_rl(y_index,i),j_mod)**2 &
                + var3d_nc(y_wind_index,grid_index_rl(x_index,i),grid_index_rl(y_index,i),j_mod)**2) 
            meteo_temp(dir_wind_index)=DIRECTION(var3d_nc(x_wind_index,grid_index_rl(x_index,i),grid_index_rl(y_index,i),j_mod) &
                ,var3d_nc(y_wind_index,grid_index_rl(x_index,i),grid_index_rl(y_index,i),j_mod))
            meteo_temp(relhumidity_index)=var3d_nc(relhumidity_index,grid_index_rl(x_index,i),grid_index_rl(y_index,i),j_mod)*100.
            if (.not.var_available_nc(precip_snow_index)) then
                meteo_temp(precip_index)=max(0.,var3d_nc(precip_index,grid_index_rl(x_index,i),grid_index_rl(y_index,i),j_mod))
                if (meteo_temp(temperature_index).gt.0) then
                    meteo_temp(rain_index)=meteo_temp(precip_index)
                    meteo_temp(snow_index)=0
                else
                    meteo_temp(rain_index)=0
                    meteo_temp(snow_index)=meteo_temp(precip_index)
                endif
            else
                meteo_temp(precip_index)=max(0.,var3d_nc(precip_index,grid_index_rl(x_index,i),grid_index_rl(y_index,i),j_mod)) !Not used but calculated
                meteo_temp(rain_index)=max(0.,var3d_nc(precip_index,grid_index_rl(x_index,i),grid_index_rl(y_index,i),j_mod))-max(0.,var3d_nc(precip_snow_index,grid_index_rl(x_index,i),grid_index_rl(y_index,i),j_mod))
                meteo_temp(snow_index)=max(0.,var3d_nc(precip_snow_index,grid_index_rl(x_index,i),grid_index_rl(y_index,i),j_mod))
            endif
            meteo_temp(shortwaveradiation_index)=var3d_nc(shortwaveradiation_index,grid_index_rl(x_index,i),grid_index_rl(y_index,i),j_mod)
            meteo_temp(longwaveradiation_index)=var3d_nc(longwaveradiation_index,grid_index_rl(x_index,i),grid_index_rl(y_index,i),j_mod)
            meteo_temp(cloudfraction_index)=var3d_nc(cloudfraction_index,grid_index_rl(x_index,i),grid_index_rl(y_index,i),j_mod)
            meteo_temp(pressure_index)=var3d_nc(pressure_index,grid_index_rl(x_index,i),grid_index_rl(y_index,i),j_mod)/100.
            meteo_temp(road_temperature_index)=var3d_nc(surface_temperature_index,grid_index_rl(x_index,i),grid_index_rl(y_index,i),j_mod)-273.15
            !meteo_temp(road_temperature_index)=missing_data
            
           
            !Bilineal interpolation, which is the same as an area weighted interpolation
            if (interpolate_meteo_data) then
                
            !Size of grid
            xpos_limit=dgrid_nc(x_index)/2.
            ypos_limit=dgrid_nc(y_index)/2.
            
            !Index of nearest neighbour meteo grid
            i_nc=grid_index_rl(x_index,i)
            j_nc=grid_index_rl(y_index,i)

            !Position of centre of road link
            call lb2lambert2_uEMEP(x_temp,y_temp,inputdata_rl(lon0_rl_index,i),inputdata_rl(lat0_rl_index,i),meteo_nc_projection_attributes)

            xpos_area_max=x_temp+xpos_limit
            xpos_area_min=x_temp-xpos_limit
            ypos_area_max=y_temp+ypos_limit
            ypos_area_min=y_temp-ypos_limit

            sum_weighting_nc=0
            !write(*,'(2i,f12.4,5f12.4)') i,j_mod,sum_weighting_nc,meteo_temp(temperature_index),meteo_temp(speed_wind_index),meteo_temp(dir_wind_index),meteo_temp(shortwaveradiation_index),meteo_temp(rain_index)
            meteo_temp=0.
            
            !Loop over the nearest grids to finding area weighting
            do jj=j_nc-1,j_nc+1
            do ii=i_nc-1,i_nc+1             
            
                xpos_min=max(xpos_area_min,var1d_nc(x_index,ii)-xpos_limit)
                xpos_max=min(xpos_area_max,var1d_nc(x_index,ii)+xpos_limit)
                ypos_min=max(ypos_area_min,var1d_nc(y_index,jj)-ypos_limit)
                ypos_max=min(ypos_area_max,var1d_nc(y_index,jj)+ypos_limit)
               
                !write(*,*) ii,jj
                !write(*,*) 'MIN1 : ',xpos_min,xpos_max,ypos_min,ypos_max
                !write(*,*) 'MIN2:',var1d_nc(x_index,ii)-xpos_limit,var1d_nc(x_index,ii)+xpos_limit,var1d_nc(y_index,jj)-ypos_limit,var1d_nc(y_index,jj)+ypos_limit
                !write(*,*) 'AREA: ',xpos_area_min,xpos_area_max,ypos_area_min,ypos_area_max
                
                !Determine the area intersection of the meteo grid and a meteo grid size centred on the road link
                if (xpos_max.gt.xpos_min.and.ypos_max.gt.ypos_min) then
                    weighting_nc=(ypos_max-ypos_min)*(xpos_max-xpos_min)/dgrid_nc(x_index)/dgrid_nc(y_index)
                else
                    weighting_nc=0.
                endif                
                sum_weighting_nc=sum_weighting_nc+weighting_nc
                
                !write(*,*) ii-i_nc,jj-j_nc,weighting_nc
                
                meteo_temp(temperature_index)=meteo_temp(temperature_index)+(var3d_nc(temperature_index,ii,jj,j_mod)-273.15)*weighting_nc
                meteo_temp(x_wind_index)=meteo_temp(x_wind_index)+var3d_nc(x_wind_index,ii,jj,j_mod)*weighting_nc
                meteo_temp(y_wind_index)=meteo_temp(y_wind_index)+var3d_nc(y_wind_index,ii,jj,j_mod)*weighting_nc
                meteo_temp(relhumidity_index)=meteo_temp(relhumidity_index)+var3d_nc(relhumidity_index,ii,jj,j_mod)*100.*weighting_nc

                if (.not.var_available_nc(precip_snow_index)) then
                    meteo_temp(precip_index)=meteo_temp(precip_index)+max(0.,var3d_nc(precip_index,ii,jj,j_mod))*weighting_nc
                else
                    meteo_temp(precip_index)=meteo_temp(precip_index)+max(0.,var3d_nc(precip_index,ii,jj,j_mod))*weighting_nc !Not used but calculated
                    meteo_temp(rain_index)=meteo_temp(rain_index) &
                        +(max(0.,var3d_nc(precip_index,ii,jj,j_mod)) &
                        -max(0.,var3d_nc(precip_snow_index,ii,jj,j_mod)))*weighting_nc
                    meteo_temp(snow_index)=meteo_temp(snow_index)+max(0.,var3d_nc(precip_snow_index,ii,jj,j_mod))*weighting_nc
                endif

                meteo_temp(shortwaveradiation_index)=meteo_temp(shortwaveradiation_index)+var3d_nc(shortwaveradiation_index,ii,jj,j_mod)*weighting_nc
                meteo_temp(longwaveradiation_index)=meteo_temp(longwaveradiation_index)+var3d_nc(longwaveradiation_index,ii,jj,j_mod)*weighting_nc
                meteo_temp(cloudfraction_index)=meteo_temp(cloudfraction_index)+var3d_nc(cloudfraction_index,ii,jj,j_mod)*weighting_nc
                meteo_temp(pressure_index)=meteo_temp(pressure_index)+var3d_nc(pressure_index,ii,jj,j_mod)/100.*weighting_nc
                meteo_temp(road_temperature_index)=meteo_temp(road_temperature_index)+(var3d_nc(surface_temperature_index,ii,jj,j_mod)-273.15)*weighting_nc
                
            enddo
            enddo

            meteo_temp(speed_wind_index)=sqrt(meteo_temp(x_wind_index)**2+meteo_temp(y_wind_index)**2)
            meteo_temp(dir_wind_index)=DIRECTION(meteo_temp(x_wind_index),meteo_temp(y_wind_index))
            if (.not.var_available_nc(precip_snow_index)) then
                if (meteo_temp(temperature_index).gt.0) then
                    meteo_temp(rain_index)=meteo_temp(precip_index)
                    meteo_temp(snow_index)=0
                else
                    meteo_temp(rain_index)=0
                    meteo_temp(snow_index)=meteo_temp(precip_index)
                endif
            endif
            
            !write(*,'(2i,f12.4,5f12.4)') i,j_mod,sum_weighting_nc,meteo_temp(temperature_index),meteo_temp(speed_wind_index),meteo_temp(dir_wind_index),meteo_temp(shortwaveradiation_index),meteo_temp(rain_index)
            endif !interpolate_meteo_data
            
            !Should I use t or j_mod here? Use t since the correct hour is placed in t
            if (replace_meteo_with_yr.eq.1.and.meteo_nc2_available(t)) then
                if (not_shown_once.and.show_analysis) then 
                    write(unit_logfile,'(a,i,f10.3,f10.3)') 'Temperature:  ',t,meteo_temp(temperature_index),var3d_nc2(temperature_index2,grid_index_rl2(x_index2,i),grid_index_rl2(y_index2,i),t)-273.15
                    write(unit_logfile,'(a,i,f10.3,f10.3)') 'Rel humidity: ',t,meteo_temp(relhumidity_index),var3d_nc2(relhumidity_index2,grid_index_rl2(x_index2,i),grid_index_rl2(y_index2,i),t)*100.
                    write(unit_logfile,'(a,i,f10.3,f10.3)') 'Wind speed:   ',t,meteo_temp(speed_wind_index),sqrt(var3d_nc2(x_wind_index2,grid_index_rl2(x_index2,i),grid_index_rl2(y_index2,i),t)**2 &
                        + var3d_nc2(y_wind_index2,grid_index_rl2(x_index2,i),grid_index_rl2(y_index2,i),t)**2)
                    write(unit_logfile,'(a,i,f10.3,f10.3)') 'Wind direct  :',t,meteo_temp(dir_wind_index),DIRECTION(var3d_nc2(x_wind_index2,grid_index_rl2(x_index2,i),grid_index_rl2(y_index2,i),t) &
                        ,var3d_nc2(y_wind_index2,grid_index_rl2(x_index2,i),grid_index_rl2(y_index2,i),t))
                    write(unit_logfile,'(a,i,f10.3,f10.3)') 'Precipitation:',t,meteo_temp(precip_index),max(0.,var3d_nc2(precip_index2,grid_index_rl2(x_index2,i),grid_index_rl2(y_index2,i),t))
                endif
                meteo_temp(temperature_index)=var3d_nc2(temperature_index2,grid_index_rl2(x_index2,i),grid_index_rl2(y_index2,i),t)-273.15
 
                meteo_temp(speed_wind_index)=sqrt(var3d_nc2(x_wind_index2,grid_index_rl2(x_index2,i),grid_index_rl2(y_index2,i),t)**2 &
                + var3d_nc2(y_wind_index2,grid_index_rl2(x_index2,i),grid_index_rl2(y_index2,i),t)**2) 
                meteo_temp(dir_wind_index)=DIRECTION(var3d_nc2(x_wind_index2,grid_index_rl2(x_index2,i),grid_index_rl2(y_index2,i),t) &
                ,var3d_nc2(y_wind_index2,grid_index_rl2(x_index2,i),grid_index_rl2(y_index2,i),t))
                meteo_temp(relhumidity_index)=var3d_nc2(relhumidity_index2,grid_index_rl2(x_index2,i),grid_index_rl2(y_index2,i),t)*100.
                meteo_temp(precip_index)=max(0.,var3d_nc2(precip_index2,grid_index_rl2(x_index2,i),grid_index_rl2(y_index2,i),t))
                if (meteo_temp(temperature_index).gt.0) then
                    meteo_temp(rain_index)=meteo_temp(precip_index)
                    meteo_temp(snow_index)=0
                else
                    meteo_temp(rain_index)=0
                    meteo_temp(snow_index)=meteo_temp(precip_index)
                endif
                meteo_temp(cloudfraction_index)=var3d_nc2(cloudfraction_index2,grid_index_rl2(x_index2,i),grid_index_rl2(y_index2,i),t)
               
            endif       

            !Replace the data with observed meteo data. The same for all roads except for temperature lapse rate
            if (meteo_obs_data_available.and.replace_meteo_with_obs.eq.1) then
                ii=save_meteo_index(jj)
                if (not_shown_once) then 
                    !Adjusts the common observed temperature to the model height (assumes model height is correct)
                    adjust_lapse=(var3d_nc(elevation_index,grid_index_rl(x_index,i),grid_index_rl(y_index,i),j_mod)-meteo_obs_position(meteo_obs_height_index,ii))*lapse_rate
                    write(unit_logfile,'(18f10.1,f10.3)') meteo_temp(temperature_index),meteo_obs_data(temperature_index,j_obs,ii)+adjust_lapse &
                        ,meteo_temp(speed_wind_index),meteo_obs_data_final(speed_wind_index,j_obs) & 
                        ,meteo_temp(dir_wind_index),meteo_obs_data_final(dir_wind_index,j_obs) & 
                        ,meteo_temp(relhumidity_index),meteo_obs_data_final(relhumidity_index,j_obs) & 
                        ,meteo_temp(precip_index),meteo_obs_data_final(precip_index,j_obs) &
                        ,meteo_temp(shortwaveradiation_index),meteo_obs_data_final(shortwaveradiation_index,j_obs) & 
                        ,meteo_temp(longwaveradiation_index),meteo_obs_data_final(longwaveradiation_index,j_obs) & 
                        ,meteo_temp(pressure_index),meteo_obs_data_final(pressure_index,j_obs) & 
                        ,meteo_temp(road_temperature_index),meteo_obs_data_final(road_temperature_index,j_obs) &
                        ,adjust_lapse
                endif
                
                if (meteo_obs_data_final(temperature_index,j_obs).ne.missing_data.and.replace_which_meteo_with_obs(temperature_index).gt.0) then
                    meteo_temp(temperature_index)=meteo_obs_data_final(temperature_index,j_obs) &
                        +(var3d_nc(elevation_index,grid_index_rl(x_index,i),grid_index_rl(y_index,i),j_mod)-meteo_obs_position(meteo_obs_height_index,1))*lapse_rate
                endif
                if (meteo_obs_data_final(road_temperature_index,j_obs).ne.missing_data.and.replace_which_meteo_with_obs(road_temperature_index).gt.0) then
                    meteo_temp(road_temperature_index)=meteo_obs_data_final(road_temperature_index,j_obs) &
                        +(var3d_nc(elevation_index,grid_index_rl(x_index,i),grid_index_rl(y_index,i),j_mod)-meteo_obs_position(meteo_obs_height_index,1))*lapse_rate
                endif
                if (meteo_obs_data_final(speed_wind_index,j_obs).ne.missing_data.and.replace_which_meteo_with_obs(speed_wind_index).gt.0) meteo_temp(speed_wind_index)=meteo_obs_data_final(speed_wind_index,j_obs)
                if (meteo_obs_data_final(dir_wind_index,j_obs).ne.missing_data.and.replace_which_meteo_with_obs(dir_wind_index).gt.0) meteo_temp(dir_wind_index)=meteo_obs_data_final(dir_wind_index,j_obs)
                if (meteo_obs_data_final(relhumidity_index,j_obs).ne.missing_data.and.replace_which_meteo_with_obs(relhumidity_index).gt.0) meteo_temp(relhumidity_index)=meteo_obs_data_final(relhumidity_index,j_obs)
                if (meteo_obs_data_final(shortwaveradiation_index,j_obs).ne.missing_data.and.replace_which_meteo_with_obs(shortwaveradiation_index).gt.0) meteo_temp(shortwaveradiation_index)=meteo_obs_data_final(shortwaveradiation_index,j_obs)
                if (meteo_obs_data_final(longwaveradiation_index,j_obs).ne.missing_data.and.replace_which_meteo_with_obs(longwaveradiation_index).gt.0) meteo_temp(longwaveradiation_index)=meteo_obs_data_final(longwaveradiation_index,j_obs)
                if (meteo_obs_data_final(cloudfraction_index,j_obs).ne.missing_data.and.replace_which_meteo_with_obs(cloudfraction_index).gt.0) meteo_temp(cloudfraction_index)=meteo_obs_data_final(cloudfraction_index,j_obs)/8.
                if (meteo_obs_data_final(pressure_index,j_obs).ne.missing_data.and.replace_which_meteo_with_obs(pressure_index).gt.0) meteo_temp(pressure_index)=meteo_obs_data_final(pressure_index,j_obs)
                if (meteo_obs_data_final(precip_index,j_obs).ne.missing_data.and.replace_which_meteo_with_obs(precip_index).gt.0) then
                    if (meteo_temp(temperature_index).gt.0) then
                        meteo_temp(rain_index)=meteo_obs_data_final(precip_index,j_obs)
                        meteo_temp(snow_index)=0
                    else
                        meteo_temp(rain_index)=0
                        meteo_temp(snow_index)=meteo_obs_data_final(precip_index,j_obs)
                    endif            
                endif      
                !Possible to remove these four data sources
                if (replace_which_meteo_with_obs(shortwaveradiation_index).lt.0) meteo_temp(shortwaveradiation_index)=missing_data
                if (replace_which_meteo_with_obs(longwaveradiation_index).lt.0) meteo_temp(longwaveradiation_index)=missing_data
                if (replace_which_meteo_with_obs(cloudfraction_index).lt.0) meteo_temp(cloudfraction_index)=missing_data
                if (replace_which_meteo_with_obs(road_temperature_index).lt.0) meteo_temp(road_temperature_index)=missing_data
            endif
            
            !Replacing at individual stations
            if (meteo_obs_data_available.and.replace_meteo_with_obs.eq.2) then
                ii=save_meteo_index(jj)
                !Adjusts the model temperature according to lapse rate so it fits to the observation height. Only does this if replace_meteo_with_obs.eq.2
                if (replace_meteo_with_yr.eq.1) then
                    adjust_lapse=(var2d_nc2(elevation_index2,grid_index_rl2(x_index2,i),grid_index_rl2(y_index2,i))-meteo_obs_position(meteo_obs_height_index,ii))*lapse_rate                
                else
                    adjust_lapse=(var3d_nc(elevation_index,grid_index_rl(x_index,i),grid_index_rl(y_index,i),j_mod)-meteo_obs_position(meteo_obs_height_index,ii))*lapse_rate                
                endif
                meteo_temp(temperature_index)=meteo_temp(temperature_index)+adjust_lapse
                if (not_shown_once) then 
                write(unit_logfile,'(18f10.1,f10.3)') meteo_temp(temperature_index)+adjust_lapse,meteo_obs_data(temperature_index,j_obs,ii) &
                        ,meteo_temp(speed_wind_index),meteo_obs_data(speed_wind_index,j_obs,ii) & 
                        ,meteo_temp(dir_wind_index),meteo_obs_data(dir_wind_index,j_obs,ii) & 
                        ,meteo_temp(relhumidity_index),meteo_obs_data(relhumidity_index,j_obs,ii) & 
                        ,meteo_temp(precip_index),meteo_obs_data(precip_index,j_obs,ii) &
                        ,meteo_temp(shortwaveradiation_index),meteo_obs_data(shortwaveradiation_index,j_obs,ii) & 
                        ,meteo_temp(longwaveradiation_index),meteo_obs_data(longwaveradiation_index,j_obs,ii) & 
                        ,meteo_temp(pressure_index),meteo_obs_data(pressure_index,j_obs,ii) & 
                        ,meteo_temp(road_temperature_index),meteo_obs_data(road_temperature_index,j_obs,ii) &
                        ,adjust_lapse
                endif
                
                if (meteo_obs_data(temperature_index,j_obs,ii).ne.missing_data.and.replace_which_meteo_with_obs(temperature_index).gt.0) meteo_temp(temperature_index)=meteo_obs_data(temperature_index,j_obs,ii)
                if (meteo_obs_data(dir_wind_index,j_obs,ii).ne.missing_data.and.replace_which_meteo_with_obs(dir_wind_index).gt.0) meteo_temp(dir_wind_index)=meteo_obs_data(dir_wind_index,j_obs,ii)
                if (meteo_obs_data(speed_wind_index,j_obs,ii).ne.missing_data.and.replace_which_meteo_with_obs(speed_wind_index).gt.0) meteo_temp(speed_wind_index)=meteo_obs_data(speed_wind_index,j_obs,ii)
                if (meteo_obs_data(relhumidity_index,j_obs,ii).ne.missing_data.and.replace_which_meteo_with_obs(relhumidity_index).gt.0) meteo_temp(relhumidity_index)=meteo_obs_data(relhumidity_index,j_obs,ii)
                if (meteo_obs_data(precip_index,j_obs,ii).ne.missing_data.and.replace_which_meteo_with_obs(precip_index).gt.0) then
                    if (meteo_temp(temperature_index).gt.0) then
                        meteo_temp(rain_index)=meteo_obs_data(precip_index,j_obs,ii)
                        meteo_temp(snow_index)=0
                    else
                        meteo_temp(rain_index)=0
                        meteo_temp(snow_index)=meteo_obs_data(precip_index,j_obs,ii)
                    endif            
                endif
                if (meteo_obs_data(shortwaveradiation_index,j_obs,ii).ne.missing_data.and.replace_which_meteo_with_obs(shortwaveradiation_index).gt.0) meteo_temp(shortwaveradiation_index)=meteo_obs_data(shortwaveradiation_index,j_obs,ii)
                if (meteo_obs_data(longwaveradiation_index,j_obs,ii).ne.missing_data.and.replace_which_meteo_with_obs(longwaveradiation_index).gt.0) meteo_temp(longwaveradiation_index)=meteo_obs_data(longwaveradiation_index,j_obs,ii)
                if (meteo_obs_data(cloudfraction_index,j_obs,ii).ne.missing_data.and.replace_which_meteo_with_obs(cloudfraction_index).gt.0) meteo_temp(cloudfraction_index)=meteo_obs_data(cloudfraction_index,j_obs,ii)
                if (meteo_obs_data(pressure_index,j_obs,ii).ne.missing_data.and.replace_which_meteo_with_obs(pressure_index).gt.0) meteo_temp(pressure_index)=meteo_obs_data(pressure_index,j_obs,ii)
                !if (meteo_obs_data(road_temperature_index,j_obs,ii).ne.missing_data.and.replace_which_meteo_with_obs(road_temperature_index).gt.0) meteo_temp(road_temperature_index)=meteo_obs_data(road_temperature_index,j_obs,ii)
                !When replacing road surface temperature with obs then include the no data values. This is mostly for the forecast initialisation
                if (replace_which_meteo_with_obs(road_temperature_index).gt.0) meteo_temp(road_temperature_index)=meteo_obs_data(road_temperature_index,j_obs,ii)

                !Possible to remove these four data sources
                if (replace_which_meteo_with_obs(shortwaveradiation_index).lt.0) meteo_temp(shortwaveradiation_index)=missing_data
                if (replace_which_meteo_with_obs(longwaveradiation_index).lt.0) meteo_temp(longwaveradiation_index)=missing_data
                if (replace_which_meteo_with_obs(cloudfraction_index).lt.0) meteo_temp(cloudfraction_index)=missing_data
                if (replace_which_meteo_with_obs(road_temperature_index).lt.0) meteo_temp(road_temperature_index)=missing_data
            endif


            if (meteo_obs_data_available) then
                meteo_obs_ID_output(i)=meteo_obs_ID(ii)
            else
                meteo_obs_ID_output(i)=0
            endif
            meteo_output(temperature_index,t,i)=meteo_temp(temperature_index)
            meteo_output(speed_wind_index,t,i)=meteo_temp(speed_wind_index)
            meteo_output(dir_wind_index,t,i)=meteo_temp(dir_wind_index)
            meteo_output(relhumidity_index,t,i)=meteo_temp(relhumidity_index)
            meteo_output(rain_index,t,i)=meteo_temp(rain_index)
            meteo_output(snow_index,t,i)=meteo_temp(snow_index)
            meteo_output(shortwaveradiation_index,t,i)=meteo_temp(shortwaveradiation_index)
            meteo_output(longwaveradiation_index,t,i)=meteo_temp(longwaveradiation_index)
            meteo_output(cloudfraction_index,t,i)=meteo_temp(cloudfraction_index)
            meteo_output(pressure_index,t,i)=meteo_temp(pressure_index)
            meteo_output(road_temperature_index,t,i)=meteo_temp(road_temperature_index)
                            
            enddo
            not_shown_once=.false.

        endif
    enddo
    
    
    deallocate (grid_index_rl)
    deallocate (dist_array_nc)
    deallocate (dist_array_nc2)
    
    if (allocated(var1d_nc)) deallocate (var1d_nc)
    if (allocated(var2d_nc)) deallocate (var2d_nc)
    if (allocated(var3d_nc)) deallocate (var3d_nc)
    if (allocated(var1d_nc2)) deallocate (var1d_nc2)
    if (allocated(var2d_nc2)) deallocate (var2d_nc2)
    if (allocated(var3d_nc2)) deallocate (var3d_nc2)
    
    
    end subroutine NORTRIP_multiroad_create_meteodata
    
    subroutine NORTRIP_multiroad_save_meteodata

    use NORTRIP_multiroad_index_definitions
    
    implicit none
    
    integer i,t,jj
    integer unit_in
    logical exists
    
    
	write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Saving multiroad meteorology file (NORTRIP_multiroad_save_meteodata)'
	write(unit_logfile,'(A)') '================================================================'
    
    !pathname_meteo='C:\BEDRE BYLUFT\NORTRIP implementation\test_output\';
    !filename_meteo='NORTRIP_test'//'_meteorology.txt'
    pathname_meteo=path_inputdata_for_NORTRIP
    filename_meteo=trim(filename_NORTRIP_data)//'_meteorology.txt'

    if (save_timeseriesdata_in_zip_format) then
        filename_zip=trim(filename_NORTRIP_data)//'_meteorology.zip'
        pathname_zip=pathname_meteo
        pathfilename_zip=trim(pathname_zip)//trim(filename_zip)
    endif 
    
    pathfilename_meteo=trim(pathname_meteo)//trim(filename_meteo)

    !Test existence of the pathname. If does not exist then use default
    inquire(directory=trim(pathname_meteo),exist=exists)
    if (.not.exists) then
        write(unit_logfile,'(A,A)') ' ERROR: Path for saving meteo data does not exist: ', trim(pathname_meteo)
        stop 26
    endif

    !Open the file for writing
    unit_in=30
    open(unit_in,file=pathfilename_meteo,access='sequential',status='unknown')  
    
    write(unit_logfile,'(a)') ' Saving meteodata for NORTRIP '//trim(pathfilename_meteo)

    !Write header
    write(unit_in,'(27a)') 'Road_number',achar(9),'Station_number',achar(9),'Time',achar(9),'T2m',achar(9),'FF',achar(9),'DD',achar(9),'RH',achar(9), &
        'Rain',achar(9),'Snow',achar(9),'Global radiation',achar(9),'Longwave radiation',achar(9),'Cloud cover',achar(9),'Pressure',achar(9),'Road surface temperature'
    
        
!Distribute meteo data to roadlinks. Saves all links or specified links.
    do jj=1,n_save_links
        i=save_links(jj)
               
        if ((inputdata_int_rl(savedata_rl_index,i).eq.1.and.use_only_special_links_flag.ge.1) &
            .or.(use_only_special_links_flag.eq.0).or.(use_only_special_links_flag.eq.2)) then
        
            do t=1,n_hours_input

                write(unit_in,'(i6,a,i6,a,i6,a,f6.2,a,f6.2,a,f6.1,a,f6.1,a,f6.2,a,f6.2,a,f6.1,a,f6.1,a,f6.2,a,f6.1,a,f6.2)') &
                ,i &
                ,achar(9),meteo_obs_ID_output(i) &
                ,achar(9),t &
                ,achar(9),meteo_output(temperature_index,t,i) &
                ,achar(9),meteo_output(speed_wind_index,t,i) &
                ,achar(9),meteo_output(dir_wind_index,t,i) &
                ,achar(9),meteo_output(relhumidity_index,t,i) &
                ,achar(9),meteo_output(rain_index,t,i) &
                ,achar(9),meteo_output(snow_index,t,i) &
                ,achar(9),meteo_output(shortwaveradiation_index,t,i) &
                ,achar(9),meteo_output(longwaveradiation_index,t,i) &
                ,achar(9),meteo_output(cloudfraction_index,t,i) &
                ,achar(9),meteo_output(pressure_index,t,i) &
                ,achar(9),meteo_output(road_temperature_index,t,i)
                            
            enddo

        endif
    enddo
    
    
    close(unit_in)

    !C:\Users\brucerd\Downloads\7za a -tzip test.zip NORTRIP_ALLROADS_metadata.txt (-sdel to delete the files)
    !Command line
    !CALL EXECUTE_COMMAND_LINE (command [,wait,exitstat,cmdstat,cmdmsg]). Set wait=T, 
    if (save_timeseriesdata_in_zip_format) then
        !Delete contents if it exists
        inquire(file=trim(pathfilename_zip),exist=exists)
        if (exists) then
            command_line_zip='7za d '//pathfilename_zip
            write(unit_logfile,'(a,a)') 'Command line zip: ',trim(command_line_zip)      
            CALL EXECUTE_COMMAND_LINE (trim(command_line_zip),wait=.true.)
        endif

        write(unit_logfile,'(a,a)') 'Saving to zip format and deleting text file: ',trim(pathfilename_zip)       
        command_line_zip='7za a -tzip '//pathfilename_zip//' '//pathfilename_meteo//' -sdel' !-bd use this to stop progress
        write(unit_logfile,'(a,a)') 'Command line zip: ',trim(command_line_zip)      
        CALL EXECUTE_COMMAND_LINE (trim(command_line_zip),wait=.true.)
    endif
    
    if (allocated(meteo_output)) deallocate (meteo_output)
 
    end subroutine NORTRIP_multiroad_save_meteodata
