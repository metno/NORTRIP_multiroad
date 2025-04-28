subroutine NORTRIP_read_metcoop_netcdf4
    !Reads MEPS -  METcoop 66 hour forecast data
    
    use NORTRIP_multiroad_index_definitions
    !Update to netcdf 4 and 64 bit in this version 2 of NORTRIP_read_meteo_netcdf

    use netcdf

    implicit none
    
    !include 'netcdf.inc'
      
    !Local variables
    integer status_nc,status_nc1,status_nc2      !Error message
    integer id_nc
    integer dim_id_nc(num_dims_nc)
    integer xtype_nc(num_var_nc)
    integer natts_nc(num_var_nc)
    integer var_id_nc(num_var_nc)
    integer, allocatable :: dim_length_metcoop_nc(:)
    integer, allocatable :: dim_start_metcoop_nc(:)
     
    character(256) dimname_temp
    integer :: i,j,k,t
    integer i_grid_mid,j_grid_mid
    real dlat_nc
    integer exists
    integer ii,jj,tt
    integer new_start_date_input(num_date_index)
    logical found_file
    character(256) pathname_nc_in,filename_nc_in,filename_alternative_nc_in
    
    integer dim_id_nc_ensemble
    logical ensemble_dim_flag
    integer nDims

    double precision, allocatable :: var1d_nc_dp(:)
    double precision, allocatable :: var2d_nc_dp(:,:)
    real, allocatable :: var3d_emep(:,:,:)
    real, allocatable :: var3d_nc_old(:,:,:,:)
    real, allocatable :: var1d_nc_old(:,:)
    real, allocatable :: var4d_nc(:,:,:,:)

    real, allocatable :: var1d_nc_temp(:,:)
    real, allocatable :: var2d_nc_temp(:,:,:)
    real, allocatable :: var3d_nc_temp(:,:,:,:)

    double precision temp_date
    double precision date_to_number

    integer :: a_temp(num_date_index) !Temporary array used when filling date_nc array
    integer meteo_nc_timesteps !The number of timesteps read from the meteo file
    character(10) :: time !for printing date and time
    
    integer var_id_nc_projection
    real :: TOC=273.15
    real :: RH_from_dewpoint_func

    logical invert_dim_flag(num_dims_nc)
    
    double precision  offset_nc, scaling_nc
    double precision seconds_correction

    character(256) units_nc(num_var_nc)

	write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Reading meteorological data (NORTRIP_read_metcoop_netcdf4)'
	write(unit_logfile,'(A)') '================================================================'

    invert_dim_flag=.false.

    !pathname_nc='C:\BEDRE BYLUFT\NORTRIP implementation\test\';
    !filename_nc='AROME_1KM_OSLO_20141028_EPI.nc'
    pathname_nc_in=pathname_nc
    filename_nc_in=filename_nc_template
    filename_alternative_nc_in=filename_alternative_nc_template
    call date_to_datestr_bracket(start_date_input,filename_nc_in,filename_nc)
    call date_to_datestr_bracket(start_date_input,filename_alternative_nc_in,filename_alternative_nc)
    call date_to_datestr_bracket(start_date_input,pathname_nc_in,pathname_nc)
    
    !write(*,*) start_date_input
    !write(*,*) trim(pathname_nc)
    !write(*,*) trim(filename_nc)

    pathfilename_nc=trim(pathname_nc)//trim(filename_nc)
    
    found_file = .True. !To capture the case when the file exist on the first try.

    !Test existence of the filename. If does not exist then use default
    inquire(file=trim(pathfilename_nc),exist=exists)

    if (.not.exists) then
        write(unit_logfile,'(A,A)') ' WARNING: Meteo netcdf file does not exist: ', trim(pathfilename_nc)
        write(unit_logfile,'(A)') ' Will try every hour for the past 25 hours.'
        !write(*,'(A,A)') ' ERROR: Meteo netcdf file does not exist. Stopping: ', trim(pathfilename_nc)
        
        !If looking for older meteo data then allow all times to be read, not limited by number_of_time_steps
        number_of_time_steps=0

        !Start search back 24 hours
        new_start_date_input=start_date_input
        found_file=.false.
        do i=1,25
            !call incrtm(-24,new_start_date_input(1),new_start_date_input(2),new_start_date_input(3),new_start_date_input(4))
            temp_date=date_to_number(new_start_date_input,ref_year)
            call number_to_date(temp_date-1./dble(hours_in_day),new_start_date_input,ref_year)
            !write(*,*) i,new_start_date_input(1:4)
            call date_to_datestr_bracket(new_start_date_input,filename_nc_in,filename_nc)
            call date_to_datestr_bracket(new_start_date_input,pathname_nc_in,pathname_nc)
            pathfilename_nc=trim(pathname_nc)//trim(filename_nc)
            write(unit_logfile,'(A,A)') ' Trying: ', trim(pathfilename_nc)
            inquire(file=trim(pathfilename_nc),exist=exists)
            if (exists) then
                found_file=.true.
                exit
            else 
                found_file=.false.
            endif
        enddo
        
        if (.not.found_file) then
            write(unit_logfile,'(A,A)') ' WARNING: Meteo netcdf file still does not exist: ', trim(pathfilename_nc)
            !write(unit_logfile,'(A)') ' STOPPING'
            !write(*,'(A,A)') ' ERROR: Meteo netcdf file does not exist. Stopping: ', trim(pathfilename_nc)
            !stop 8
        else
            write(unit_logfile,'(A,A)') ' Found earlier meteo netcdf file: ', trim(pathfilename_nc)
        endif
    else 
        if (index(meteo_data_type,'emep').gt.0) then
            !Do nothing as it is OK for emep meteo data
        else
        write(*, *) "ERROR: Meteo file was found on first try. Need to use file from at least one hour back to get correct radiation data. Stopping."
        stop
        endif
    endif
    if (.not.found_file) then
        pathfilename_nc=trim(pathname_nc)//trim(filename_alternative_nc)
        write(unit_logfile,'(A,A)') ' Trying to find alternative meteo netcdf file does not exist: ', trim(pathfilename_nc)
        
        !Test existence of the filename. If does not exist then use default
        inquire(file=trim(pathfilename_nc),exist=exists)
        if (.not.exists) then
            write(unit_logfile,'(A,A)') ' WARNING: Alternative meteo netcdf file does not exist: ', trim(pathfilename_nc)
            write(unit_logfile,'(A)') ' Will try every hour for the past 25 hours.'

            !Start search back 24 hours
            new_start_date_input=start_date_input
            found_file=.false.
            do i=1,25
                !call incrtm(-24,new_start_date_input(1),new_start_date_input(2),new_start_date_input(3),new_start_date_input(4))
                temp_date=date_to_number(new_start_date_input,ref_year)
                call number_to_date(temp_date-1./dble(hours_in_day),new_start_date_input,ref_year)
                !write(*,*) i,new_start_date_input(1:4)
                call date_to_datestr_bracket(new_start_date_input,filename_alternative_nc_in,filename_alternative_nc)
                call date_to_datestr_bracket(new_start_date_input,pathname_nc_in,pathname_nc)
                pathfilename_nc=trim(pathname_nc)//trim(filename_alternative_nc)
                write(unit_logfile,'(A,A)') ' Trying: ', trim(pathfilename_nc)
                inquire(file=trim(pathfilename_nc),exist=exists)
                if (exists) then
                    found_file=.true.
                    exit
                else 
                    found_file=.false.
                endif
            enddo
            
            if (.not.found_file) then
                write(unit_logfile,'(A,A)') ' ERROR: Alternative meteo netcdf file still does not exist: ', trim(pathfilename_nc)
                write(unit_logfile,'(A)') ' STOPPING'
                !write(*,'(A,A)') ' ERROR: Meteo netcdf file does not exist. Stopping: ', trim(pathfilename_nc)
                stop 8
            else
                write(unit_logfile,'(A,A)') ' Found earlier meteo netcdf file: ', trim(pathfilename_nc)
            endif
            
        endif
    endif
    
    !Open the netcdf file for reading
    write(unit_logfile,'(2A)') ' Opening netcdf meteo file: ',trim(pathfilename_nc)
    status_nc = NF90_OPEN (pathfilename_nc, NF90_NOWRITE, id_nc)
    if (status_nc .NE. NF90_NOERR) then
        write(unit_logfile,'(A,I)') 'ERROR opening netcdf file: ',status_nc
        stop 38
    endif
    

    !Find the projection. If no projection then in lat lon coordinates
    status_nc = NF90_INQ_VARID (id_nc,trim(projection_name_nc),var_id_nc_projection)
        
        if (status_nc.eq.NF90_NOERR) then
            !If there is a projection then read in the attributes. All these are doubles
            !status_nc = nf90_inquire_variable(id_nc, var_id_nc_projection, natts = numAtts_projection)
                status_nc = nf90_get_att(id_nc, var_id_nc_projection, 'standard_parallel', meteo_nc_projection_attributes(1:2))
                status_nc = nf90_get_att(id_nc, var_id_nc_projection, 'longitude_of_central_meridian', meteo_nc_projection_attributes(3))
                status_nc = nf90_get_att(id_nc, var_id_nc_projection, 'latitude_of_projection_origin', meteo_nc_projection_attributes(4))
                status_nc = nf90_get_att(id_nc, var_id_nc_projection, 'earth_radius', meteo_nc_projection_attributes(5))
                meteo_nc_projection_type=LCC_projection_index
                        
            write(unit_logfile,'(A,5f12.2)') 'Reading lambert_conformal_conic projection. ',meteo_nc_projection_attributes(1:5)
        else
            meteo_nc_projection_type=LL_projection_index             
        endif


    
    !Find out the x,y and time dimmensions of the file by looking at pressure variable
    status_nc = NF90_INQ_DIMID (id_nc,dim_name_nc(x_index),dim_id_nc(x_index))
    status_nc = NF90_INQUIRE_DIMENSION (id_nc,dim_id_nc(x_index),dimname_temp,dim_length_nc(x_index))
    status_nc = NF90_INQ_DIMID (id_nc,dim_name_nc(y_index),dim_id_nc(y_index))
    status_nc = NF90_INQUIRE_DIMENSION (id_nc,dim_id_nc(y_index),dimname_temp,dim_length_nc(y_index))
    status_nc = NF90_INQ_DIMID (id_nc,dim_name_nc(time_index),dim_id_nc(time_index))
    status_nc = NF90_INQUIRE_DIMENSION (id_nc,dim_id_nc(time_index),dimname_temp,dim_length_nc(time_index))
    write(unit_logfile,'(A,3I)') ' Pos of dimensions (x,y,t): ',dim_id_nc
    write(unit_logfile,'(A,3I)') ' Size of dimensions (x,y,t): ',dim_length_nc
    
    !Reducing x and y dimensions to save space
    call NORTRIP_reduce_meteo_region(id_nc)
 
    if (number_of_time_steps.ne.0) then
        dim_length_nc(time_index)=number_of_time_steps
        write(unit_logfile,'(A,3I)') ' WARNING: Reducing dimensions of (t) to save space: ',dim_length_nc(time_index)
    endif
     
    !Allocate the nc arrays for reading
    allocate (var1d_time_nc_old(dim_length_nc(time_index)) ) !Time allocated separately bc. it needs to be double presicion.
    allocate (var1d_nc_old(num_dims_nc,maxval(dim_length_nc))) !x and y and time maximum dimmensions
    allocate (var1d_nc_dp(maxval(dim_length_nc))) !x and y and time maximum dimmensions
    allocate (var3d_nc_old(num_var_nc,dim_length_nc(x_index),dim_length_nc(y_index),dim_length_nc(time_index)))
    allocate (var2d_nc(2,dim_length_nc(x_index),dim_length_nc(y_index))) !Lat and lon
    allocate (var2d_nc_dp(dim_length_nc(x_index),dim_length_nc(y_index))) !Lat and lon
    if (index(meteo_data_type,'emep').gt.0) then
        allocate (var3d_emep(dim_length_nc(x_index),dim_length_nc(y_index),dim_length_nc(time_index)))
    else
        allocate (var4d_nc(dim_length_nc(x_index),dim_length_nc(y_index),1,dim_length_nc(time_index)))
    endif
    
    !Account for the fact that EMEP is in days since 1900 and MEPS is in seconds since 1970
    if (index(meteo_data_type,'emep').gt.0) then
        seconds_correction=1.
    else
        seconds_correction=dble(seconds_in_hour*hours_in_day)
    endif

    !Set the number of hours to be read
    
    !Read the x, y and time values
    do i=1,num_dims_nc
        status_nc = NF90_INQ_VARID (id_nc, trim(dim_name_nc(i)), var_id_nc(i))
        status_nc = NF90_GET_VAR (id_nc, var_id_nc(i), var1d_nc_old(i,1:dim_length_nc(i)), start=(/dim_start_nc(i)/), count=(/dim_length_nc(i)/))

        if (i.eq.time_index) then
            status_nc = NF90_GET_VAR (id_nc, var_id_nc(i), var1d_nc_dp(1:dim_length_nc(i)), start=(/dim_start_nc(i)/), count=(/dim_length_nc(i)/))
            
           !This write statement is correct for both EMEP and METCOOP time data
            var1d_time_nc_old(:)=var1d_nc_dp(1:dim_length_nc(time_index))
            write(unit_logfile,'(3A,2i14)') ' ',trim(dim_name_nc(i)),' (min, max in hours): ' &
                ,int((var1d_nc_old(i,1)-var1d_nc_old(i,1))/dble(seconds_correction)*dble(hours_in_day)+.5)+1 &
                ,int((var1d_nc_old(i,dim_length_nc(i))-var1d_nc_old(i,1))/dble(seconds_correction)*dble(hours_in_day)+.5)+1

        else
            write(unit_logfile,'(3A,2f12.2)') ' ',trim(dim_name_nc(i)),' (min, max in km): ' &
                ,minval(var1d_nc_old(i,1:dim_length_nc(i))),maxval(var1d_nc_old(i,1:dim_length_nc(i))) 
        endif
        !Check the order of increasing size
        if (var1d_nc_old(i,2).lt.var1d_nc_old(i,1)) then
            invert_dim_flag(i)=.true.
        else
            invert_dim_flag(i)=.false.
        endif

        
    enddo
    
    !Test for an ensemble dimension
    !status_nc = nf90_inquire(id_nc, nDimensions = nDims)
    !write(*,*) status_nc,nDims
    status_nc = NF90_INQ_DIMID (id_nc,'ensemble_member',dim_id_nc_ensemble)
    !write(*,*) status_nc,dim_id_nc_ensemble
    if (status_nc.ge.0) then
        ensemble_dim_flag=.true.
    else
        ensemble_dim_flag=.false.
    endif
    
    
    if (ensemble_dim_flag) then
        allocate (dim_length_metcoop_nc(5))
        allocate (dim_start_metcoop_nc(5))
        dim_length_metcoop_nc(1:2)=dim_length_nc(1:2)
        dim_length_metcoop_nc(3)=1
        dim_length_metcoop_nc(4)=1
        dim_length_metcoop_nc(5)=dim_length_nc(time_index)
        dim_start_metcoop_nc(1:2)=dim_start_nc(1:2)
        dim_start_metcoop_nc(3)=1
        dim_start_metcoop_nc(4)=1
        dim_start_metcoop_nc(5)=dim_start_nc(time_index)
        write(unit_logfile,'(a,i)') 'Ensemble member 0 used with dim index ',dim_id_nc_ensemble
    elseif (index(meteo_data_type,'emep').gt.0) then
        write(unit_logfile,'(a)') 'Reading as EMEP meteo data with 3 dimensions:'
        ! emep has 3 dimensions. z is 1 so must adjust this.
        allocate (dim_length_metcoop_nc(3))
        allocate (dim_start_metcoop_nc(3))
        dim_length_metcoop_nc(1:2)=dim_length_nc(1:2)
        dim_length_metcoop_nc(3)=dim_length_nc(time_index)
        dim_start_metcoop_nc(1:2)=dim_start_nc(1:2)
        dim_start_metcoop_nc(3)=dim_start_nc(time_index)
    else 
        !MetCoOp data has 4 dimensions. z is 1 so must adjust this.
        write(unit_logfile,'(a)') 'Reading as MetCoop/MEPS meteo data with 4 dimensions:'
        allocate (dim_length_metcoop_nc(4))
        allocate (dim_start_metcoop_nc(4))
        dim_length_metcoop_nc(1:2)=dim_length_nc(1:2)
        dim_length_metcoop_nc(3)=1
        dim_length_metcoop_nc(4)=dim_length_nc(time_index)
        dim_start_metcoop_nc(1:2)=dim_start_nc(1:2)
        dim_start_metcoop_nc(3)=1
        dim_start_metcoop_nc(4)=dim_start_nc(time_index)
    endif
    
    
    !Read through the variables in a loop
    do i=1,num_var_nc

        status_nc = NF90_INQ_VARID (id_nc, trim(var_name_nc(i)), var_id_nc(i))

        if (status_nc.eq.NF90_NOERR) then
            if (i.eq.lat_index.or.i.eq.lon_index) then

                status_nc = NF90_GET_VAR (id_nc, var_id_nc(i), var2d_nc_dp,start=(/dim_start_metcoop_nc(1:2)/), count=(/dim_length_metcoop_nc(1:2)/))
                var2d_nc(i,:,:)=real(var2d_nc_dp)

                write(unit_logfile,'(A,i3,A,2A,2f16.4)') ' ',status_nc,' ',trim(var_name_nc(i)),' (min, max): ' &
                    ,minval(var2d_nc(i,:,:)),maxval(var2d_nc(i,:,:)) 
            else
                
                if (index(meteo_data_type,'emep').gt.0) then
                write(*,*) 'Reading EMEP: ',trim(var_name_nc(i))
                write(*,*) 'Dim start: ',dim_start_metcoop_nc
                write(*,*) 'Dim length: ',dim_length_metcoop_nc
                    status_nc = NF90_GET_VAR (id_nc, var_id_nc(i), var3d_emep,start=(/dim_start_metcoop_nc/), count=(/dim_length_metcoop_nc/))
                    write(*,*) 'Read'
                    var3d_nc_old(i,:,:,:)=var3d_emep(:,:,:)
                    !Read offsets and scaling
                    offset_nc=0.
                    scaling_nc=1.
                    status_nc1 =nf90_get_att(id_nc, var_id_nc(i), 'add_offset', offset_nc)
                    status_nc2 =nf90_get_att(id_nc, var_id_nc(i), 'scale_factor', scaling_nc)
                    !Only add offset and scale factor if available
                    if (status_nc1.eq.0.and.status_nc2.eq.0) then
                        var3d_nc_old(i,:,:,:)=var3d_emep(:,:,:)*scaling_nc+offset_nc
                    endif
                
                else
                    status_nc = NF90_GET_VAR (id_nc, var_id_nc(i), var4d_nc,start=(/dim_start_metcoop_nc/), count=(/dim_length_metcoop_nc/))
                    var3d_nc_old(i,:,:,:)=var4d_nc(:,:,1,:)
                endif
            
            !Read units
                status_nc =nf90_get_att(id_nc, var_id_nc(i), 'units', units_nc(i))

                !Make appropriate changes, going backwards so as to overwrite the existing data
                !EMEP data is not accumulated
                if (index(meteo_data_type,'emep').eq.0) then
                    if (i.eq.precip_index.or.i.eq.precip_snow_index) then
                        do tt=dim_length_nc(time_index),2,-1
                            var3d_nc_old(i,:,:,tt)=var3d_nc_old(i,:,:,tt)-var3d_nc_old(i,:,:,tt-1)
                        enddo
                        !Assumes units of kg/m^2. Scale if otherwise (EC data)
                        if (index(units_nc(i),'Mg/m^2').gt.0.or.index(units_nc(i),'Mg/m2').gt.0) then
                            var3d_nc_old(i,:,:,:)=var3d_nc_old(i,:,:,:)*1000.
                            write(*,*) 'Precipitation units are Mg/m^2. Converting to kg/m^2'
                        endif

                        !Don't allow precip below the cutoff value
                        where (var3d_nc_old(i,:,:,:).lt.precip_cutoff) var3d_nc_old(i,:,:,:)=0.                
                    endif

                    if (i.eq.shortwaveradiation_index) then
                        do tt=dim_length_nc(time_index),2,-1
                            var3d_nc_old(i,:,:,tt)=(var3d_nc_old(i,:,:,tt)-var3d_nc_old(i,:,:,tt-1))/dble(seconds_in_hour)
                        enddo
                    endif

                    if (i.eq.longwaveradiation_index) then
                        do tt=dim_length_nc(time_index),2,-1
                            var3d_nc_old(i,:,:,tt)=(var3d_nc_old(i,:,:,tt)-var3d_nc_old(i,:,:,tt-1))/dble(seconds_in_hour)
                        enddo
                    endif
                endif

                if (i.eq.elevation_index) then
                    var3d_nc_old(i,:,:,:)=var3d_nc_old(i,:,:,:)/9.8
                endif
                
                write(unit_logfile,'(A,i3,A,2A,2f16.2)') ' ',status_nc,' ',trim(var_name_nc(i)),' (min, max): ' &
                    ,minval(var3d_nc_old(i,:,:,:)),maxval(var3d_nc_old(i,:,:,:)) 
            endif
            var_available_nc(i)=.true.
        else
            write(unit_logfile,'(8A,8A)') ' Cannot read ',trim(var_name_nc(i))
            var_available_nc(i)=.false.
        endif
        
        
    enddo
    
    !invert_dim_flag=.false.
    
    !Invert dimmension if required    
    if (invert_dim_flag(x_index).or.invert_dim_flag(y_index)) then
        allocate (var1d_nc_temp(num_dims_nc,maxval(dim_length_nc)))
        allocate (var2d_nc_temp(2,dim_length_nc(x_index),dim_length_nc(y_index)))
        allocate (var3d_nc_temp(num_var_nc,dim_length_nc(x_index),dim_length_nc(y_index),dim_length_nc(time_index)))
   
        var1d_nc_temp=var1d_nc_old
        var2d_nc_temp=var2d_nc
        var3d_nc_temp=var3d_nc_old
        
        if (invert_dim_flag(x_index)) then
            write(unit_logfile,'(A)') ' Inverting X dimension'

            do i=1,dim_length_nc(x_index)
                var1d_nc_old(x_index,i)=var1d_nc_temp(x_index,dim_length_nc(x_index)+1-i)
                var2d_nc(:,i,:)=var2d_nc_temp(:,dim_length_nc(x_index)+1-i,:)
                var3d_nc_old(:,i,:,:)=var3d_nc_temp(:,dim_length_nc(x_index)+1-i,:,:)
            enddo
        endif
        if (invert_dim_flag(y_index)) then
            write(unit_logfile,'(A)') ' Inverting Y dimension'
            do j=1,dim_length_nc(y_index)
                var1d_nc_old(y_index,j)=var1d_nc_temp(y_index,dim_length_nc(y_index)+1-j)
                var2d_nc(:,:,j)=var2d_nc_temp(:,:,dim_length_nc(y_index)+1-j)
                var3d_nc_old(:,:,j,:)=var3d_nc_temp(:,:,dim_length_nc(y_index)+1-j,:)
            enddo
        endif

        deallocate (var1d_nc_temp)
        deallocate (var2d_nc_temp)
        deallocate (var3d_nc_temp)

    endif
    
    !NOTE: round off errors in precipitation. Need to include a 0 minimum.
    
    status_nc = NF90_CLOSE (id_nc)

    !Put in some basic data checks to see if file is corrupt
    if (abs(maxval(var3d_nc_old(temperature_index,:,:,:))).gt.500) then
        write(unit_logfile,'(A,e12.2)') ' ERROR: out of bounds temperature: ', maxval(var3d_nc_old(temperature_index,:,:,:))
        write(unit_logfile,'(A)') ' STOPPING'
        stop
    endif    
    if (abs(maxval(var3d_nc_old(x_wind_index,:,:,:))).gt.500) then
        write(unit_logfile,'(A,e12.2)') ' ERROR: out of bounds x wind: ', maxval(var3d_nc_old(x_wind_index,:,:,:))
        write(unit_logfile,'(A)') ' STOPPING'
        stop
    endif    
    if (abs(maxval(var3d_nc_old(shortwaveradiation_index,:,:,:))).gt.5000) then
        write(unit_logfile,'(A,e12.2)') ' ERROR: out of bounds short wave radiation: ', maxval(var3d_nc_old(shortwaveradiation_index,:,:,:))
        write(unit_logfile,'(A)') ' STOPPING'
        stop
    endif    

    !Convert dew point to RH if RH not available and dewpoint is
    if (var_available_nc(dewpoint_index).and..not.var_available_nc(relhumidity_index)) then
        do k=1,size(var3d_nc_old,4)
            do j=1,size(var3d_nc_old,3)
                do i=1,size(var3d_nc_old,2)
                    var3d_nc_old(relhumidity_index,i,j,k)=RH_from_dewpoint_func(var3d_nc_old(temperature_index,i,j,k)-TOC,var3d_nc_old(dewpoint_index,i,j,k)-TOC)/100.
                    var3d_nc_old(relhumidity_index,i,j,k)=max(var3d_nc_old(relhumidity_index,i,j,k),0.)
                    var3d_nc_old(relhumidity_index,i,j,k)=min(var3d_nc_old(relhumidity_index,i,j,k),1.)
                enddo
            enddo
        enddo
    endif
    
    !In the case of lat lon coordinates in dimensions then populate the lat lon 2d field as this is used further
    if (meteo_nc_projection_type.ne.LL_projection_index) then
        do j=1,size(var2d_nc,3)
            do i=1,size(var2d_nc,2)
                var2d_nc(lon_index,i,j)=var1d_nc_old(x_index,i)
                var2d_nc(lat_index,i,j)=var1d_nc_old(y_index,j)
            enddo
        enddo
    endif
    
    !In the case of lat lon coordinates in dimensions then populate the lat lon 2d field as this is used further
    if (meteo_nc_projection_type.ne.LL_projection_index) then
        do j=1,size(var2d_nc,3)
            do i=1,size(var2d_nc,2)
                var2d_nc(lon_index,i,j)=var1d_nc_old(x_index,i)
                var2d_nc(lat_index,i,j)=var1d_nc_old(y_index,j)
            enddo
        enddo
    endif
    
    !Calculate angle difference between North and the Model Y direction based on the middle grids
    !Not correct, needs to be fixed !TODO: Is this fixed?
    i_grid_mid=int(dim_length_nc(x_index)/2)
    j_grid_mid=int(dim_length_nc(y_index)/2)
    dgrid_nc(x_index)=var1d_nc_old(x_index,i_grid_mid)-var1d_nc_old(x_index,i_grid_mid-1)
    dgrid_nc(y_index)=var1d_nc_old(y_index,j_grid_mid)-var1d_nc_old(y_index,j_grid_mid-1)
    dlat_nc=var2d_nc(lat_index,i_grid_mid,j_grid_mid)-var2d_nc(lat_index,i_grid_mid,j_grid_mid-1)
    
    !If the coordinates are in km instead of metres then change to metres (assuming the difference is not going to be > 100 km
    if (dgrid_nc(x_index).lt.100.and.meteo_nc_projection_type.ne.LL_projection_index) then
        dgrid_nc=dgrid_nc*1000.
        var1d_nc_old(x_index,:)=var1d_nc_old(x_index,:)*1000.
        var1d_nc_old(y_index,:)=var1d_nc_old(y_index,:)*1000.
    endif

    !This doesn't seem to make sense. Check this again
    angle_nc=180./3.14159*acos(dlat_nc*3.14159/180.*6.37e6/dgrid_nc(y_index))
    write(unit_logfile,'(A,2f12.3)') ' Grid spacing X and Y (m): ', dgrid_nc(x_index),dgrid_nc(y_index)
    write(unit_logfile,'(A,2i,f12.4)') ' Angle difference between grid and geo North (i,j,deg): ', i_grid_mid,j_grid_mid,angle_nc



    meteo_nc_timesteps = nint(1 + (dim_length_nc(time_index)-1)/timestep) !Number of time steps that will be saved from the meteo file. (If timestep = 1h this will just be the number of hours)

    !Fill a date_nc array that is used to match meteo dates to the date range specified in the simulation call. 
    allocate(date_nc(num_date_index,meteo_nc_timesteps))

    call number_to_date(dble(int(var1d_nc_old(time_index,1)/seconds_correction+1./dble(hours_in_day*minutes_in_hour))),date_nc(:,1),ref_year)

    date_nc(hour_index,1)=int((var1d_nc_old(time_index,1)-(dble(int(var1d_nc_old(time_index,1)/seconds_correction+1./dble(hours_in_day*minutes_in_hour))))*seconds_correction)/dble(seconds_in_hour)+.5)
    do t=1, meteo_nc_timesteps-1
        a_temp=date_nc(:,1)
        call minute_increment(int(dble(minutes_in_hour)*timestep)*t,a_temp(1),a_temp(2),a_temp(3),a_temp(4),a_temp(5)) 
        date_nc(:,t+1)=a_temp   
    enddo

    !Check if timestep is != 1; if true, allocate new, larger arrays and interpolate the hourly values into the new arrays.
    if ( timestep .ne. 1) then 

        !Allocate an array with the new time_index.           
        if (allocated(var3d_nc)) deallocate(var3d_nc)
        allocate (var3d_nc(num_var_nc,dim_length_nc(x_index),dim_length_nc(y_index),nint(1 + (dim_length_nc(time_index)-1)/timestep)))
        allocate (var1d_nc(num_dims_nc,maxval(dim_length_nc)))
        allocate (var1d_time_nc(nint(1 + (dim_length_nc(time_index)-1)/timestep)))

        call date_and_time(TIME=time)
        print*, "loop start: ", time
        do i = int(1/timestep), nint(dim_length_nc(time_index)/timestep)

            var3d_nc(:,:,:,i-int(1/timestep)+1) = var3d_nc_old(:,:,:,floor(i*timestep)) + ( var3d_nc_old(:,:,:,min(floor(i*timestep)+1,size(var3d_nc_old,dim=4))) - var3d_nc_old(:,:,:,floor(i*timestep)) ) * (i*timestep-floor(i*timestep)) !/1 

            var3d_nc(precip_index,:,:,i-int(1/timestep)+1) = max(0.,var3d_nc_old(precip_index,:,:,min(floor(i*timestep)+1,size(var3d_nc_old,dim=4)))/6)
            var3d_nc(precip_snow_index,:,:,i-int(1/timestep)+1) = max(0.,var3d_nc_old(precip_snow_index,:,:,min(floor(i*timestep)+1,size(var3d_nc_old,dim=4)))/6)

            var1d_time_nc(i-int(1/timestep)+1) = var1d_time_nc_old(floor(i*timestep)) + ( var1d_time_nc_old(min(floor(i*timestep)+1,size(var1d_time_nc_old))) - var1d_time_nc_old(floor(i*timestep)) ) * (i*timestep-floor(i*timestep)) !/1 

        end do
        call date_and_time(TIME = time)
        print*, "end loop: ", time

        
        var1d_nc(x_index,:) = var1d_nc_old(x_index,:)
        var1d_nc(y_index,:) = var1d_nc_old(y_index,:)

    else
        var1d_nc = var1d_nc_old
        var1d_time_nc = var1d_time_nc_old
        var3d_nc = var3d_nc_old
    end if

    !Set the array dimensions to the available ones. Can be changed later based on input information, particularly for time
    start_dim_nc=dim_start_nc
    end_dim_nc=dim_length_nc
    end_dim_nc(time_index) = size(var3d_nc,dim=4)
    if (allocated(var3d_nc_old)) deallocate(var3d_nc_old)
    if (allocated(var1d_nc_old)) deallocate(var1d_nc_old)
    if (allocated(var1d_nc_dp)) deallocate(var1d_nc_dp)
    if (allocated(var2d_nc_dp)) deallocate(var2d_nc_dp)
    if (allocated(var4d_nc)) deallocate(var4d_nc)
    if (allocated(var3d_emep)) deallocate(var3d_emep)



end subroutine NORTRIP_read_metcoop_netcdf4