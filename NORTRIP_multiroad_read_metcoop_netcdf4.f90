    subroutine NORTRIP_read_metcoop_netcdf4
    !Reads MEPS -  METcoop 66 hour forecast data
    
    use NORTRIP_multiroad_index_definitions
    !Update to netcdf 4 and 64 bit in this version 2 of NORTRIP_read_meteo_netcdf

    use netcdf

    implicit none
    
    !include 'netcdf.inc'
      
    !Local variables
    integer status_nc      !Error message
    integer id_nc
    integer dim_id_nc(num_dims_nc)
    integer xtype_nc(num_var_nc)
    integer natts_nc(num_var_nc)
    integer var_id_nc(num_var_nc)
    integer, allocatable :: dim_length_metcoop_nc(:)
    integer, allocatable :: dim_start_metcoop_nc(:)
     
    character(256) dimname_temp
    integer i
    integer i_grid_mid,j_grid_mid
    real dlat_nc
    integer exists
    integer ii,jj,tt
    integer new_start_date_input(num_date_index)
    logical found_file
    character(256) pathname_nc_in,filename_nc_in
    
    integer dim_id_nc_ensemble
    logical ensemble_dim_flag
    integer nDims
    
    double precision, allocatable :: var1d_nc_dp(:)
    double precision, allocatable :: var2d_nc_dp(:,:)
    double precision, allocatable :: var3d_nc_dp(:,:,:)
    double precision, allocatable :: var4d_nc_dp(:,:,:,:)
    real, allocatable :: var4d_nc(:,:,:,:)

    double precision temp_date
    double precision date_to_number
    
    integer var_id_nc_projection
    
	write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Reading meteorological data (NORTRIP_read_metcoop_netcdf4)'
	write(unit_logfile,'(A)') '================================================================'

    !pathname_nc='C:\BEDRE BYLUFT\NORTRIP implementation\test\';
    !filename_nc='AROME_1KM_OSLO_20141028_EPI.nc'
    pathname_nc_in=pathname_nc
    filename_nc_in=filename_nc_template
    call date_to_datestr_bracket(start_date_input,filename_nc_in,filename_nc)
    call date_to_datestr_bracket(start_date_input,pathname_nc_in,pathname_nc)
    
    pathfilename_nc=trim(pathname_nc)//trim(filename_nc)
     
    !Test existence of the filename. If does not exist then use default
    inquire(file=trim(pathfilename_nc),exist=exists)
    if (.not.exists) then
        write(unit_logfile,'(A,A)') ' WARNING: Meteo netcdf file does not exist: ', trim(pathfilename_nc)
        write(unit_logfile,'(A)') ' Will try 24 hours before.'
        !write(*,'(A,A)') ' ERROR: Meteo netcdf file does not exist. Stopping: ', trim(pathfilename_nc)
        
        !Start search back 24 hours
        new_start_date_input=start_date_input
        found_file=.false.
        do i=1,1
            !call incrtm(-24,new_start_date_input(1),new_start_date_input(2),new_start_date_input(3),new_start_date_input(4))
            temp_date=date_to_number(new_start_date_input)
            call number_to_date(temp_date-1.,new_start_date_input)
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
            write(unit_logfile,'(A,A)') ' ERROR: Meteo netcdf file still does not exist: ', trim(pathfilename_nc)
            write(unit_logfile,'(A)') ' STOPPING'
            !write(*,'(A,A)') ' ERROR: Meteo netcdf file does not exist. Stopping: ', trim(pathfilename_nc)
            stop 8
        else
            write(unit_logfile,'(A,A)') ' Found earlier meteo netcdf file: ', trim(pathfilename_nc)
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
    
    if (number_of_time_steps.ne.0) then
        dim_length_nc(time_index)=number_of_time_steps
        write(unit_logfile,'(A,3I)') ' WARNING: Reducing dimensions of (t) to save space: ',dim_length_nc(time_index)
    endif
     
    !Allocate the nc arrays for reading
    allocate (var1d_nc(num_dims_nc,maxval(dim_length_nc))) !x and y and time maximum dimmensions
    allocate (var1d_nc_dp(maxval(dim_length_nc))) !x and y and time maximum dimmensions
    allocate (var3d_nc(num_var_nc,dim_length_nc(x_index),dim_length_nc(y_index),dim_length_nc(time_index)))
    allocate (var2d_nc(2,dim_length_nc(x_index),dim_length_nc(y_index))) !Lat and lon
    allocate (var3d_nc_dp(dim_length_nc(x_index),dim_length_nc(y_index),dim_length_nc(time_index)))
    allocate (var2d_nc_dp(dim_length_nc(x_index),dim_length_nc(y_index))) !Lat and lon
    allocate (var4d_nc_dp(dim_length_nc(x_index),dim_length_nc(y_index),1,dim_length_nc(time_index)))
    allocate (var4d_nc(dim_length_nc(x_index),dim_length_nc(y_index),1,dim_length_nc(time_index)))
 
    !Set the number of hours to be read
    
    !Read the x, y and time values
    do i=1,num_dims_nc
        status_nc = NF90_INQ_VARID (id_nc, trim(dim_name_nc(i)), var_id_nc(i))
        status_nc = NF90_GET_VAR (id_nc, var_id_nc(i), var1d_nc(i,1:dim_length_nc(i)), start=(/dim_start_nc(i)/), count=(/dim_length_nc(i)/))
        if (i.eq.time_index) then
        status_nc = NF90_GET_VAR (id_nc, var_id_nc(i), var1d_nc_dp(1:dim_length_nc(i)), start=(/dim_start_nc(i)/), count=(/dim_length_nc(i)/))
        !write(*,*) status_nc,dim_length_nc(i),trim(dim_name_nc(i)), var1d_nc_dp(1), var1d_nc_dp(dim_length_nc(i))
        var1d_nc(i,:)=real(var1d_nc_dp(:))
            write(unit_logfile,'(3A,2i14)') ' ',trim(dim_name_nc(i)),' (min, max in hours): ' &
                !,minval(int((var1d_nc(i,1:dim_length_nc(i))-var1d_nc(i,dim_start_nc(i)))/3600.+.5)+1) &
                !,maxval(int((var1d_nc(i,1:dim_length_nc(i))-var1d_nc(i,dim_start_nc(i)))/3600.+.5)+1) 
                ,int((var1d_nc(i,1)-var1d_nc(i,1))/3600.+.5)+1 &
                ,int((var1d_nc(i,dim_length_nc(i))-var1d_nc(i,1))/3600.+.5)+1
                !,int(var1d_nc(i,1)) &
                !,int(var1d_nc(i,dim_length_nc(i)))
        else
            write(unit_logfile,'(3A,2f12.2)') ' ',trim(dim_name_nc(i)),' (min, max in km): ' &
                ,minval(var1d_nc(i,1:dim_length_nc(i))),maxval(var1d_nc(i,1:dim_length_nc(i))) 
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
    else
        !MetCoOp data has 4 dimensions. z is 1 so must adjust this.
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
        !write(*,*) i,trim(var_name_nc(i))
        status_nc = NF90_INQ_VARID (id_nc, trim(var_name_nc(i)), var_id_nc(i))
        !write(*,*) 'Status1: ',status_nc,id_nc,var_id_nc(i),trim(var_name_nc(i)),NF_NOERR
        !write(*,*) 'Status1: ',dim_start_metcoop_nc
        !write(*,*) 'Status1: ',dim_length_metcoop_nc
        if (status_nc.eq.NF90_NOERR) then
        if (i.eq.lat_index.or.i.eq.lon_index) then
            !write(*,*) i,lat_index,lon_index,dim_start_metcoop_nc(1:2), dim_length_metcoop_nc(1:2)
            !status_nc = NF_GET_VARA_REAL (id_nc, var_id_nc(i), dim_start_metcoop_nc(1:2), dim_length_metcoop_nc(1:2), var2d_nc(i,:,:))
            !status_nc = NF_GET_VARA_DOUBLE (id_nc, var_id_nc(i), dim_start_nc(1:2), dim_length_nc(1:2), var2d_nc_dp);var2d_nc(i,:,:)=real(var2d_nc_dp)
            status_nc = NF90_GET_VAR (id_nc, var_id_nc(i), var2d_nc_dp,start=(/dim_start_metcoop_nc(1:2)/), count=(/dim_length_metcoop_nc(1:2)/));var2d_nc(i,:,:)=real(var2d_nc_dp)

            write(unit_logfile,'(A,i3,A,2A,2f16.4)') ' ',status_nc,' ',trim(var_name_nc(i)),' (min, max): ' &
                ,minval(var2d_nc(i,:,:)),maxval(var2d_nc(i,:,:)) 
        else
            !status_nc = NF_GET_VARA_REAL (id_nc, var_id_nc(i), dim_start_metcoop_nc, dim_length_metcoop_nc, var4d_nc);var3d_nc(i,:,:,:)=var4d_nc(:,:,1,:)
            status_nc = NF90_GET_VAR (id_nc, var_id_nc(i), var4d_nc,start=(/dim_start_metcoop_nc/), count=(/dim_length_metcoop_nc/));var3d_nc(i,:,:,:)=var4d_nc(:,:,1,:)
            
            !Make appropriate changes, going backwards so as to overwrite the existing data
            if (i.eq.precip_index.or.i.eq.precip_snow_index) then
                do tt=dim_length_nc(time_index),2,-1
                    !write(*,*) dim_length_nc(y_index),tt
                    var3d_nc(i,:,:,tt)=var3d_nc(i,:,:,tt)-var3d_nc(i,:,:,tt-1)
                enddo
                !Don't allow precip below the cutoff value
                where (var3d_nc(i,:,:,:).lt.precip_cutoff) var3d_nc(i,:,:,:)=0.
                
            endif
            if (i.eq.shortwaveradiation_index) then
                do tt=dim_length_nc(time_index),2,-1
                    var3d_nc(i,:,:,tt)=(var3d_nc(i,:,:,tt)-var3d_nc(i,:,:,tt-1))/3600.
                enddo
            endif
            if (i.eq.longwaveradiation_index) then
                do tt=dim_length_nc(time_index),2,-1
                    var3d_nc(i,:,:,tt)=(var3d_nc(i,:,:,tt)-var3d_nc(i,:,:,tt-1))/3600.
                enddo
            endif
            if (i.eq.elevation_index) then
                var3d_nc(i,:,:,:)=var3d_nc(i,:,:,:)/9.8
            endif
            
            write(unit_logfile,'(A,i3,A,2A,2f16.2)') ' ',status_nc,' ',trim(var_name_nc(i)),' (min, max): ' &
                ,minval(var3d_nc(i,:,:,:)),maxval(var3d_nc(i,:,:,:)) 
        endif
        var_available_nc(i)=.true.
        else
             write(unit_logfile,'(8A,8A)') ' Cannot read ',trim(var_name_nc(i))
             var_available_nc(i)=.false.
        endif
        
        
    enddo
    
    !NOTE: round off errors in precipitation. Need to include a 0 minimum.
    
    status_nc = NF90_CLOSE (id_nc)
    
    !Put in some basic data checks to see if file is corrupt
    if (abs(maxval(var3d_nc(temperature_index,:,:,:))).gt.500) then
        write(unit_logfile,'(A,e12.2)') ' ERROR: out of bounds temperature: ', maxval(var3d_nc(temperature_index,:,:,:))
        write(unit_logfile,'(A)') ' STOPPING'
        stop
    endif    
    if (abs(maxval(var3d_nc(x_wind_index,:,:,:))).gt.500) then
        write(unit_logfile,'(A,e12.2)') ' ERROR: out of bounds x wind: ', maxval(var3d_nc(x_wind_index,:,:,:))
        write(unit_logfile,'(A)') ' STOPPING'
        stop
    endif    
    if (abs(maxval(var3d_nc(shortwaveradiation_index,:,:,:))).gt.5000) then
        write(unit_logfile,'(A,e12.2)') ' ERROR: out of bounds short wave radiation: ', maxval(var3d_nc(shortwaveradiation_index,:,:,:))
        write(unit_logfile,'(A)') ' STOPPING'
        stop
    endif    

    !Calculate angle difference between North and the Model Y direction based on the middle grids
    !Not correct, needs to be fixed
    i_grid_mid=int(dim_length_nc(x_index)/2)
    j_grid_mid=int(dim_length_nc(y_index)/2)
    dgrid_nc(x_index)=var1d_nc(x_index,i_grid_mid)-var1d_nc(x_index,i_grid_mid-1)
    dgrid_nc(y_index)=var1d_nc(y_index,j_grid_mid)-var1d_nc(y_index,j_grid_mid-1)
    dlat_nc=var2d_nc(lat_index,i_grid_mid,j_grid_mid)-var2d_nc(lat_index,i_grid_mid,j_grid_mid-1)
    
    !If the coordinates are in km instead of metres then change to metres (assuming the difference is not going to be > 100 km
    if (dgrid_nc(x_index).lt.100) then
        dgrid_nc=dgrid_nc*1000.
        var1d_nc(x_index,:)=var1d_nc(x_index,:)*1000.
        var1d_nc(y_index,:)=var1d_nc(y_index,:)*1000.
    endif
    
    angle_nc=180./3.14159*acos(dlat_nc*3.14159/180.*6.37e6/dgrid_nc(x_index))
    write(unit_logfile,'(A,2f12.1)') ' Grid spacing X and Y (m): ', dgrid_nc(x_index),dgrid_nc(y_index)
    write(unit_logfile,'(A,2i,f12.4)') ' Angle difference between grid and geo North (i,j,deg): ', i_grid_mid,j_grid_mid,angle_nc

    !Set the array dimensions to the available ones. Can be changed later based on input information, particularly for time
    end_dim_nc=dim_length_nc
    start_dim_nc=dim_start_nc
    
    deallocate (var3d_nc_dp)
    deallocate (var2d_nc_dp)
    deallocate (var4d_nc_dp)
    deallocate (var4d_nc)

    end subroutine NORTRIP_read_metcoop_netcdf4

    
