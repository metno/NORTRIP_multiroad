    
    subroutine NORTRIP_read_analysismeteo_netcdf4
    !Reads yr temperature 66 hour forecast data
    
    use NORTRIP_multiroad_index_definitions
    !Update to netcdf 4 and 64 bit in this version 2 of NORTRIP_read_t2m500yr_netcdf
    use netcdf
    
    implicit none
    
    !include 'netcdf.inc'
      
    !Local variables
    integer status_nc2     !Error message
    integer id_nc2
    integer dim_id_nc2(num_dims_nc2)
    integer xtype_nc2(num_var_nc2)
    integer natts_nc2(num_var_nc2)
    integer var_id_nc2(num_var_nc2)
    integer dim_length_metcoop_nc2(num_dims_nc2+1)
    integer dim_start_metcoop_nc2(num_dims_nc2+1)
     
    character(256) dimname_temp
    integer i
    integer i_grid_mid,j_grid_mid
    real dlat_nc2
    integer exists
    integer ii,jj,tt,t
    
    integer var_id_nc2_projection
    character(256) pathname_nc2_in,filename_nc2_in
    integer new_start_date_input(num_date_index)
    double precision temp_date
    double precision date_to_number

    double precision, allocatable :: var2d_nc2_dp(:,:)
    double precision, allocatable :: var3d_nc2_dp(:,:)

    logical dim_read_flag
    
	write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Reading additional meteorological data (NORTRIP_read_analysismeteo_netcdf4)'
	write(unit_logfile,'(A)') '================================================================'

    pathfilename_nc2=trim(pathname_nc2)//trim(filename_nc2)
    pathname_nc2_in=pathname_nc2
    filename_nc2_in=filename_nc2_template
    new_start_date_input=start_date_input
         
    if (.not.allocated(meteo_nc2_available)) allocate (meteo_nc2_available(n_hours_input/timesteps_in_hour)) 
    if (.not.allocated(meteo_var_nc2_available)) allocate (meteo_var_nc2_available(n_hours_input,num_var_nc2)) 
    meteo_var_nc2_available=.true.
    
    dim_read_flag=.false.
    
    !Loop through the number of time steps and read in data when available
    do t=1,int(n_hours_input/timesteps_in_hour)
        temp_date=date_to_number(start_date_input)
        call number_to_date(temp_date+(t-1)/dble(24.),new_start_date_input)
        write(unit_logfile,'(a,7i)') 'Date array: ',t,new_start_date_input(1:6)
        call date_to_datestr_bracket(new_start_date_input,filename_nc2_in,filename_nc2)
        call date_to_datestr_bracket(new_start_date_input,pathname_nc2_in,pathname_nc2)
        pathfilename_nc2=trim(pathname_nc2)//trim(filename_nc2)
        
        !Test existence of the filename. If does not exist then skip the time index
        inquire(file=trim(pathfilename_nc2),exist=exists)
        if (.not.exists) then
            write(unit_logfile,'(A,A)') ' WARNING: Meteo netcdf2 file does not exist: ', trim(pathfilename_nc2)
            meteo_nc2_available(t)=.false.
        else
            meteo_nc2_available(t)=.true.
        endif
        !Open the netcdf file for reading
        if (meteo_nc2_available(t)) then
        
            write(unit_logfile,'(2A)') ' Opening netcdf meteo file: ',trim(pathfilename_nc2)
            status_nc2 = NF90_OPEN (pathfilename_nc2, NF90_NOWRITE, id_nc2)
            if (status_nc2 .NE. NF90_NOERR) write(unit_logfile,'(A,I)') 'ERROR opening netcdf file: ',status_nc2

            !Find the projection. If no projection then in lat lon coordinates
            status_nc2 = NF90_INQ_VARID (id_nc2,trim(projection_name_nc2),var_id_nc2_projection)
                
            if (status_nc2.eq.NF90_NOERR) then
                !If there is a projection then read in the attributes. All these are doubles
                !status_nc = nf90_inquire_variable(id_nc, var_id_nc_projection, natts = numAtts_projection)
                    status_nc2 = nf90_get_att(id_nc2, var_id_nc2_projection, 'standard_parallel', meteo_nc2_projection_attributes(1:2))
                    status_nc2 = nf90_get_att(id_nc2, var_id_nc2_projection, 'longitude_of_central_meridian', meteo_nc2_projection_attributes(3))
                    status_nc2 = nf90_get_att(id_nc2, var_id_nc2_projection, 'latitude_of_projection_origin', meteo_nc2_projection_attributes(4))
                    status_nc2 = nf90_get_att(id_nc2, var_id_nc2_projection, 'earth_radius', meteo_nc2_projection_attributes(5))
                    meteo_nc2_projection_type=LCC_projection_index
                            
                write(unit_logfile,'(A,5f12.2)') 'Reading lambert_conformal_conic projection. ',meteo_nc2_projection_attributes(1:5)
            else
                meteo_nc2_projection_type=LL_projection_index             
            endif

            if (.not.dim_read_flag) then
                !Find out the x,y and time dimmensions of the file
                status_nc2 = NF90_INQ_DIMID (id_nc2,dim_name_nc2(x_index2),dim_id_nc2(x_index2))
                status_nc2 = NF90_INQUIRE_DIMENSION (id_nc2,dim_id_nc2(x_index2),dimname_temp,dim_length_nc2(x_index2))
                status_nc2 = NF90_INQ_DIMID (id_nc2,dim_name_nc2(y_index2),dim_id_nc2(y_index2))
                status_nc2 = NF90_INQUIRE_DIMENSION (id_nc2,dim_id_nc2(y_index2),dimname_temp,dim_length_nc2(y_index2))
        
                call NORTRIP_reduce_meteo_region2(id_nc2)
                dim_read_flag=.true.
            end if 
            status_nc2 = NF90_INQ_DIMID (id_nc2,dim_name_nc2(time_index2),dim_id_nc2(time_index2))
            status_nc2 = NF90_INQUIRE_DIMENSION (id_nc2,dim_id_nc2(time_index2),dimname_temp,dim_length_nc2(time_index2))
            write(unit_logfile,'(A,3I)') ' Size of dimensions (x,y,t): ',dim_length_nc2
            
            if (number_of_time_steps.ne.0) then
                dim_length_nc2(time_index)=number_of_time_steps
                write(unit_logfile,'(A,3I)') ' WARNING: Reducing dimensions of (t) to save space: ',dim_length_nc2(time_index)
            endif
        
            !Allocate the nc arrays for reading
            !write(*,*) dim_length_nc2(x_index2),dim_length_nc2(y_index2),dim_length_nc2(time_index2)
            if (.not.allocated(var1d_nc2)) allocate (var1d_nc2(num_dims_nc2,maxval(dim_length_nc2))) !x and y and time maximum dimmensions
            if (.not.allocated(var3d_nc2)) allocate (var3d_nc2(num_var_nc2,dim_length_nc2(x_index2),dim_length_nc2(y_index2),n_hours_input))
            if (.not.allocated(var2d_nc2)) allocate (var2d_nc2(num_var_nc2,dim_length_nc2(x_index2),dim_length_nc2(y_index2))) !Lat and lon and elevation
            if (.not.allocated(var3d_nc2_dp)) allocate (var3d_nc2_dp(dim_length_nc2(x_index2),dim_length_nc2(y_index2)))
            if (.not.allocated(var2d_nc2_dp)) allocate (var2d_nc2_dp(dim_length_nc2(x_index2),dim_length_nc2(y_index2))) !Lat and lon

            !Set the number of hours to be read
            
            !Read the x, y and time values
            do i=1,num_dims_nc2
                status_nc2 = NF90_INQ_VARID (id_nc2, trim(dim_name_nc2(i)), var_id_nc2(i))
                !status_nc2 = NF_GET_VARA_REAL (id_nc2, var_id_nc2(i), dim_start_nc2(i), dim_length_nc2(i), var1d_nc2(i,:))
                status_nc2 = NF90_GET_VAR (id_nc2, var_id_nc2(i), var1d_nc2(i,1:dim_length_nc2(i)), start=(/dim_start_nc2(i)/), count=(/dim_length_nc2(i)/))
            if (i.eq.time_index2) then
                    write(unit_logfile,'(3A,2i12)') ' ',trim(dim_name_nc2(i)),' (min, max in hours): ' &
                        ,minval(int((var1d_nc2(i,1:dim_length_nc2(i))-var1d_nc2(i,dim_start_nc2(i)))/3600.+.5)+1) &
                        ,maxval(int((var1d_nc2(i,1:dim_length_nc2(i))-var1d_nc2(i,dim_start_nc2(i)))/3600.+.5)+1) 
                else
                    write(unit_logfile,'(3A,2f12.2)') ' ',trim(dim_name_nc2(i)),' (min, max in km): ' &
                        ,minval(var1d_nc2(i,1:dim_length_nc2(i))),maxval(var1d_nc2(i,1:dim_length_nc2(i))) 
                endif
                
            enddo
            

            !Read through the variables in a loop
            do i=1,num_var_nc2

                status_nc2 = NF90_INQ_VARID (id_nc2, trim(var_name_nc2(i)), var_id_nc2(i))

                if (status_nc2.eq.NF90_NOERR) then
                    if (i.eq.lat_index2.or.i.eq.lon_index2) then

                        status_nc2 = NF90_GET_VAR (id_nc2, var_id_nc2(i), var2d_nc2_dp(:,:), start=(/dim_start_nc2/), count=(/dim_length_nc2/));
                        var2d_nc2(i,:,:)=real(var2d_nc2_dp)
                        write(unit_logfile,'(A,i3,2A,2f16.4)') ' ',status_nc2,trim(var_name_nc2(i)),' (min, max): ' &
                            ,minval(var2d_nc2(i,:,:)),maxval(var2d_nc2(i,:,:)) 
                    
                    elseif (i.eq.elevation_index2) then
                        
                        status_nc2 = NF90_GET_VAR (id_nc2, var_id_nc2(i), var2d_nc2_dp(:,:), start=(/dim_start_nc2/), count=(/dim_length_nc2/));
                        var2d_nc2(i,:,:)=real(var2d_nc2_dp)
                        write(unit_logfile,'(A,i3,2A,2f16.4)') ' ',status_nc2,trim(var_name_nc2(i)),' (min, max): ' &
                            ,minval(var2d_nc2(i,:,:)),maxval(var2d_nc2(i,:,:)) 
                        
                    else
                        !Due to memory problems must loop through time on this variable
                        !do t=1,dim_length_nc2(time_index2)
                            dim_start_nc2(time_index2)=1
                            dim_length_nc2(time_index2)=1
                            status_nc2 = NF90_GET_VAR (id_nc2, var_id_nc2(i), var3d_nc2_dp(:,:), start=(/dim_start_nc2/), count=(/dim_length_nc2/));
                            var3d_nc2(i,:,:,t)=real(var3d_nc2_dp)
                        !enddo
                            dim_start_nc2(time_index2)=1
                            dim_length_nc2(time_index2)=dim_length_nc2(time_index2)
                            write(unit_logfile,'(A,i3,2A,2f16.2)') ' ',status_nc2,trim(var_name_nc2(i)),' (min, max): ' &
                            ,minval(var3d_nc2(i,:,:,t)),maxval(var3d_nc2(i,:,:,t)) 

                    endif
                else
                    write(unit_logfile,'(8A,8A)') ' Cannot read ',trim(var_name_nc2(i))
                    meteo_var_nc2_available(t,i)=.false.
                endif
                
                if (i.eq.precip_index2) then
                    !Don't allow precip below the cutoff value
                    where (var3d_nc2(i,:,:,t).lt.precip_cutoff) var3d_nc2(i,:,:,t)=0.
                endif
                
            enddo
            
            status_nc2 = NF90_CLOSE (id_nc2)
            
            !Put in some basic data checks to see if file is corrupt
            if (abs(maxval(var3d_nc2(temperature_index2,:,:,:))).gt.500) then
                !write(unit_logfile,'(A,e12.2)') ' ERROR: out of bounds temperature: ', maxval(var3d_nc2(temperature_index,:,:,:))
                !write(unit_logfile,'(A)') ' STOPPING'
                write(unit_logfile,'(A,e12.2)') ' WARNING: out of bounds temperature. Will not use these data but will continue calculations: ', maxval(var3d_nc2(temperature_index2,:,:,:))
                meteo_var_nc2_available(temperature_index2,t)=.false.
                !stop
            endif    
            if (abs(maxval(var3d_nc2(precip_index2,:,:,:))).gt.1000) then
                !write(unit_logfile,'(A,e12.2)') ' ERROR: out of bounds temperature: ', maxval(var3d_nc2(temperature_index,:,:,:))
                !write(unit_logfile,'(A)') ' STOPPING'
                write(unit_logfile,'(A,e12.2)') ' WARNING: out of bounds precipitation. Will not use these data but will continue calculations: ', maxval(var3d_nc2(precip_index2,:,:,:))
                meteo_var_nc2_available(precip_index2,t)=.false.
                !stop
            endif    
            if (abs(maxval(var3d_nc2(relhumidity_index2,:,:,:))).gt.1.10) then
                write(unit_logfile,'(A,e12.2)') ' WARNING: out of bounds humidity. Will not use these data but will continue calculations: ', maxval(var3d_nc2(relhumidity_index2,:,:,:))
                meteo_var_nc2_available(relhumidity_index2,t)=.false.
                !stop
            endif    

            i_grid_mid=int(dim_length_nc2(x_index)/2)
            j_grid_mid=int(dim_length_nc2(y_index)/2)
            dgrid_nc2(x_index2)=var1d_nc2(x_index2,i_grid_mid)-var1d_nc2(x_index2,i_grid_mid-1)
            dgrid_nc2(y_index2)=var1d_nc2(y_index2,j_grid_mid)-var1d_nc2(y_index2,j_grid_mid-1)
                
            !If the coordinates are in km instead of metres then change to metres (assuming the difference is not going to be > 100 km
            if (dgrid_nc2(x_index2).lt.100) then
                dgrid_nc2=dgrid_nc2*1000.
                var1d_nc2(x_index2,:)=var1d_nc2(x_index2,:)*1000.
                var1d_nc2(y_index2,:)=var1d_nc2(y_index2,:)*1000.
            endif
            write(unit_logfile,'(A,2f12.1)') ' Grid spacing X and Y (m): ', dgrid_nc2(x_index2),dgrid_nc2(y_index2)

            !Set the array dimensions to the available ones. Can be changed later based on input information, particularly for time
            end_dim_nc2=dim_length_nc2
            start_dim_nc2=dim_start_nc2
    
        endif !End if exists
    enddo !end t loop
    
    if (allocated(var3d_nc2_dp)) deallocate (var3d_nc2_dp)
    if (allocated(var2d_nc2_dp)) deallocate (var2d_nc2_dp)

    end subroutine NORTRIP_read_analysismeteo_netcdf4