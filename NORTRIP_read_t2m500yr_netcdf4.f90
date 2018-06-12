    
    subroutine NORTRIP_read_t2m500yr_netcdf4
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
    
    double precision, allocatable :: var2d_nc2_dp(:,:)
    double precision, allocatable :: var3d_nc2_dp(:,:)

    
	write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Reading meteorological data (NORTRIP_read_t2m500yr_netcdf v2)'
	write(unit_logfile,'(A)') '================================================================'

    !pathname_nc='C:\BEDRE BYLUFT\NORTRIP implementation\test\';
    !filename_nc='AROME_1KM_OSLO_20141028_EPI.nc'
    pathfilename_nc2=trim(pathname_nc2)//trim(filename_nc2)
     
    !Test existence of the filename. If does not exist then use default
    inquire(file=trim(pathfilename_nc2),exist=exists)
    if (.not.exists) then
        write(unit_logfile,'(A,A)') ' ERROR: Meteo netcdf file does not exist: ', trim(pathfilename_nc2)
        write(unit_logfile,'(A)') ' STOPPING'
        !write(*,'(A,A)') ' ERROR: Meteo netcdf file does not exist. Stopping: ', trim(pathfilename_nc)
        stop
    endif

    !Open the netcdf file for reading
    write(unit_logfile,'(2A)') ' Opening netcdf meteo file: ',trim(pathfilename_nc2)
    status_nc2 = NF90_OPEN (pathfilename_nc2, NF90_NOWRITE, id_nc2)
    if (status_nc2 .NE. NF90_NOERR) write(unit_logfile,'(A,I)') 'ERROR opening netcdf file: ',status_nc2

    !Find out the x,y and time dimmensions of the file
    status_nc2 = NF90_INQ_DIMID (id_nc2,dim_name_nc2(x_index2),dim_id_nc2(x_index2))
    status_nc2 = NF90_INQUIRE_DIMENSION (id_nc2,dim_id_nc2(x_index2),dimname_temp,dim_length_nc2(x_index2))
    status_nc2 = NF90_INQ_DIMID (id_nc2,dim_name_nc2(y_index2),dim_id_nc2(y_index2))
    status_nc2 = NF90_INQUIRE_DIMENSION (id_nc2,dim_id_nc2(y_index2),dimname_temp,dim_length_nc2(y_index2))
    status_nc2 = NF90_INQ_DIMID (id_nc2,dim_name_nc2(time_index2),dim_id_nc2(time_index2))
    status_nc2 = NF90_INQUIRE_DIMENSION (id_nc2,dim_id_nc2(time_index2),dimname_temp,dim_length_nc2(time_index2))
    write(unit_logfile,'(A,3I)') ' Size of dimensions (x,y,t): ',dim_length_nc2
    
    if (number_of_time_steps.ne.0) then
        dim_length_nc2(time_index)=number_of_time_steps
        write(unit_logfile,'(A,3I)') ' WARNING: Reducing dimensions of (t) to save space: ',dim_length_nc2(time_index)
    endif
   
    !Allocate the nc arrays for reading
    !write(*,*) dim_length_nc2(x_index2),dim_length_nc2(y_index2),dim_length_nc2(time_index2)
    allocate (var1d_nc2(num_dims_nc2,maxval(dim_length_nc2))) !x and y and time maximum dimmensions
    allocate (var3d_nc2(temperature_index2,dim_length_nc2(x_index2),dim_length_nc2(y_index2),dim_length_nc2(time_index2)))
    allocate (var2d_nc2(elevation_index2,dim_length_nc2(x_index2),dim_length_nc2(y_index2))) !Lat and lon and elevation
    allocate (var3d_nc2_dp(dim_length_nc2(x_index2),dim_length_nc2(y_index2)))
    allocate (var2d_nc2_dp(dim_length_nc2(x_index2),dim_length_nc2(y_index2))) !Lat and lon
 
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
        
    !MetCoOp data has 3 dimensions. z is 1 so must adjust this.
    !dim_length_metcoop_nc(1:2)=dim_length_nc(1:2)
    !dim_length_metcoop_nc(3)=1
    !dim_length_metcoop_nc(4)=dim_length_nc(time_index)
    !dim_start_metcoop_nc(1:2)=dim_start_nc(1:2)
    !dim_start_metcoop_nc(3)=1
    !dim_start_metcoop_nc(4)=dim_start_nc(time_index)
   
    !Read through the variables in a loop
    do i=1,num_var_nc2
        !write(*,*) i,trim(var_name_nc(i))
        status_nc2 = NF90_INQ_VARID (id_nc2, trim(var_name_nc2(i)), var_id_nc2(i))
        !write(*,*) 'Status1: ',status_nc2,id_nc2,var_id_nc2(i),trim(var_name_nc2(i)),NF_NOERR
        !write(*,*) 'Status1: ',dim_start_metcoop_nc
        !write(*,*) 'Status1: ',dim_length_metcoop_nc
        if (status_nc2.eq.NF90_NOERR) then
        if (i.eq.lat_index2.or.i.eq.lon_index2) then
            !write(*,*) id_nc2, var_id_nc2(i), dim_start_nc2(1:2), dim_length_nc2(1:2)
            !status_nc = NF_GET_VARA_REAL (id_nc, var_id_nc(i), dim_start_metcoop_nc(1:2), dim_length_metcoop_nc(1:2), var2d_nc(i,:,:))
            !status_nc2 = NF_GET_VARA_DOUBLE (id_nc2, var_id_nc2(i), dim_start_nc2(1:2), dim_length_nc2(1:2), var2d_nc2_dp);var2d_nc2(i,:,:)=real(var2d_nc2_dp)
            status_nc2 = NF90_GET_VAR (id_nc2, var_id_nc2(i), var2d_nc2_dp(:,:), start=(/dim_start_nc2/), count=(/dim_length_nc2/));var2d_nc2(i,:,:)=real(var2d_nc2_dp)
            write(unit_logfile,'(A,i3,2A,2f16.4)') ' ',status_nc2,trim(var_name_nc2(i)),' (min, max): ' &
                ,minval(var2d_nc2(i,:,:)),maxval(var2d_nc2(i,:,:)) 
        
        elseif (i.eq.elevation_index2) then
            
            !status_nc2 = NF_GET_VARA_DOUBLE (id_nc2, var_id_nc2(i), dim_start_nc2(1:2), dim_length_nc2(1:2), var2d_nc2_dp);var2d_nc2(i,:,:)=real(var2d_nc2_dp)
            status_nc2 = NF90_GET_VAR (id_nc2, var_id_nc2(i), var2d_nc2_dp(:,:), start=(/dim_start_nc2/), count=(/dim_length_nc2/));var2d_nc2(i,:,:)=real(var2d_nc2_dp)
            write(unit_logfile,'(A,i3,2A,2f16.4)') ' ',status_nc2,trim(var_name_nc2(i)),' (min, max): ' &
                ,minval(var2d_nc2(i,:,:)),maxval(var2d_nc2(i,:,:)) 
            !write(*,*) maxval(var2d_nc2(i,:,:))
            
        else
            !write(*,*) id_nc2, var_id_nc2(i)
            !write(*,*) dim_start_nc2
            !write(*,*) dim_length_nc2
            !Due to memory problems must loop through time on this variable
            do t=1,dim_length_nc2(time_index2)
                dim_start_nc2(time_index2)=t
                dim_length_nc2(time_index2)=1
                !status_nc2 = NF_GET_VARA_DOUBLE (id_nc2, var_id_nc2(i), dim_start_nc2, dim_length_nc2, var3d_nc2_dp);var3d_nc2(i,:,:,t)=real(var3d_nc2_dp)
                status_nc2 = NF90_GET_VAR (id_nc2, var_id_nc2(i), var3d_nc2_dp(:,:), start=(/dim_start_nc2/), count=(/dim_length_nc2/));var3d_nc2(i,:,:,t)=real(var3d_nc2_dp)
            enddo
                dim_start_nc2(time_index2)=1
                dim_length_nc2(time_index2)=dim_length_nc2(time_index2)
            write(unit_logfile,'(A,i3,2A,2f16.2)') ' ',status_nc2,trim(var_name_nc2(i)),' (min, max): ' &
                ,minval(var3d_nc2(i,:,:,:)),maxval(var3d_nc2(i,:,:,:)) 
        endif
        else
             write(unit_logfile,'(8A,8A)') ' Cannot read ',trim(var_name_nc2(i))
        endif
        
        
    enddo
        
    status_nc2 = NF90_CLOSE (id_nc2)
    

    !Set the array dimensions to the available ones. Can be changed later based on input information, particularly for time
    end_dim_nc2=dim_length_nc2
    start_dim_nc2=dim_start_nc2
    
    if (allocated(var3d_nc2_dp)) deallocate (var3d_nc2_dp)
    if (allocated(var2d_nc2_dp)) deallocate (var2d_nc2_dp)

    end subroutine NORTRIP_read_t2m500yr_netcdf4