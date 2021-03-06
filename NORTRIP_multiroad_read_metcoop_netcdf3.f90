    subroutine NORTRIP_read_metcoop_netcdf3
    !Reads METcoop 66 hour forecast data
    
    use NORTRIP_multiroad_index_definitions
    
    implicit none
    
    include 'netcdf.inc'
      
    !Local variables
    integer status_nc      !Error message
    integer id_nc
    integer dim_id_nc(num_dims_nc)
    integer xtype_nc(num_var_nc)
    integer natts_nc(num_var_nc)
    integer var_id_nc(num_var_nc)
    integer dim_length_metcoop_nc(num_dims_nc+1)
    integer dim_start_metcoop_nc(num_dims_nc+1)
     
    character(256) dimname_temp
    integer i
    integer i_grid_mid,j_grid_mid
    real dlat_nc
    integer exists
    integer ii,jj,tt
    
    double precision, allocatable :: var2d_nc_dp(:,:)
    double precision, allocatable :: var3d_nc_dp(:,:,:)
    double precision, allocatable :: var4d_nc_dp(:,:,:,:)
    real, allocatable :: var4d_nc(:,:,:,:)

    
	write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Reading meteorological data (NORTRIP_read_metcoop_netcdf3)'
	write(unit_logfile,'(A)') '================================================================'

    !pathname_nc='C:\BEDRE BYLUFT\NORTRIP implementation\test\';
    !filename_nc='AROME_1KM_OSLO_20141028_EPI.nc'
    pathfilename_nc=trim(pathname_nc)//trim(filename_nc)
     
    !Test existence of the filename. If does not exist then use default
    inquire(file=trim(pathfilename_nc),exist=exists)
    if (.not.exists) then
        write(unit_logfile,'(A,A)') ' ERROR: Meteo netcdf file does not exist: ', trim(pathfilename_nc)
        write(unit_logfile,'(A)') ' STOPPING'
        !write(*,'(A,A)') ' ERROR: Meteo netcdf file does not exist. Stopping: ', trim(pathfilename_nc)
        stop
    endif

    !Open the netcdf file for reading
    write(unit_logfile,'(2A)') ' Opening netcdf meteo file: ',trim(pathfilename_nc)
    status_nc = NF_OPEN (pathfilename_nc, NF_NOWRITE, id_nc)
    if (status_nc .NE. NF_NOERR) write(unit_logfile,'(A,I)') 'ERROR opening netcdf file: ',status_nc

    !Find out the x,y and time dimmensions of the file by looking at pressure variable
    status_nc = NF_INQ_DIMID (id_nc,dim_name_nc(x_index),dim_id_nc(x_index))
    status_nc = NF_INQ_DIM (id_nc,dim_id_nc(x_index),dimname_temp,dim_length_nc(x_index))
    status_nc = NF_INQ_DIMID (id_nc,dim_name_nc(y_index),dim_id_nc(y_index))
    status_nc = NF_INQ_DIM (id_nc,dim_id_nc(y_index),dimname_temp,dim_length_nc(y_index))
    status_nc = NF_INQ_DIMID (id_nc,dim_name_nc(time_index),dim_id_nc(time_index))
    status_nc = NF_INQ_DIM (id_nc,dim_id_nc(time_index),dimname_temp,dim_length_nc(time_index))
    write(unit_logfile,'(A,3I)') ' Size of dimensions (x,y,t): ',dim_length_nc

    if (number_of_time_steps.ne.0) then
        dim_length_nc(time_index)=number_of_time_steps
        write(unit_logfile,'(A,3I)') ' WARNING: Reducing dimensions of (t) to save space: ',dim_length_nc(time_index)
    endif
    
     
    !Allocate the nc arrays for reading
    allocate (var1d_nc(num_dims_nc,maxval(dim_length_nc))) !x and y and time maximum dimmensions
    allocate (var3d_nc(num_var_nc,dim_length_nc(x_index),dim_length_nc(y_index),dim_length_nc(time_index)))
    allocate (var2d_nc(2,dim_length_nc(x_index),dim_length_nc(y_index))) !Lat and lon
    allocate (var3d_nc_dp(dim_length_nc(x_index),dim_length_nc(y_index),dim_length_nc(time_index)))
    allocate (var2d_nc_dp(dim_length_nc(x_index),dim_length_nc(y_index))) !Lat and lon
    allocate (var4d_nc_dp(dim_length_nc(x_index),dim_length_nc(y_index),1,dim_length_nc(time_index)))
    allocate (var4d_nc(dim_length_nc(x_index),dim_length_nc(y_index),1,dim_length_nc(time_index)))
 
    !Set the number of hours to be read
    
    !Read the x, y and time values
    do i=1,num_dims_nc
        status_nc = NF_INQ_VARID (id_nc, trim(dim_name_nc(i)), var_id_nc(i))
        status_nc = NF_GET_VARA_REAL (id_nc, var_id_nc(i), dim_start_nc(i), dim_length_nc(i), var1d_nc(i,:))
        if (i.eq.time_index) then
            write(unit_logfile,'(3A,2i12)') ' ',trim(dim_name_nc(i)),' (min, max in hours): ' &
                ,minval(int((var1d_nc(i,1:dim_length_nc(i))-var1d_nc(i,dim_start_nc(i)))/3600.+.5)+1) &
                ,maxval(int((var1d_nc(i,1:dim_length_nc(i))-var1d_nc(i,dim_start_nc(i)))/3600.+.5)+1) 
        else
            write(unit_logfile,'(3A,2f12.2)') ' ',trim(dim_name_nc(i)),' (min, max in km): ' &
                ,minval(var1d_nc(i,1:dim_length_nc(i))),maxval(var1d_nc(i,1:dim_length_nc(i))) 
        endif
        
    enddo
        
    !MetCoOp data has 4 dimensions. z is 1 so must adjust this.
    dim_length_metcoop_nc(1:2)=dim_length_nc(1:2)
    dim_length_metcoop_nc(3)=1
    dim_length_metcoop_nc(4)=dim_length_nc(time_index)
    dim_start_metcoop_nc(1:2)=dim_start_nc(1:2)
    dim_start_metcoop_nc(3)=1
    dim_start_metcoop_nc(4)=dim_start_nc(time_index)
   
    !Read through the variables in a loop
    do i=1,num_var_nc
        !write(*,*) i,trim(var_name_nc(i))
        status_nc = NF_INQ_VARID (id_nc, trim(var_name_nc(i)), var_id_nc(i))
        !write(*,*) 'Status1: ',status_nc,id_nc,var_id_nc(i),trim(var_name_nc(i)),NF_NOERR
        !write(*,*) 'Status1: ',dim_start_metcoop_nc
        !write(*,*) 'Status1: ',dim_length_metcoop_nc
        if (status_nc.eq.NF_NOERR) then
        if (i.eq.lat_index.or.i.eq.lon_index) then
            !write(*,*) i,lat_index,lon_index,dim_start_metcoop_nc(1:2), dim_length_metcoop_nc(1:2)
            !status_nc = NF_GET_VARA_REAL (id_nc, var_id_nc(i), dim_start_metcoop_nc(1:2), dim_length_metcoop_nc(1:2), var2d_nc(i,:,:))
            status_nc = NF_GET_VARA_DOUBLE (id_nc, var_id_nc(i), dim_start_nc(1:2), dim_length_nc(1:2), var2d_nc_dp);var2d_nc(i,:,:)=real(var2d_nc_dp)

            write(unit_logfile,'(A,i3,A,2A,2f16.4)') ' ',status_nc,' ',trim(var_name_nc(i)),' (min, max): ' &
                ,minval(var2d_nc(i,:,:)),maxval(var2d_nc(i,:,:)) 
        else
            status_nc = NF_GET_VARA_REAL (id_nc, var_id_nc(i), dim_start_metcoop_nc, dim_length_metcoop_nc, var4d_nc);var3d_nc(i,:,:,:)=var4d_nc(:,:,1,:)
            
            !Make appropriate changes, going backwards so as to overwrite the existing data
            if (i.eq.precip_index.or.i.eq.precip_snow_index) then
                do tt=dim_length_nc(time_index),2,-1
                    !write(*,*) dim_length_nc(y_index),tt
                    var3d_nc(i,:,:,tt)=var3d_nc(i,:,:,tt)-var3d_nc(i,:,:,tt-1)
                enddo
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
    
    status_nc = NF_CLOSE (id_nc)
    
    !Calculate angle difference between North and the Model Y direction based on the middle grids
    !Not correct, needs to be fixed
    i_grid_mid=int(dim_length_nc(x_index)/2)
    j_grid_mid=int(dim_length_nc(y_index)/2)
    dgrid_nc(x_index)=var1d_nc(x_index,i_grid_mid)-var1d_nc(x_index,i_grid_mid-1)
    dgrid_nc(y_index)=var1d_nc(y_index,j_grid_mid)-var1d_nc(y_index,j_grid_mid-1)
    dlat_nc=var2d_nc(lat_index,i_grid_mid,j_grid_mid)-var2d_nc(lat_index,i_grid_mid,j_grid_mid-1)
    
    !If the coordinates are in km instead of metres then change to metres (assuming the difference is not going to be > 100 km
    if (dgrid_nc(x_index).lt.100) dgrid_nc=dgrid_nc*1000.
    
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

    end subroutine NORTRIP_read_metcoop_netcdf3

    
