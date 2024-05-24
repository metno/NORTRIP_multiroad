    subroutine NORTRIP_read_nora3_netcdf4
    !Reads NORA3 data -  provided in individual hourly files with specific 6 hour intervals, from 4 to 9, read from 3 to 9
    !Needs to read in 4 files directories and 25 files for a single calendar day from 01-00
    !Do not need tocheck for ensemble and do not need to chaeck for alternative meteo as these do not exist
    !Based on the metcoop reading routine
    
    use NORTRIP_multiroad_index_definitions
   
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
    integer i,j,k
    integer i_grid_mid,j_grid_mid
    real dlat_nc
    integer exists
    integer ii,jj,tt,kk
    integer new_start_date_input(num_date_index)
    logical found_file
    character(256) pathname_nc_in,filename_nc_in,filename_alternative_nc_in
    
    integer dim_id_nc_ensemble
    logical ensemble_dim_flag
    integer nDims
    
    double precision, allocatable :: var1d_nc_dp(:)
    double precision, allocatable :: var2d_nc_dp(:,:)
    !double precision, allocatable :: var3d_nc_dp(:,:,:)
    !double precision, allocatable :: var4d_nc_dp(:,:,:,:)
    real, allocatable :: var4d_nc(:,:,:,:)
    real, allocatable :: var3d_nc_in(:,:,:,:)
    real, allocatable :: var1d_nc_in(:,:)
    double precision, allocatable :: var1d_time_nc_in(:)

    double precision temp_date
    double precision date_to_number
    
    integer var_id_nc_projection
    real :: TOC=273.15
    real :: RH_from_dewpoint_func
    
    double precision nora3_forecast_hour_dp
    integer, allocatable ::  nora3_forecast_hour(:)
    integer, allocatable :: nora3_date_data(:,:)
    logical, allocatable :: new_nora3_forecast(:)
    
    integer nora3_start_index
    integer nora3_index
    double precision :: nora3_starting_hour=4.
    integer new_nora3_forecast_count
    
    integer a(num_date_index)
    
    allocate (nora3_date_data(num_date_index,n_hours_input))
    allocate (new_nora3_forecast(n_hours_input+1))
    allocate (nora3_forecast_hour(n_hours_input+1))
    
	write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Reading meteorological data (NORTRIP_read_nora3_netcdf4)'
	write(unit_logfile,'(A)') '================================================================'

    !pathname_nc='C:\BEDRE BYLUFT\NORTRIP implementation\test\';
    !filename_nc='AROME_1KM_OSLO_20141028_EPI.nc'
    pathname_nc_in=pathname_nc
    filename_nc_in=filename_nc_template
    
    !Defne the NORA3 files corresponding to the input time
    !fc<yyyymmddHH>_[num]_fp.nc num is 2 digits 003 to 009 and HH is the forecast hour
    new_nora3_forecast_count=0
    do i=1,n_hours_input
        !Hours. small offset required
        nora3_forecast_hour_dp=24.*date_to_number(date_data(:,i))-nora3_starting_hour+.0001
        !Set the date of the directory and file
        call number_to_date((nora3_forecast_hour_dp-(dmod(nora3_forecast_hour_dp,6.)))/24.,nora3_date_data(:,i))
        !Specify the forecast hour as the remainder
        !nora3_forecast_hour(i)=floor(nora3_forecast_hour_dp-dmod(nora3_forecast_hour_dp,6.)*6.+0.5)
        nora3_forecast_hour(i)=idint(dmod(nora3_forecast_hour_dp,6.))+nora3_starting_hour
        !Specify if it is a new forecast or not
        if (i.eq.1) then
            new_nora3_forecast(i)=.true.           
        elseif (sum(nora3_date_data(:,i)).eq.sum(nora3_date_data(:,i-1))) then
            new_nora3_forecast(i)=.false.
        else
            new_nora3_forecast(i)=.true.
        endif
        if (new_nora3_forecast(i)) then
            new_nora3_forecast_count=new_nora3_forecast_count+1
        endif
        
       ! write(*,*) i,nora3_forecast_hour(i),new_nora3_forecast(i)
       ! write(*,*) nora3_date_data(1:4,i)
        !write(*,*) date_data(1:4,i)
        !write(*,*) (nora3_forecast_hour_dp-(dmod(nora3_forecast_hour_dp,6.)))/24.,nora3_forecast_hour_dp
    enddo
    
    !i=0
    do jj=1,n_hours_input
        !i=i+1
        if (new_nora3_forecast(jj)) then
        !read all files in the directory
        do kk=3,9
            call date_to_datestr_bracket(nora3_date_data(:,jj),filename_nc_in,filename_nc)
            !write(*,*) filename_nc_in,filename_nc
            call date_to_datestr_bracket(nora3_date_data(:,jj),pathname_nc_in,pathname_nc)
            call num_to_numstr_squarebracket(kk,filename_nc,filename_nc)
            pathfilename_nc=trim(pathname_nc)//trim(filename_nc)

            inquire(file=trim(pathfilename_nc),exist=exists)
            if (.not.exists) then
                write(unit_logfile,'(A,A)') ' WARNING: Meteo netcdf file does not exist: ', trim(pathfilename_nc)
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
            if (.not.allocated(var1d_nc)) then
                allocate (var1d_nc(num_dims_nc,maxval(dim_length_nc))) !x and y and time maximum dimmensions
                var1d_nc=0.
            endif
            
            if (.not.allocated(var1d_time_nc)) allocate (var1d_time_nc(n_hours_input) )!x and y and time maximum dimmensions
            if (.not.allocated(var1d_time_nc_in)) allocate (var1d_time_nc_in(9) )!x and y and time maximum dimmensions
            if (.not.allocated(var1d_nc_in)) allocate (var1d_nc_in(num_dims_nc,maxval(dim_length_nc))) !x and y and time maximum dimmensions
            if (.not.allocated(var1d_nc_dp)) allocate (var1d_nc_dp(maxval(dim_length_nc))) !x and y and time maximum dimmensions
            if (.not.allocated(var3d_nc)) allocate (var3d_nc(num_var_nc,dim_length_nc(x_index),dim_length_nc(y_index),n_hours_input))
            if (.not.allocated(var3d_nc_in)) allocate (var3d_nc_in(num_var_nc,dim_length_nc(x_index),dim_length_nc(y_index),9))
            if (.not.allocated(var2d_nc)) allocate (var2d_nc(2,dim_length_nc(x_index),dim_length_nc(y_index))) !Lat and lon
            !allocate (var3d_nc_dp(dim_length_nc(x_index),dim_length_nc(y_index),dim_length_nc(time_index)))
            if (.not.allocated(var2d_nc_dp)) allocate (var2d_nc_dp(dim_length_nc(x_index),dim_length_nc(y_index))) !Lat and lon
            !allocate (var4d_nc_dp(dim_length_nc(x_index),dim_length_nc(y_index),1,dim_length_nc(time_index)))
            if (.not.allocated(var4d_nc)) allocate (var4d_nc(dim_length_nc(x_index),dim_length_nc(y_index),1,dim_length_nc(time_index)))
    
            
            
            !Set the number of hours to be read
    
            !Read the x, y and time values
            do i=1,num_dims_nc
                status_nc = NF90_INQ_VARID (id_nc, trim(dim_name_nc(i)), var_id_nc(i))
                status_nc = NF90_GET_VAR (id_nc, var_id_nc(i), var1d_nc_in(i,1:dim_length_nc(i)), start=(/dim_start_nc(i)/), count=(/dim_length_nc(i)/))
                if (i.eq.time_index) then
                status_nc = NF90_GET_VAR (id_nc, var_id_nc(i), var1d_nc_dp(1:dim_length_nc(i)), start=(/dim_start_nc(i)/), count=(/dim_length_nc(i)/))
                !write(*,*) status_nc,dim_length_nc(i),trim(dim_name_nc(i)), var1d_nc_dp(1), var1d_nc_dp(dim_length_nc(i))
                var1d_time_nc_in(kk)=var1d_nc_dp(1)
                !write(*,*) var1d_nc_in(i,kk)
                    write(unit_logfile,'(3A,2i14)') ' ',trim(dim_name_nc(i)),' (min, max in hours): ' &
                        !,minval(int((var1d_nc(i,1:dim_length_nc(i))-var1d_nc(i,dim_start_nc(i)))/3600.+.5)+1) &
                        !,maxval(int((var1d_nc(i,1:dim_length_nc(i))-var1d_nc(i,dim_start_nc(i)))/3600.+.5)+1) 
                        ,int((var1d_time_nc_in(kk)-var1d_time_nc_in(1))/3600.+.5)+1 &
                        ,int((var1d_time_nc_in(dim_length_nc(i))-var1d_time_nc_in(kk))/3600.+.5)+1
                        !,int(var1d_nc(i,1)) &
                        !,int(var1d_nc(i,dim_length_nc(i)))
                else
                    var1d_nc(i,:)=var1d_nc_in(i,:)
                    write(unit_logfile,'(3A,2f12.2)') ' ',trim(dim_name_nc(i)),' (min, max in km): ' &
                        ,minval(var1d_nc(i,1:dim_length_nc(i))),maxval(var1d_nc(i,1:dim_length_nc(i))) 
                endif
        
            enddo
    
    
 
            !MetCoOp data has 4 dimensions. z is 1 so must adjust this.
            write(unit_logfile,'(a)') 'Reading as NORA3 meteo data with 4 dimensions:'
            if (.not.allocated(dim_length_metcoop_nc)) allocate (dim_length_metcoop_nc(4))
            if (.not.allocated(dim_start_metcoop_nc)) allocate (dim_start_metcoop_nc(4))
            dim_length_metcoop_nc(1:2)=dim_length_nc(1:2)
            dim_length_metcoop_nc(3)=1
            dim_length_metcoop_nc(4)=dim_length_nc(time_index)
            dim_start_metcoop_nc(1:2)=dim_start_nc(1:2)
            dim_start_metcoop_nc(3)=1
            dim_start_metcoop_nc(4)=dim_start_nc(time_index)

    
   
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
                    status_nc = NF90_GET_VAR (id_nc, var_id_nc(i), var4d_nc,start=(/dim_start_metcoop_nc/), count=(/dim_length_metcoop_nc/));var3d_nc_in(i,:,:,kk)=var4d_nc(:,:,1,1)
           
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

            !Convert dew point to RH if RH not available and dewpoint is
            if (var_available_nc(dewpoint_index).and..not.var_available_nc(relhumidity_index)) then
                do k=1,size(var3d_nc,4)
                do j=1,size(var3d_nc,3)
                do i=1,size(var3d_nc,2)
                    var3d_nc(relhumidity_index,i,j,k)=RH_from_dewpoint_func(var3d_nc(temperature_index,i,j,k)-TOC,var3d_nc(dewpoint_index,i,j,k)-TOC)/100.
                    var3d_nc(relhumidity_index,i,j,k)=max(var3d_nc(relhumidity_index,i,j,k),0.)
                    var3d_nc(relhumidity_index,i,j,k)=min(var3d_nc(relhumidity_index,i,j,k),1.)
                enddo
                enddo
                enddo
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

    
        enddo !kk
    
            !Read through the variables in a loop
            dim_length_nc(time_index)=9
            do i=1,num_var_nc
           
            !Make appropriate changes, going backwards so as to overwrite the existing data
            if (i.eq.precip_index.or.i.eq.precip_snow_index) then
                do tt=dim_length_nc(time_index),2,-1
                    !write(*,*) dim_length_nc(y_index),tt
                    var3d_nc_in(i,:,:,tt)=var3d_nc_in(i,:,:,tt)-var3d_nc_in(i,:,:,tt-1)
                enddo
                !Don't allow precip below the cutoff value
                where (var3d_nc_in(i,:,:,:).lt.precip_cutoff) var3d_nc_in(i,:,:,:)=0.
                
            endif
            if (i.eq.shortwaveradiation_index) then
                do tt=dim_length_nc(time_index),2,-1
                    var3d_nc_in(i,:,:,tt)=(var3d_nc_in(i,:,:,tt)-var3d_nc_in(i,:,:,tt-1))/3600.
                enddo
            endif
            if (i.eq.longwaveradiation_index) then
                do tt=dim_length_nc(time_index),2,-1
                    var3d_nc_in(i,:,:,tt)=(var3d_nc_in(i,:,:,tt)-var3d_nc_in(i,:,:,tt-1))/3600.
                enddo
            endif
            if (i.eq.elevation_index) then
                var3d_nc_in(i,:,:,:)=var3d_nc_in(i,:,:,:)/9.8
            endif
            
            if (i.eq.lat_index.or.i.eq.lon_index) then
            write(unit_logfile,'(A,i3,A,2A,2f16.2)') ' ',status_nc,' ',trim(var_name_nc(i)),' (min, max): ' &
                ,minval(var2d_nc(i,:,:)),maxval(var2d_nc(i,:,:))         
            else
            write(unit_logfile,'(A,i3,A,2A,2f16.2)') ' ',status_nc,' ',trim(var_name_nc(i)),' (min, max): ' &
                ,minval(var3d_nc_in(i,:,:,:)),maxval(var3d_nc_in(i,:,:,:))         
            endif
    
            
            enddo

            !Place the files in the correct array
    
            ii=jj
            !write(*,*) ii,jj
            var3d_nc(:,:,:,ii)=var3d_nc_in(:,:,:,nora3_forecast_hour(ii))
            var1d_nc(:,ii)=var1d_nc_in(:,nora3_forecast_hour(ii))
            var1d_time_nc(ii)=var1d_time_nc_in(nora3_forecast_hour(ii))
            write(unit_logfile,'(2i,es16.8)') ii,nora3_forecast_hour(ii),var1d_time_nc(ii)
            ii=ii+1
            do while (ii.le.n_hours_input.and..not.new_nora3_forecast(ii))
                var3d_nc(:,:,:,ii)=var3d_nc_in(:,:,:,nora3_forecast_hour(ii)) 
                var1d_nc(:,ii)=var1d_nc_in(:,nora3_forecast_hour(ii))
                var1d_time_nc(ii)=var1d_time_nc_in(nora3_forecast_hour(ii))
                write(unit_logfile,'(2i,es16.8)') ii,nora3_forecast_hour(ii),var1d_time_nc(ii)
                ii=ii+1
            enddo
            
            !write(*,*) 'HERE1: ',var1d_nc(time_index,1)
        
    endif !new forecast   
           ! write(*,*) 'HERE2: ',var1d_nc(time_index,1)

    enddo !jj
            ! write(*,*) 'HERE3: ',var1d_nc(time_index,1)
   

    dim_length_nc(time_index)=n_hours_input
    start_dim_nc(time_index)=1
    end_dim_nc(time_index)=n_hours_input
    
    do ii=start_dim_nc(time_index),end_dim_nc(time_index)
    call number_to_date((var1d_time_nc(ii)+1.)/dble(3600.*24.),a)
    write(*,'(7i8,es16.8)') ii, a(1:6),var1d_time_nc(ii)!/3600./24.
    enddo
    
    !deallocate (var3d_nc_dp)
    deallocate (var2d_nc_dp)
    !deallocate (var4d_nc_dp)
    if (allocated(var4d_nc)) deallocate(var4d_nc)
    if (allocated(var3d_nc_in)) deallocate(var3d_nc_in)
    
    end subroutine NORTRIP_read_nora3_netcdf4

    
