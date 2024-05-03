subroutine NORTRIP_multiroad_read_meteo_obs_data
    
    use NORTRIP_multiroad_index_definitions
    
    implicit none
          
    !Local variables     
    integer i
    integer exists
    
    character(256) filename_temp
    character(256) temp_str,temp_str1,temp_str2
    character(256) temp_str_array(30)
    character(64) header_str(30),read_str(30),match_str
    integer index_val,index_val1,index_val2,index_val3 !TODO: Hva er 1, 2, 3?
    integer i_head
    integer unit_in
    integer ro,k,jj,ii,ro2,t,j
    real, allocatable :: input_array(:,:,:)
    real, allocatable :: input_array_line(:,:)
    logical start_time_index_meteo_obs_found,end_time_index_meteo_obs_found
    integer hours_time_index_meteo_obs
    integer, allocatable :: meteo_obs_ID_temp(:)
    integer counter
    real available_meteo_obs_fraction(num_var_meteo)
    real minimum_meteo_obs_criteria
    double precision date_num_temp
    integer a(num_date_index)
    real val_temp
    integer pos(30)
    logical semi_col_found
    logical  multiple_obs_files
    integer n_read_obs_files
    integer n_meteo_obs_station_data
    integer, allocatable :: meteo_obs_inputdata_available(:)
    integer, allocatable :: n_meteo_obs_date_counter(:)
    integer, allocatable :: station_id_temp(:)
    integer, allocatable :: counter_id(:)
    real, allocatable :: repeat_data(:)

    integer repeat_count
    integer :: max_count=5
    real :: max_hop=10.
    real :: min_val=-40.
    real :: max_val=+40.
    real :: max_diff_ta_tv=15.
    logical :: test_repetition=.true.
    
    !Functions
    double precision date_to_number
    character(256) replace_string_char

        

    write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Reading observed meteorological data (NORTRIP_multiroad_read_meteo_obs_data)'
	write(unit_logfile,'(A)') '================================================================'

    !Read in the meteo obs metadata file
    !Test existence of the filename. If does not exist then use default
    filename_temp=filename_meteo_obs_metadata
    inquire(file=trim(filename_temp),exist=exists)
    if (.not.exists) then
        write(unit_logfile,'(A,A)') ' WARNING: Meteo obs metadata file does not exist: ', trim(filename_temp)
        write(unit_logfile,'(A)') ' Will use the model data for this time period'
        return
    endif
    
    !Open the obs file for reading
    unit_in=40
    open(unit_in,file=filename_temp,access='sequential',status='old',readonly)  
    write(unit_logfile,'(2A)') ' Opening obs meteo metadata file: ',trim(filename_temp)
    
    !This file is a fixed format approximately as it comes out of KDVH but with names as single strings
    !Only accounts for height, not positional differences as these are given in lat lon
    n_meteo_obs_stations=8 !TODO: Why is this set to 8?
    

    !Find out how long the file is, reading a dummy variable
    read(unit_in,'(a)',end=2) temp_str !Read the header string
    index_val=0
    do while(.not.eof(unit_in))
        index_val=index_val+1
        read(unit_in,*,ERR=2)
    enddo  
2   write(unit_logfile,*) 'Number of rows= ',index_val
    n_meteo_obs_stations=index_val
    
    allocate (meteo_obs_ID(n_meteo_obs_stations))
    allocate (meteo_obs_name(n_meteo_obs_stations))
    allocate (meteo_obs_position(num_meteo_obs_position,n_meteo_obs_stations))
    
    rewind(unit_in)
    read(unit_in,'(a)',end=3) temp_str !Read the header string
    if (index(temp_str,';').eq.1) then
        !Looks for ; at the start of the file. This detmines if the file is a ; seperated files, as used in RoadMET, or a normal BB file
        !Search for header that starts with 'Stnr'
        do while (index(temp_str,'Stnr').ne.1)
            read(unit_in,'(a)',end=3) temp_str
        enddo
        i=0
        !Stnr;Name;Operates from;Operates until;Altitude;Latitude;Longitude;Municipality;County;Region/Country;TV-Fromdate;TV-Until
        
        do while(.not.EOF(unit_in))
            i=i+1
            read(unit_in,'(a)',end=3) temp_str !Read the string
            temp_str1=temp_str
            !pos=index(temp_str1,';')
            counter=0
            semi_col_found=.true.
            !write(*,*) trim(temp_str1)
            do while (semi_col_found)
                counter=counter+1
                pos(counter)=index(temp_str1,';')
                if (pos(counter).gt.0) then
                    semi_col_found=.true.
                else
                    semi_col_found=.false.
                endif
                temp_str_array(counter)=temp_str1(1:pos(counter)-1);
                temp_str1=temp_str1(pos(counter)+1:)
                !write(*,*) trim(temp_str1)
            end do
            !write(*,*) pos
            !write(*,*) counter,trim(temp_str_array(1)),'  ',trim(temp_str_array(2)),'  ',trim(temp_str_array(5))
            read (temp_str_array(1),*) meteo_obs_ID(i)
            meteo_obs_name(i)=temp_str_array(2)
            if (temp_str_array(5).gt.' ') then
                read (temp_str_array(5),*) meteo_obs_position(meteo_obs_height_index,i)
            else
                write(unit_logfile,*) 'WARNING: No elevation for site ',trim(meteo_obs_name(i)), ' Setting to 0 m'
                meteo_obs_position(meteo_obs_height_index,i)=0.
            endif          
            read (temp_str_array(6),*) meteo_obs_position(meteo_obs_lat_index,i)
            read (temp_str_array(7),*) meteo_obs_position(meteo_obs_lon_index,i)
            
            write(unit_logfile,'(i10,a24,f10.1,f10.5,f10.5)') meteo_obs_ID(i),trim(meteo_obs_name(i)) &
                ,meteo_obs_position(meteo_obs_height_index,i),meteo_obs_position(meteo_obs_lat_index,i),meteo_obs_position(meteo_obs_lon_index,i)
            index_val=index_val+1
        enddo
        write(unit_logfile,*) 'Number of stations in ; seperated file = ',index_val
        n_meteo_obs_stations=i

    else
        
        write(unit_logfile,'(a10,a24,a10,a10,a10)') 'ID','Name','Height','Lat','Lon'
        do i=1,n_meteo_obs_stations
            !read(unit_in,'(i,a,a,a,f,f,f)',end=3) meteo_obs_ID(i),meteo_obs_name(i),temp_str,temp_str &
            read(unit_in,*,end=3) meteo_obs_ID(i),meteo_obs_name(i),temp_str,temp_str &
                ,meteo_obs_position(meteo_obs_height_index,i),meteo_obs_position(meteo_obs_lat_index,i),meteo_obs_position(meteo_obs_lon_index,i)
            write(unit_logfile,'(i10,a24,f10.1,f10.5,f10.5)') meteo_obs_ID(i),trim(meteo_obs_name(i)) &
                ,meteo_obs_position(meteo_obs_height_index,i),meteo_obs_position(meteo_obs_lat_index,i),meteo_obs_position(meteo_obs_lon_index,i)  
        enddo
    endif
    
3   close(unit_in,status='keep')
    
    !Transform to UTM coordinates
    do i=1,n_meteo_obs_stations
        call LL2UTM(1,utm_zone,meteo_obs_position(meteo_obs_lat_index,i),meteo_obs_position(meteo_obs_lon_index,i),meteo_obs_position(meteo_obs_y_index,i),meteo_obs_position(meteo_obs_x_index,i))
        !write(*,*) meteo_obs_ID(i),meteo_obs_position(meteo_obs_lat_index,i),meteo_obs_position(meteo_obs_lon_index,i),meteo_obs_position(meteo_obs_y_index,i),meteo_obs_position(meteo_obs_x_index,i)
    enddo
    
    
    !If read obs data not specified then return without doing anything
    if (replace_meteo_with_obs.eq.0) then
        write(unit_logfile,'(a)') 'No observational data used in calculation'
        return
    endif
   
    !Read in the time varying data
    !Based on output from 'http://klapp/metnopub/production/metno'
    !http://metklim.met.no/klima/userservices/urlinterface/brukerdok
    
    filename_temp=trim(inpath_meteo_obs_data)//trim(infile_meteo_obs_data)
    !write(*,*) filename_temp
    
    !Test for the 'station_str' in the obs filename which indicates that it is multiple stations with multiple files
    if (index(filename_temp,'station_str').gt.0) then
            multiple_obs_files=.true.
            n_read_obs_files=n_meteo_obs_stations
    else
            multiple_obs_files=.false.
            n_read_obs_files=1
    endif
    
    !Test existence of the filename when there is only one file. If does not exist then use default
    if (.not.multiple_obs_files) then
        inquire(file=trim(filename_temp),exist=exists)
        if (.not.exists) then
            write(unit_logfile,'(A,A)') ' WARNING: Meteo obs file does not exist: ', trim(filename_temp)
            write(unit_logfile,'(A)') ' Will use the model data for this time period'
            return
        endif
    endif
            
    if (.not.multiple_obs_files) then
        !Open the obs file for reading
        unit_in=40
    
        open(unit_in,file=filename_temp,access='sequential',status='old',readonly)  
        write(unit_logfile,'(2A)') ' Opening obs meteo file: ',trim(filename_temp)
    
        !rewind(unit_in)
    
        !Read in header
        !Stnr Year Month Day Time(NMT) UU PO TA RR_1 FF DD QSI NN TV
 
        !Read header string and split at spaces. Assumes single space seperation
        temp_str1=''
        temp_str2='Not available'
        index_val=1
        i_head=0
        read(unit_in,'(a)',end=10) temp_str !Read the header string
    
        do while (len(trim(temp_str)).ne.0)
            index_val=index(temp_str,' ')
            !write(*,*) index_val,trim(temp_str),len(trim(temp_str))
            if (index_val.gt.1) then
                temp_str1=temp_str(1:index_val-1)
                i_head=i_head+1
                temp_str=temp_str(index_val+1:)
                header_str(i_head)=temp_str1
                !write(unit_logfile,*) i_head,len(temp_str),index_val,trim(header_str(i_head))
            else
                temp_str=temp_str(index_val+1:)
                !write(*,*) index_val,len(temp_str),'temp_str',trim(temp_str)
            endif               
        end do
        write(unit_logfile,*) 'Number of columns= ',i_head

        !Find out how long the file is, reading a dummy variable
        index_val=0
        do while(.not.eof(unit_in))
            index_val=index_val+1
            read(unit_in,*,ERR=5)
        enddo  
5       write(unit_logfile,*) 'Number of rows= ',index_val

        allocate (station_id_temp(index_val))
    
        !Read in all the available IDs
        rewind(unit_in)
        read(unit_in,'(a)',end=7) temp_str !Read the header string
        index_val=0
        do while(.not.eof(unit_in))
            index_val=index_val+1
            read(unit_in,*,end=7) station_id_temp(index_val)
            !write(*,*) index_val,station_id_temp(index_val)
        enddo  
7       write(unit_logfile,*) 'Reading in station IDs ',index_val

        !Search for matching ID's in the metadata
        allocate (meteo_obs_inputdata_available(n_meteo_obs_stations))
        allocate (n_meteo_obs_date_counter(n_meteo_obs_stations))
    
        meteo_obs_inputdata_available=0
        n_meteo_obs_date=0
        do i=1,n_meteo_obs_stations
            n_meteo_obs_date_counter(i)=0
            do j=1,index_val
                if (station_id_temp(j).eq.meteo_obs_ID(i)) then
                    meteo_obs_inputdata_available(i)=1
                    n_meteo_obs_date_counter(i)=n_meteo_obs_date_counter(i)+1 !Sets the number of dates based on the last id read
                endif
            enddo
        enddo
        n_meteo_obs_station_data=sum(meteo_obs_inputdata_available)
        n_meteo_obs_date=maxval(n_meteo_obs_date_counter)   !Uses the maximum value which should be correct
    
        write(unit_logfile,*) 'Number of stations= ',n_meteo_obs_stations
        write(unit_logfile,*) 'Number of stations in file= ',n_meteo_obs_station_data
        write(unit_logfile,*) 'Number of dates= ',n_meteo_obs_date
    
        allocate (input_array(i_head,n_meteo_obs_date,n_meteo_obs_stations))
        input_array=missing_data
    
        start_dim_meteo_obs=1
        end_dim_meteo_obs=n_meteo_obs_date
    
        rewind(unit_in)
        read(unit_in,*,ERR=10) !Skip header
    
        allocate (input_array_line(i_head,index_val))

        index_val=0
        do while(.not.eof(unit_in))
            index_val=index_val+1
            read(unit_in,*,end=8) read_str(1:i_head)
            !write(*,*) read_str
            do ii=1,i_head         
                    !Set these two string types to missing data for the old obs datasets
                    index_val3=index(read_str(ii),'x')
                    index_val1=index(read_str(ii),'-')
                    index_val2=index(read_str(ii),'.')
                    if (index_val1.eq.len(trim(read_str(ii))).or.index_val2.eq.len(trim(read_str(ii))).or.index_val3.eq.len(trim(read_str(ii)))) then
                        input_array_line(ii,index_val)=missing_data
                    else
                        read(read_str(ii),*) input_array_line(ii,index_val)
                    endif
            enddo
        enddo  

8       allocate (counter_id(n_meteo_obs_stations))
        counter_id=0
        input_array=missing_data
        do i=1,n_meteo_obs_stations
            do j=1,index_val
                if (station_id_temp(j).eq.meteo_obs_ID(i)) then
                    counter_id(i)=counter_id(i)+1
                    input_array(:,counter_id(i),i)=input_array_line(:,j)

                endif
            enddo
        enddo

10      close(unit_in,status='keep')
    
    else       
        do ro=1,n_read_obs_files
            
            !Open the obs file for reading
            unit_in=40
        
            temp_str='station_str'
            write(temp_str2,'(i0)') meteo_obs_ID(ro)
            filename_temp=trim(inpath_meteo_obs_data)//trim(infile_meteo_obs_data)
            filename_temp=replace_string_char(trim(temp_str2),trim(temp_str),filename_temp)

            inquire(file=trim(filename_temp),exist=exists)
            if (.not.exists) then
                write(unit_logfile,'(A,A)') ' WARNING: Meteo obs data file does not exist: ', trim(filename_temp)
                goto 12
            else
                    write(unit_logfile,'(2A)') ' Opening obs meteo file: ',trim(filename_temp)
            endif
            open(unit_in,file=filename_temp,access='sequential',status='old',readonly)  
        
            !rewind(unit_in)
        
            !Read in header
            !Stnr Year Month Day Time(NMT) UU PO TA RR_1 FF DD QSI NN TV
    
            !Read header string and split at spaces. Assumes single space seperation
            temp_str1=''
            temp_str2='Not available'
            index_val=1
            i_head=0
            read(unit_in,'(a)',end=11) temp_str !Read the header string
            !Check if any data given the ndap format for no data
            !write(*,*) temp_str
            if (index(temp_str,'****').gt.0) then
                input_array(:,:,ro)=missing_data
                goto 11
            endif
        
            do while (len(trim(temp_str)).ne.0)
                index_val=index(temp_str,' ')
                !write(*,*) index_val,trim(temp_str),len(trim(temp_str))
                if (index_val.gt.1) then
                    temp_str1=temp_str(1:index_val-1)
                    i_head=i_head+1
                    temp_str=temp_str(index_val+1:)
                    header_str(i_head)=temp_str1
                    !write(unit_logfile,*) i_head,len(temp_str),index_val,trim(header_str(i_head))
                else
                    temp_str=temp_str(index_val+1:)
                    !write(*,*) index_val,len(temp_str),'temp_str',trim(temp_str)
                endif        
            
            end do
            !write(unit_logfile,*) 'Number of columns= ',i_head

            !Find out how long the file is, reading a dummy variable
            index_val=0
            do while(.not.eof(unit_in))
                index_val=index_val+1
                read(unit_in,*,ERR=6)
            enddo  
            !write(unit_logfile,*) 'Number of rows= ',index_val

            !Read data
6           n_meteo_obs_date=int(index_val)
            !write(unit_logfile,*) 'Number of stations= ',ro,n_meteo_obs_stations
            !write(unit_logfile,*) 'Number of dates= ',n_meteo_obs_date
            !allocate (input_array(i_head,index_val))
            if (.not.allocated(input_array)) then
                allocate (input_array(i_head,n_meteo_obs_date,n_meteo_obs_stations))
                input_array=missing_data
            endif
        
            start_dim_meteo_obs=1
            end_dim_meteo_obs=n_meteo_obs_date
        
            rewind(unit_in)
            read(unit_in,*,ERR=6) !Skip header
        
            !read(unit_in,*) (((input_array(ii,jj,ro),ii=1,i_head),jj=1,n_meteo_obs_date),ro=1,n_meteo_obs_stations)
                do jj=1,n_meteo_obs_date
                    read(unit_in,*) read_str(1:i_head)
                    !write(*,'(<i_head>a10)') read_str(1:i_head)
                    do ii=1,i_head
                        !Set these two string types to missing data for the old obs datasets
                        index_val3=index(read_str(ii),'x')
                        index_val1=index(read_str(ii),'-')
                        index_val2=index(read_str(ii),'.')
                        if (index_val1.eq.len(trim(read_str(ii))).or.index_val2.eq.len(trim(read_str(ii))).or.index_val3.eq.len(trim(read_str(ii)))) then
                            input_array(ii,jj,ro)=missing_data
                        else
                            read(read_str(ii),*) input_array(ii,jj,ro)
                        endif
                    enddo          
                    !write(*,'(<i_head>f10.2)') input_array(:,jj,ro)
                enddo               
11        close(unit_in,status='keep')
12      enddo
    endif

    write(unit_logfile,'(a32,a14,a14,a14)') 'Parameter','First value','Last value','Mean value'
	write(unit_logfile,'(A)') '----------------------------------------------------------------'
    do i=1,i_head
        write(unit_logfile,'(a32,f14.2,f14.2,f14.2)') trim(header_str(i)), &
            input_array(i,1,1),input_array(i,n_meteo_obs_date,n_meteo_obs_stations), &
            sum(input_array(i,1:n_meteo_obs_date,1:n_meteo_obs_stations)/(n_meteo_obs_date*n_meteo_obs_stations))
    end do
    write(unit_logfile,'(A)') '----------------------------------------------------------------'

    allocate (meteo_obs_ID_data(n_meteo_obs_date,n_meteo_obs_stations))
    allocate (meteo_obs_date(num_date_index,n_meteo_obs_date))
    allocate (meteo_obs_data(num_var_meteo,n_meteo_obs_date,n_meteo_obs_stations))
    allocate (meteo_obs_ID_temp(n_meteo_obs_stations))
    
    !Match the dates based on the first stations data and reorder the headers
    meteo_obs_date=0
    do i=1,i_head
        if (index(header_str(i),'Year').ne.0) meteo_obs_date(year_index,:)=int(input_array(i,:,1))
        if (index(header_str(i),'Month').ne.0) meteo_obs_date(month_index,:)=int(input_array(i,:,1))
        if (index(header_str(i),'Day').ne.0) meteo_obs_date(day_index,:)=int(input_array(i,:,1))
        if (index(header_str(i),'Time').ne.0) meteo_obs_date(hour_index,:)=int(input_array(i,:,1))
        if (index(header_str(i),'Stnr').ne.0) meteo_obs_ID_temp(:)=int(input_array(i,1,:))
    enddo

    !Fix to be 0-23 instead of 1-24 hours
    !Do not need this any more
    !do t=start_dim_meteo_obs,end_dim_meteo_obs
    !    date_num_temp=date_to_number(meteo_obs_date(:,t))
    !    a=0
    !    call number_to_date(date_num_temp,a)
    !    meteo_obs_date(:,t)=a
        !write(*,*) meteo_obs_date(:,t)
    !enddo
    !stop
    !Very weird. Including the call above changes date_nc. How is this possible?
    !Because the leap month number of days was rremembered and not reedefined wiht the data statement in 'number_to_date'
    
    !Obs are in UTC, convert to NMT
    !DIFUTC_H is UTC relative to local, so negative if local time is ahead
    !Do not convert to NMT. Rather change traffic times to UTC
     !do t=start_dim_meteo_obs,end_dim_meteo_obs
        !call incrtm(int(-DIFUTC_H),meteo_obs_date(1,t),meteo_obs_date(2,t),meteo_obs_date(3,t),meteo_obs_date(4,t))
     !enddo

    
    !Match the observed meteorology date indexes to the prescribed dates
    !TODO: Why does it have to match start or stop? This means that it will not replace hours in the middle of the prescribed date range? Replace this with a date-to-date match
    start_time_index_meteo_obs=0
    end_time_index_meteo_obs=0
    start_time_index_meteo_obs_found=.false.
    end_time_index_meteo_obs_found=.false.
    do t=start_dim_meteo_obs,end_dim_meteo_obs
        if (meteo_obs_date(year_index,t).eq.date_data(year_index,1) &
            .and.meteo_obs_date(month_index,t).eq.date_data(month_index,1) &
            .and.meteo_obs_date(day_index,t).eq.date_data(day_index,1) &
            .and.meteo_obs_date(hour_index,t).eq.date_data(hour_index,1)) then
            start_time_index_meteo_obs=t
            start_time_index_meteo_obs_found=.true.
        endif 
        if (meteo_obs_date(year_index,t).eq.date_data(year_index,n_hours_input) &
            .and.meteo_obs_date(month_index,t).eq.date_data(month_index,n_hours_input) &
            .and.meteo_obs_date(day_index,t).eq.date_data(day_index,n_hours_input) &
            .and.meteo_obs_date(hour_index,t).eq.date_data(hour_index,n_hours_input)) then
            end_time_index_meteo_obs=t
            end_time_index_meteo_obs_found=.true.
        endif        
    enddo

    hours_time_index_meteo_obs=end_time_index_meteo_obs-start_time_index_meteo_obs+1
    write(unit_logfile,'(a32,i6,a32,i6,a32,i6)') ' Start_time_index_meteo_obs= ',start_time_index_meteo_obs, &
        ' End_time_index_meteo_obs= ',end_time_index_meteo_obs,' Hours_meteo_obs= ',hours_time_index_meteo_obs
        write(unit_logfile,'(a32,6i6)') ' Start date input = ',start_date_input
        write(unit_logfile,'(a32,6i6)') ' End date input = ',end_date_input
        write(unit_logfile,'(a32,6i6)') ' Start date meteo obs = ',meteo_obs_date(:,start_dim_meteo_obs)
        write(unit_logfile,'(a32,6i6)') ' End date meteo obs = ',meteo_obs_date(:,end_dim_meteo_obs)
   !write(*,*) meteo_obs_date(:,:)
    !Note hours go from 1 to 24. Is this a problem?
    !Yes it is, need to fix this
        !if (start_time_index_meteo_obs.ne.2) then
        !write(*,*) 'Wrong obs start index. Stopping: !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',start_time_index_meteo_obs
        !stop
        !endif

    if (.not.start_time_index_meteo_obs_found.or..not.end_time_index_meteo_obs_found) then
        write(*,'(A)') ' WARNING: Input time start or stop date not found in meteo obs data. Will not use observations'
        return
    endif
    
    !Sort in order of the metadata ID's and put in the correct columns
    meteo_obs_data=missing_data
    do ro=1,n_meteo_obs_stations
        do ro2=1,n_meteo_obs_stations
            if (meteo_obs_ID(ro).eq.meteo_obs_ID_temp(ro2)) then
                do i=1,i_head
                    do j=1,num_var_meteo
                        if (index(trim(header_str(i)),trim(var_name_meteo_obs(j))).ne.0.and.len(trim(var_name_meteo_obs(j))).gt.0) then
                            meteo_obs_data(j,:,ro)=input_array(i,:,ro2)
                        endif
                    enddo     
                enddo
            endif
        enddo
    enddo

    !Create the new meteo data using the first station in metadata list and filling up missing data downover
    !Temperature adjusted for lapse rate
    allocate (meteo_obs_data_final(num_var_meteo,hours_time_index_meteo_obs))
    
    !Basic quality control for observed temperature
    !Test for extremes
    do t=start_time_index_meteo_obs,end_time_index_meteo_obs
        do ro=1,n_meteo_obs_stations
            !Upper and lower limits of -35 and +25 for this period of the year
            if (meteo_obs_data(road_temperature_index,t,ro).lt.min_val.or.meteo_obs_data(road_temperature_index,t,ro).gt.max_val) then
                meteo_obs_data(road_temperature_index,t,ro)=missing_data
            endif
            if (meteo_obs_data(temperature_index,t,ro).lt.min_val.or.meteo_obs_data(temperature_index,t,ro).gt.max_val) then
                meteo_obs_data(temperature_index,t,ro)=missing_data
            endif
            !If difference air and road is > 15 C then discard
            !if (abs(meteo_obs_data(road_temperature_index,t,ro)-meteo_obs_data(temperature_index,t,ro)).gt.max_diff_ta_tv) then
            !    meteo_obs_data(road_temperature_index,t,ro)=missing_data
            !    meteo_obs_data(temperature_index,t,ro)=missing_data
            !endif
       enddo
    enddo
 
    !Test for repetition of the same value
    if (test_repetition) then
    
        if (.not.allocated(repeat_data)) allocate (repeat_data(n_meteo_obs_date))
    
    do ro=1,n_meteo_obs_stations
        repeat_count=0
        repeat_data=meteo_obs_data(road_temperature_index,:,ro)
        do t=start_time_index_meteo_obs+1,end_time_index_meteo_obs
            if (repeat_data(t)-repeat_data(t-1).eq.0) then
                repeat_count=repeat_count+1
            else
                repeat_count=0
            endif
            if (repeat_count.ge.max_count) then
                meteo_obs_data(road_temperature_index,t-repeat_count:t,ro)=missing_data                       
            endif
            !Test for hop
            if (abs(repeat_data(t)-repeat_data(t-1)).gt.max_hop.and.repeat_data(t-1).ne.missing_data) then
                meteo_obs_data(road_temperature_index,t,ro)=missing_data                       
            endif
        enddo
        repeat_count=0
        repeat_data=meteo_obs_data(temperature_index,:,ro)
        do t=start_time_index_meteo_obs+1,end_time_index_meteo_obs
            if (repeat_data(t)-repeat_data(t-1).eq.0) then
                repeat_count=repeat_count+1
            else
                repeat_count=0
            endif
            if (repeat_count.ge.max_count) then
                meteo_obs_data(temperature_index,t-repeat_count:t,ro)=missing_data
            endif
            !Test for hop
            if (abs(repeat_data(t)-repeat_data(t-1)).gt.max_hop.and.repeat_data(t-1).ne.missing_data) then
                meteo_obs_data(temperature_index,t,ro)=missing_data                       
            endif
        enddo

    
    enddo
    endif
    
    !Fill with the primary (station 1) meteo data file
    meteo_obs_data_final(:,:)=meteo_obs_data(:,start_time_index_meteo_obs:end_time_index_meteo_obs,1)
    
    do ro=2,n_meteo_obs_stations
        do j=1,num_var_meteo
            i=0
            do t=start_time_index_meteo_obs,end_time_index_meteo_obs
                i=i+1
                if (meteo_obs_data_final(j,i).eq.missing_data) then  
                    if ((j.eq.temperature_index.or.j.eq.road_temperature_index)) then
                        if (meteo_obs_data(j,t,ro).ne.missing_data) then
                            meteo_obs_data_final(j,i)=meteo_obs_data(j,t,ro) &
                            +(meteo_obs_position(meteo_obs_height_index,1)-meteo_obs_position(meteo_obs_height_index,ro))*lapse_rate
                        endif
                    elseif ((j.eq.shortwaveradiation_index)) then
                        if (meteo_obs_data(j,t,ro).ne.missing_data) then
                            meteo_obs_data_final(j,i)=max(0.0,meteo_obs_data(j,t,ro))
                        endif                     
                    else
                        meteo_obs_data_final(j,i)=meteo_obs_data(j,t,ro)
                    endif
                endif
            enddo
        enddo   
    enddo

    !Calculate the average instead
    do j=1,num_var_meteo
        if (replace_which_meteo_with_obs(j).eq.2) then
            meteo_obs_data_final(j,:)=missing_data
            i=0
            do t=start_time_index_meteo_obs,end_time_index_meteo_obs
                i=i+1
                counter=0
                val_temp=0.
                do ro=1,n_meteo_obs_stations           
                    if (meteo_obs_data(j,t,ro).ne.missing_data) then
                        if ((j.eq.temperature_index.or.j.eq.road_temperature_index)) then
                            val_temp=val_temp+meteo_obs_data(j,t,ro) &
                                    +(meteo_obs_position(meteo_obs_height_index,1)-meteo_obs_position(meteo_obs_height_index,ro))*lapse_rate
                        elseif ((j.eq.shortwaveradiation_index)) then
                            val_temp=val_temp+max(0.0,meteo_obs_data(j,t,ro))
                        else
                            val_temp=val_temp+meteo_obs_data(j,t,ro)
                        endif                       
                        counter=counter+1;
                        !write(*,*) t,ro,counter,val_temp
                    endif
                enddo
                if (counter.gt.0) then
                    meteo_obs_data_final(j,i)=val_temp/counter
                else
                    meteo_obs_data_final(j,i)=missing_data
                endif
            enddo       
        endif
    enddo
    
    !Special case for surface temperature where we take the height corrected average of all the available surface temperatures. Put this into the final value
    meteo_obs_data_final(road_temperature_index,:)=missing_data
    i=0
    do t=start_time_index_meteo_obs,end_time_index_meteo_obs
        i=i+1
        counter=0
        val_temp=0.
        do ro=1,n_meteo_obs_stations           
            if (meteo_obs_data(road_temperature_index,t,ro).ne.missing_data) then
                val_temp=val_temp+meteo_obs_data(road_temperature_index,t,ro) &
                        +(meteo_obs_position(meteo_obs_height_index,1)-meteo_obs_position(meteo_obs_height_index,ro))*lapse_rate
                counter=counter+1;
                !write(*,*) t,ro,counter,val_temp
            endif
        enddo
        if (counter.gt.0) then
            meteo_obs_data_final(road_temperature_index,i)=val_temp/counter
        else
            meteo_obs_data_final(road_temperature_index,i)=missing_data
        endif  
    enddo
    !meteo_obs_data_final(:,:)=meteo_obs_data(:,start_time_index_meteo_obs:end_time_index_meteo_obs,1)

    !Determine how much is still missing
    write(unit_logfile,'(a)') ' Available meteo obs data'
    write(unit_logfile,'(a12,a12,a14)') 'Index','Name','Available(%)'
    do j=1,num_var_meteo
        counter=0
        do i=1,hours_time_index_meteo_obs
            if (meteo_obs_data_final(j,i).ne.missing_data) then
                counter=counter+1
            endif
        enddo         
        available_meteo_obs_fraction(j)=real(counter)/real(hours_time_index_meteo_obs)*100.0
        if (len(trim(var_name_meteo_obs(j))).gt.0) then 
            write(unit_logfile,'(i12,a12,f14.1)') j,trim(var_name_meteo_obs(j)),available_meteo_obs_fraction(j)
        endif
    enddo
            
    !Criteria for using the observations at all
    minimum_meteo_obs_criteria=0.
    if (available_meteo_obs_fraction(temperature_index).gt.minimum_meteo_obs_criteria.or. &
        available_meteo_obs_fraction(relhumidity_index).gt.minimum_meteo_obs_criteria.or. &
        available_meteo_obs_fraction(precip_index).gt.minimum_meteo_obs_criteria.or. &
        available_meteo_obs_fraction(shortwaveradiation_index).gt.minimum_meteo_obs_criteria.or. &
        available_meteo_obs_fraction(longwaveradiation_index).gt.minimum_meteo_obs_criteria.or. &
        available_meteo_obs_fraction(dir_wind_index).gt.minimum_meteo_obs_criteria.or. &
        available_meteo_obs_fraction(speed_wind_index).gt.minimum_meteo_obs_criteria) then
        
            write(unit_logfile,'(a)') ' Sufficient observational data available. Will replace model data'
            meteo_obs_data_available=.true.
    else
            write(unit_logfile,'(a)') ' WARNING: Insufficient observational data available. Will not replace model data'
            meteo_obs_data_available=.false.
        
    endif
    
    !Substitute
    
    !If there are still missing bits, no more than 2 hours, then interpolate linearly
    
    !If still missing then use model. i.e. do nothing
    
    if (allocated(input_array)) deallocate(input_array)
    if (allocated(input_array_line)) deallocate(input_array_line)

    
    end subroutine NORTRIP_multiroad_read_meteo_obs_data
