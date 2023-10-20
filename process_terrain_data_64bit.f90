!process_terrain_data.f90
    subroutine process_terrain_data_64bit
    
    use NORTRIP_multiroad_index_definitions

    implicit none

    
    integer ncols
    integer nrows
    real cellsize
 
    character(256), allocatable ::  filename_ascii(:)
    character(256) temp_name
    integer :: unit_out=30
    character(256) temp_str
    integer temp_int
    real temp_real
    integer i,ii,j,jj
    integer n_grid_search_canyon,n_grid_search_skyview
    integer grid_search_index
    real x_point,y_point
    real lat_point,lon_point
    integer i_point,j_point
    integer i_min_search,i_max_search,j_min_search,j_max_search
    real search_min,search_max
    real road_angle,stepsize
    real x_point_step,y_point_step
    integer i_point_step,j_point_step
    real dist_step,height_angle_max,height_angle
    real height_angle_max_final(2)
    real height_angle_max_av(2)
    real width_canyon(2),height_canyon(2)
    real :: pi=3.14159
    integer s,f,ro,ro2
    integer exists
    integer count_found
    real y_point_north,x_point_north,y_point_south,x_point_south
    integer i_point_north,j_point_north,i_point_south,j_point_south
    real :: max_slope=15.
    character(256) message
    integer south_index,north_index
    parameter (south_index=1,north_index=2)
    
    !set search parameters for shading
    real :: length_segment_init=50.
    real :: kerb_width=5.
    real :: max_canyon_search_distance=100.
    real :: max_skyview_search_distance=20000.
    real :: forest_kerb_width=10.
    real :: forest_height=10.
    integer :: is_forest=0
    
    integer seg,n_segments,n_valid_segment
    real length_segment
    real height_angle_max_av_canyon(2),height_angle_max_canyon(2)
    real height_angle_max_skyview(n_skyview)
    real height_angle_max_av_skyview(n_skyview)
    logical :: first_valid_segment=.false.

    real length_sublink
 

    !n_skyview set in definitions
    allocate (az_skyview(n_skyview,n_roadlinks))
    allocate (zen_skyview(n_skyview,n_roadlinks))
    allocate (dis_skyview(n_skyview,n_roadlinks))
    allocate (height_skyview(n_skyview,n_roadlinks))
    
	write(unit_logfile,'(A)') '================================================================'
	write(unit_logfile,'(A)') 'Calculating shading parameters (process_terrain_data_64bit)'
	write(unit_logfile,'(A)') '================================================================'

    az_skyview=0.
    zen_skyview=90.
    dis_skyview=0.
    height_skyview=0.

    if (n_dem_files.lt.1) then
        write(unit_logfile,'(A)') ' WARNING: No terrain files available. Will not calculate skyview or street canyon parameters'
        return
    endif
    
    allocate (filename_ascii(n_dem_files))
    do f=1,n_dem_files
        filename_ascii(f)=trim(pathname_terrain)//trim(filename_terrain_data(f))
        !write(*,*) trim(filename_ascii(f))
    enddo
    
    count_found=0
    
    do s=1,n_skyview
        az_skyview(s,:)=(s-1)*360./n_skyview
    enddo
    
    do f=1,n_dem_files

        !Test existence of the roadlink filename (1). If does not exist then use default
        !write(*,*) trim(filename_ascii(f)),' ',trim(filename_terrain_data(f))
        inquire(file=trim(filename_ascii(f)),exist=exists)
        if (.not.exists) then
            write(unit_logfile,'(A,A)') ' WARNING: Terrain file does not exist: ', trim(filename_ascii(f))
            goto 10
        endif
        
        if (index(filename_ascii(f),'.asc').gt.0) then
            write(unit_logfile,'(A)') ' Reading terrain asc file'
            call read_esri_ascii_terrain_file(filename_ascii(f),ncols,nrows,cellsize)
        elseif (index(filename_ascii(f),'.nc').gt.0) then
            write(unit_logfile,'(A)') ' Reading terrain netcdf file '           
            call read_netcdf4_terrain_file(filename_ascii(f),ncols,nrows,cellsize)
        else
            write(unit_logfile,'(A,A)') ' WARNING: Terrain file type not netcdf or asc. Cannot read ', trim(filename_ascii(f))
            goto 10
        endif
        
        if (utm_zone.ne.terrain_utm_zone) then
            write(unit_logfile,'(A,2i)') ' WARNING: Terrain is not the same projection as the road (road utm, terrain utm)', utm_zone,terrain_utm_zone
        endif
        
        !Define a point for testing
        !x_point=xllcorner+140.0*cellsize
        !y_point=yllcorner+122.4*cellsize
    
        !Loop through road links
        do ro=1,n_roadlinks
        
            first_valid_segment=.true.
        
            !Loop through the sublinks
            do ro2=1,inputdata_int_rl(n_subnodes_rl_index,ro)-1
 
            !call distrl(save_road_x(j),save_road_y(j),inputdata_rl_sub(x1_rl_index,ii,i),inputdata_rl_sub(y1_rl_index,ii,i),inputdata_rl_sub(x2_rl_index,ii,i),inputdata_rl_sub(y2_rl_index,ii,i),temp_val,temp_val2,distance_to_link)!(X0,Y0,X1,Y1,X2,Y2,XM,YM,DM)
            length_sublink=sqrt((inputdata_rl_sub(x1_rl_index,ro2,ro)-inputdata_rl_sub(x2_rl_index,ro2,ro))**2+(inputdata_rl_sub(y1_rl_index,ro2,ro)-inputdata_rl_sub(y2_rl_index,ro2,ro))**2)
            n_segments=int(length_sublink/length_segment_init)+1
            length_segment=length_sublink/n_segments

            !n_segments=int(inputdata_rl(length_rl_index,ro)/length_segment_init)+1
            !n_segments=max(1,n_segments)
            !length_segment=inputdata_rl(length_rl_index,ro)/n_segments
            !write(*,*) ro,n_segments,length_segment
            
            
            
            do seg=0,n_segments-1
            
                !x_point=(inputdata_rl(x1_rl_index,ro)*(n_segments-seg-.5)+inputdata_rl(x2_rl_index,ro)*(seg+.5))/n_segments
                !y_point=(inputdata_rl(y1_rl_index,ro)*(n_segments-seg-.5)+inputdata_rl(y2_rl_index,ro)*(seg+.5))/n_segments
                x_point=(inputdata_rl_sub(x1_rl_index,ro2,ro)*(n_segments-seg-.5)+inputdata_rl_sub(x2_rl_index,ro2,ro)*(seg+.5))/n_segments
                y_point=(inputdata_rl_sub(y1_rl_index,ro2,ro)*(n_segments-seg-.5)+inputdata_rl_sub(y2_rl_index,ro2,ro)*(seg+.5))/n_segments
     
                !Here we assume that the road link data is in UTM coordinates and the terrain data is in UTM33
                !If utm_zone <> terrain_utm_zone for the road links then convert the road data points to utm 33
                if (utm_zone.ne.terrain_utm_zone) then                   
                    call UTM2LL(utm_zone,y_point,x_point,lat_point,lon_point)
                    call LL2UTM(1,terrain_utm_zone,lat_point,lon_point,y_point,x_point)
                endif
                
                
                !Determine the grid index it is in
                i_point=int((x_point-x_array(1)+cellsize)/(x_array(ncols)-x_array(1)+cellsize)*ncols+1)
                j_point=int((y_point-y_array(1)+cellsize)/(y_array(nrows)-y_array(1)+cellsize)*nrows+1)
                !write(*,*) 'Grid index :',i_point,j_point
    
                !Mark it as out of bounds    
                if (i_point.le.0.or.i_point.gt.ncols) i_point=0
                if (j_point.le.0.or.j_point.gt.nrows) j_point=0
    
                !Search for minimum in the local area
                if (i_point.ne.0.and.j_point.ne.0) then    
                    !Do not find minimum if grid size is > 15 m. So only for 10 m or 5 m terrain data
                    if (cellsize.gt.15) then
                        search_min=array(i_point,j_point)
                        search_max=array(i_point,j_point)
                    else
                        do grid_search_index=1,1
                            i_min_search=max(i_point-grid_search_index,1)
                            i_max_search=min(i_point+grid_search_index,ncols)
                            j_min_search=max(j_point-grid_search_index,1)
                            j_max_search=min(j_point+grid_search_index,nrows)
                            search_min=minval(array(i_min_search:i_max_search,j_min_search:j_max_search))
                            search_max=maxval(array(i_min_search:i_max_search,j_min_search:j_max_search))
                            !write(*,*) 'Distance (+/-): ',grid_search_index*cellsize,'Min: ',search_min,' Max: ',search_max
                        enddo
                    endif
                endif
    
                
                
                !Search in a particular direction
                road_angle=inputdata_rl(angle_rl_index,ro)
                stepsize=cellsize
                n_grid_search_canyon=int(max_canyon_search_distance/stepsize) !Number of grids to search in, out to a radius of max_canyon_search_distance
                width_canyon=stepsize
                height_canyon=0
                height_angle_max=0
                !search_min=array(i_point,j_point)
            
                !If the point is inside the terrain area
                if (i_point.ne.0.and.j_point.ne.0) then    
                    !write(*,*) ro,i_point,j_point
                    count_found=count_found+1
                    inputdata_rl(elevation_rl_index,ro)=array(i_point,j_point)
                
                    if (first_valid_segment) then
                        n_valid_segment=1
                    else
                        n_valid_segment=n_valid_segment+1
                    endif
                    
                    !Calculate canyon width and height
                    do j=south_index,north_index
                        height_angle_max=0
                        do i=1,n_grid_search_canyon
                            !Select north (2) or south (1) directions
                            if (j.eq.south_index) dist_step=i*stepsize
                            if (j.eq.north_index) dist_step=-i*stepsize
        
                            !Set position of the step
                            x_point_step=x_point+cos(pi/180.*road_angle)*dist_step
                            y_point_step=y_point-sin(pi/180.*road_angle)*dist_step

                            !Determine the grid index it is in
                            i_point_step=int((x_point_step-x_array(1)+cellsize)/(x_array(ncols)-x_array(1)+cellsize)*ncols+1)
                            j_point_step=int((y_point_step-y_array(1)+cellsize)/(y_array(nrows)-y_array(1)+cellsize)*nrows+1)

            
                            if (i_point_step.gt.1.and.i_point_step.lt.ncols.and.j_point_step.gt.1.and.j_point_step.lt.nrows) then
            
                            !Calculate the height angle
                            height_angle=atan((array(i_point_step,j_point_step)-search_min)/abs(dist_step))
                            if (height_angle.gt.height_angle_max) then
                                width_canyon(j)=dist_step
                                height_canyon(j)=max(array(i_point_step,j_point_step)-search_min,0.)
                                height_angle_max=height_angle
                            endif
                            !write(*,'(i6,5f12.2)') j,dist_step,x_point_step-x_point,y_point_step-y_point,height_angle,height_canyon(j)           
                            endif
        
                        enddo
                        height_angle_max_canyon(j)=height_angle_max
                    
                    enddo !Direction loop
    
                    if (first_valid_segment) then
                        height_angle_max_av_canyon=height_angle_max_canyon
                    else
                        height_angle_max_av_canyon=height_angle_max_av_canyon*(n_valid_segment-1.)/n_valid_segment+height_angle_max_canyon/n_valid_segment
                    endif
                    !write(*,*) ro,height_angle_max_av_canyon
                    !write(*,'(A,i6,a,2f6.2,a,2f6.2)') 'Road:',ro,' Width canyon (N/S): ',width_canyon(2),width_canyon(1),' Height canyon (N/S): ',height_canyon(1),height_canyon(2)
                    !inputdata_rl(canyonheight_north_rl_index,ro)=height_canyon(2)
                    !inputdata_rl(canyonheight_south_rl_index,ro)=height_canyon(1)
                    !inputdata_rl(canyonwidth_rl_index,ro)=abs(width_canyon(1))+abs(width_canyon(2))
                    !inputdata_rl(canyondist_north_rl_index,ro)=abs(width_canyon(2))
                    !inputdata_rl(canyondist_south_rl_index,ro)=abs(width_canyon(1))
                
    
                    !Calculate skyview
                    stepsize=cellsize
                    !n_grid_search_skyview=40
                    n_grid_search_skyview=int(sqrt(max_skyview_search_distance/stepsize-n_grid_search_canyon))+n_grid_search_canyon

                    do s=1,n_skyview
                        az_skyview(s,ro)=(s-1)*360./n_skyview
                        height_angle_max=0
        
                        do i=1,n_grid_search_skyview
                            !Set step sizes below 10 (i.e. 50 m) it is the same step as for the canyon.
                            !Above this it increases with the square to get the distant topography.
                            !Value of n_grid_search_skyview=40 and stepsize=5 is 4.5 km
                            if (i.le.n_grid_search_canyon) then
                                dist_step=i*stepsize
                            else                            
                                dist_step=stepsize*(n_grid_search_canyon+(i-n_grid_search_canyon)**2)
                            endif
        
                            !Set position of the step
                            x_point_step=x_point+sin(pi/180.*az_skyview(s,ro))*dist_step
                            y_point_step=y_point+cos(pi/180.*az_skyview(s,ro))*dist_step

                            !Determine the grid index it is in
                            i_point_step=int((x_point_step-x_array(1)+cellsize)/(x_array(ncols)-x_array(1)+cellsize)*ncols+1)
                            j_point_step=int((y_point_step-y_array(1)+cellsize)/(y_array(nrows)-y_array(1)+cellsize)*nrows+1)
                            
                            !write(*,*) i_point_step,j_point_step
           
                            if (i_point_step.gt.1.and.i_point_step.lt.ncols.and.j_point_step.gt.1.and.j_point_step.lt.nrows) then
            
                                !Calculate the height angle
                                height_angle=atan((array(i_point_step,j_point_step)-search_min)/abs(dist_step))
                                if (height_angle.gt.height_angle_max) then
                                    dis_skyview(s,ro)=dist_step
                                    height_skyview(s,ro)=max(array(i_point_step,j_point_step)-search_min,0.)
                                    height_angle_max=height_angle
                                endif
                                !write(*,'(i6,5f12.2,2i)') s,dist_step,x_point_step-x_point,y_point_step-y_point,height_angle,height_skyview(s,ro),i_point_step,j_point_step  
                                !write(*,*) array(i_point_step,j_point_step)
                            endif
                        
                        enddo

                        height_angle_max_skyview(s)=height_angle_max
                        

                        !zen_skyview(s,ro)=max(min(90.-180./pi*height_angle_max,90.),0.)
                    
                    enddo !skyview
    
                    if (first_valid_segment) then
                        height_angle_max_av_skyview=height_angle_max_skyview
                    else
                        height_angle_max_av_skyview=height_angle_max_av_skyview*(n_valid_segment-1.)/n_valid_segment+height_angle_max_skyview/n_valid_segment
                    endif
               
                    !write(*,'(a,i6,<n_skyview>f6.0)') 'AZIMUTH:  ',ro,(az_skyview(s,ro),s=1,n_skyview)          
                    !write(*,'(a,i6,<n_skyview>f6.1)') 'ZENITH:   ',ro,(zen_skyview(s,ro),s=1,n_skyview)          
                    !write(*,'(a,i6,<n_skyview>f6.0)') 'DISTANCE: ',ro,(dis_skyview(s,ro),s=1,n_skyview)          
                    !write(*,'(a,i6,<n_skyview>f6.0)') 'HEIGHT:   ',ro,(height_skyview(s,ro),s=1,n_skyview)     
                
                    !Set first valid segment to false since it has now been found in the domain
                    if (first_valid_segment) then
                        first_valid_segment=.false.
                    endif
                   
                else
                    height_angle_max_av_skyview=missing_data
                    height_angle_max_av_canyon=missing_data                 
                endif !if within domain
                
            enddo !segments
            enddo !sublink loop
                

            !Set the average skyview
            if (height_angle_max_av_skyview(1).ne.missing_data) then
                do s=1,n_skyview
                    zen_skyview(s,ro)=max(min(90.-180./pi*height_angle_max_skyview(s),90.),0.)
                enddo
            endif
            
            
            if (height_angle_max_av_canyon(north_index).ne.missing_data) then
                !Put the canyon width always at road_width+10 m.
                inputdata_rl(canyonwidth_rl_index,ro)=inputdata_rl(width_rl_index,ro)+kerb_width*2.
                !Set the heights according to the average height angle
                inputdata_rl(canyondist_north_rl_index,ro)=inputdata_rl(canyonwidth_rl_index,ro)/2.
                inputdata_rl(canyondist_south_rl_index,ro)=inputdata_rl(canyonwidth_rl_index,ro)/2.
                inputdata_rl(canyonheight_north_rl_index,ro)=tan(height_angle_max_av_canyon(north_index))*inputdata_rl(canyondist_north_rl_index,ro)
                inputdata_rl(canyonheight_south_rl_index,ro)=tan(height_angle_max_av_canyon(south_index))*inputdata_rl(canyondist_south_rl_index,ro)
                !write(*,'(A,i6,A,i10,A,i4,a,2f6.1,a,2f6.1,a,2f6.1,a,f6.2)') 'Road:',ro,' ID:',inputdata_int_rl(id_rl_index,ro), &
                !    ' n_seg:',n_segments,' Width canyon (N/S): ', &
                !    inputdata_rl(canyondist_north_rl_index,ro),inputdata_rl(canyondist_south_rl_index,ro) &
                !    ,' Height canyon (N/S): ',inputdata_rl(canyonheight_north_rl_index,ro),inputdata_rl(canyonheight_south_rl_index,ro) &
                !    ,' Angle canyon (N/S): ',height_angle_max_av_canyon(north_index)*180/pi,height_angle_max_av_canyon(south_index)*180/pi &
                !    ,' Road angle: ',inputdata_rl(angle_rl_index,ro)
                !write(*,*) ro,inputdata_rl(canyonheight_north_rl_index,ro),height_angle_max_av_canyon
              !Calculate road slope
                
                if (inputdata_rl(y1_rl_index,ro).ge.inputdata_rl(y2_rl_index,ro)) then
                    y_point_north=inputdata_rl(y1_rl_index,ro)
                    x_point_north=inputdata_rl(x1_rl_index,ro)
                    y_point_south=inputdata_rl(y2_rl_index,ro)
                    x_point_south=inputdata_rl(x2_rl_index,ro)
                else
                    y_point_north=inputdata_rl(y2_rl_index,ro)
                    x_point_north=inputdata_rl(x2_rl_index,ro)
                    y_point_south=inputdata_rl(y1_rl_index,ro)
                    x_point_south=inputdata_rl(x1_rl_index,ro)
                endif
                
                if (utm_zone.ne.terrain_utm_zone) then                   
                    call UTM2LL(utm_zone,y_point_north,x_point_north,lat_point,lon_point)
                    call LL2UTM(1,terrain_utm_zone,lat_point,lon_point,y_point_north,x_point_north)
                    call UTM2LL(utm_zone,y_point_south,x_point_south,lat_point,lon_point)
                    call LL2UTM(1,terrain_utm_zone,lat_point,lon_point,y_point_south,x_point_south)
                endif
                
                !Determine the grid index it is in
                i_point_north=int((x_point_north-x_array(1)+cellsize)/(x_array(ncols)-x_array(1)+cellsize)*ncols+1)
                j_point_north=int((y_point_north-y_array(1)+cellsize)/(y_array(nrows)-y_array(1)+cellsize)*nrows+1)
                i_point_south=int((x_point_south-x_array(1)+cellsize)/(x_array(ncols)-x_array(1)+cellsize)*ncols+1)
                j_point_south=int((y_point_south-y_array(1)+cellsize)/(y_array(nrows)-y_array(1)+cellsize)*nrows+1)
                
                !write(*,*) j_point_north,i_point_north,j_point_south,i_point_south
                if (i_point_north.gt.1.and.i_point_north.lt.ncols.and.j_point_north.gt.1.and.j_point_north.lt.nrows) then
                if (i_point_south.gt.1.and.i_point_south.lt.ncols.and.j_point_south.gt.1.and.j_point_south.lt.nrows) then
                    inputdata_rl(slope_rl_index,ro)=180./pi*atan((array(i_point_north,j_point_north)-array(i_point_south,j_point_south))/inputdata_rl(length_rl_index,ro))
                    !write(*,*) inputdata_rl(slope_rl_index,ro)
                endif
                endif
                
                !Maximum slope is 15 degrees
                if (inputdata_rl(slope_rl_index,ro).gt.max_slope) inputdata_rl(slope_rl_index,ro)=max_slope
                if (inputdata_rl(slope_rl_index,ro).lt.-max_slope) inputdata_rl(slope_rl_index,ro)=-max_slope
                
            endif!if not missing data
        enddo !Road loop
    
        if (allocated(array)) deallocate(array)
        if (allocated(x_array)) deallocate(x_array)
        if (allocated(y_array)) deallocate(y_array)
  
10  enddo !File loop
    write(unit_logfile,'(A,f12.1,i12)') ' Canyon search distance (m) and index: ',max_canyon_search_distance,n_grid_search_canyon
    write(unit_logfile,'(A,f12.1,i12)') ' Skyview search distance (m) and index: ',max_skyview_search_distance,n_grid_search_skyview

    write(unit_logfile,'(A,2f6.1,a,2f6.1,A,f8.1,A,f8.1,a,i6)') ' Max width canyon (N/S): ' &
        ,maxval(inputdata_rl(canyondist_north_rl_index,:)),maxval(inputdata_rl(canyondist_south_rl_index,:)), &
        ' Max height canyon (N/S): ',maxval(inputdata_rl(canyonheight_north_rl_index,:)),maxval(inputdata_rl(canyonheight_south_rl_index,:)) &
        ,' Min skyview zenith: ',minval(zen_skyview(:,:)),' Max skyview dist: ',maxval(dis_skyview(:,:)), &
        ' Links found: ',count_found
   
    if (allocated(filename_ascii)) deallocate (filename_ascii)
    
    !Instead of saving the north and south distance then an 'equivalent' canyon height could be made using the smallest or average distance.
    !Something to think about. Wouldn't need to change the model then
    
    !Read in forest data and reset canyon appropriately
    !Sets canyon to 0 
    !Does not include the sublinks currently, so not really correct
    if (n_forest_files.gt.0) then

    allocate (filename_ascii(n_forest_files))
    do f=1,n_forest_files
        filename_ascii(f)=trim(pathname_forest)//trim(filename_forest_data(f))
        !write(*,*) trim(filename_ascii(f))
    enddo
    
    do ro=1,n_roadlinks
            inputdata_rl(canyonwidth_rl_index,ro)=inputdata_rl(width_rl_index,ro)+kerb_width*2.
            inputdata_rl(canyondist_north_rl_index,ro)=inputdata_rl(canyonwidth_rl_index,ro)/2.
            inputdata_rl(canyondist_south_rl_index,ro)=inputdata_rl(canyonwidth_rl_index,ro)/2.
            inputdata_rl(canyonheight_north_rl_index,ro)=1.
            inputdata_rl(canyonheight_south_rl_index,ro)=1.
    enddo
    

    do f=1,n_forest_files

        inquire(file=trim(filename_ascii(f)),exist=exists)
        if (.not.exists) then
            write(unit_logfile,'(A,A)') ' WARNING: forest file does not exist: ', trim(filename_ascii(f))
            goto 20
        endif
        
        if (index(filename_ascii(f),'.asc').gt.0) then
            write(unit_logfile,'(A)') ' Reading forest asc file'
            call read_esri_ascii_terrain_file(filename_ascii(f),ncols,nrows,cellsize)
        elseif (index(filename_ascii(f),'.nc').gt.0) then
            write(unit_logfile,'(A)') ' Reading forest netcdf file '
            call read_netcdf4_terrain_file(filename_ascii(f),ncols,nrows,cellsize)
        else
            write(unit_logfile,'(A,A)') ' WARNING: Forest file type not netcdf or asc. Cannot read ', trim(filename_ascii(f))
            goto 20
        endif     
    
        !Loop through road links
        do ro=1,n_roadlinks

            n_segments=int(inputdata_rl(length_rl_index,ro)/length_segment_init)+1
            !n_segments=max(1,n_segments)
            length_segment=inputdata_rl(length_rl_index,ro)/n_segments
            !write(*,*) ro,n_segments,length_segment
            
            first_valid_segment=.true.
            
            is_forest=0
            do seg=0,n_segments-1
            
                x_point=(inputdata_rl(x1_rl_index,ro)*(n_segments-seg-.5)+inputdata_rl(x2_rl_index,ro)*(seg+.5))/n_segments
                y_point=(inputdata_rl(y1_rl_index,ro)*(n_segments-seg-.5)+inputdata_rl(y2_rl_index,ro)*(seg+.5))/n_segments
     
                !Here we assume that the road link data is in UTM coordinates and the terrain data is in UTM33
                !If utm_zone <> terrain_utm_zone for the road links then convert the road data points to utm 33
                if (utm_zone.ne.terrain_utm_zone) then                   
                    call UTM2LL(utm_zone,y_point,x_point,lat_point,lon_point)
                    call LL2UTM(1,terrain_utm_zone,lat_point,lon_point,y_point,x_point)
                endif

                !Determine the grid index it is in
                i_point=int((x_point-x_array(1)+cellsize)/(x_array(ncols)-x_array(1)+cellsize)*ncols+1)
                j_point=int((y_point-y_array(1)+cellsize)/(y_array(nrows)-y_array(1)+cellsize)*nrows+1)
                !write(*,*) 'Grid index :',i_point,j_point
    
                !Mark it as out of bounds    
                if (i_point.lt.0.or.i_point.gt.ncols) i_point=0
                if (j_point.lt.0.or.j_point.gt.nrows) j_point=0
    
                !See if there is forest for that segment
                if (i_point.ne.0.and.j_point.ne.0) then
                    if (array(i_point,j_point).gt.0) then
                        is_forest=is_forest+1
                        !write(*,*) 'Forest found at: ',i_point,j_point
                    endif
                endif
            enddo
                        
            if (is_forest.gt.0) then
                inputdata_rl(canyonwidth_rl_index,ro)=inputdata_rl(width_rl_index,ro)+forest_kerb_width*2.
                inputdata_rl(canyondist_north_rl_index,ro)=inputdata_rl(canyonwidth_rl_index,ro)/2.
                inputdata_rl(canyondist_south_rl_index,ro)=inputdata_rl(canyonwidth_rl_index,ro)/2.
                inputdata_rl(canyonheight_north_rl_index,ro)=forest_height*real(is_forest)/real(n_segments)  !If only a fraction of segments is forest then decrease height
                inputdata_rl(canyonheight_south_rl_index,ro)=forest_height*real(is_forest)/real(n_segments)
                !write(*,*) 'Forest found at: ',is_forest,i_point,j_point,inputdata_rl(canyonheight_north_rl_index,ro),inputdata_rl(canyonheight_south_rl_index,ro)
            endif
            
        enddo
20  enddo
    
        
    endif
    
    
    
    !Save the data to be read later
    temp_name=trim(pathname_terrain)//trim(filename_skyview)
    write(unit_logfile,'(a)') ' Saving skyview for NORTRIP to: '//trim(temp_name)
    open(unit_out,file=temp_name,status='replace')
    
        
        write(unit_out,'(A)')   'Street canyon and skyview zenith angles data for NORTRIP in '//trim(city_str(1))//' (not tab delimitted)'
        write(unit_out,'(A,i6)') 'n_roads: ',n_roadlinks
        write(unit_out,'(A,i6)') 'n_skyview_angles: ',n_skyview
        write(unit_out,'(A8,7A14,<n_skyview>f8.1)') 'Road','RoadLinkID','Elevation','Slope','Can_dis_N','Can_dis_S','Can_height_N','Can_height_S',(az_skyview(s,1),s=1,n_skyview)

        do ro=1,n_roadlinks
            write(unit_out,'(i8,i14,6f14.1,<n_skyview>f8.1)') &
                ,ro &
                ,inputdata_int_rl(id_rl_index,ro) &
                ,inputdata_rl(elevation_rl_index,ro),inputdata_rl(slope_rl_index,ro) &
                ,inputdata_rl(canyondist_north_rl_index,ro),inputdata_rl(canyondist_south_rl_index,ro) &
                ,inputdata_rl(canyonheight_north_rl_index,ro),inputdata_rl(canyonheight_south_rl_index,ro) &
                ,(zen_skyview(s,ro),s=1,n_skyview)                
        enddo
        
    close (unit_out,IOMSG=message)
    !write(*,*) 'MESSAGE: ',trim(message)
    
    if (allocated(az_skyview)) deallocate(az_skyview)
    if (allocated(zen_skyview)) deallocate(zen_skyview)
    if (allocated(dis_skyview)) deallocate(dis_skyview)
    if (allocated(height_skyview)) deallocate(height_skyview)
    if (allocated(filename_ascii)) deallocate(filename_ascii)

    end subroutine process_terrain_data_64bit
    
    
    