!read_esri_ascii_file.f90
    
    subroutine read_esri_ascii_terrain_file(filename_ascii_sub,ncols_sub,nrows_sub,cellsize_sub)
    
    use NORTRIP_multiroad_index_definitions

    implicit none
    character(*) filename_ascii_sub
    character(256) temp_str
    integer i,j,ii,jj
    integer ncols_sub,nrows_sub
    real cellsize_sub
    real xllcorner
    real yllcorner
    real NODATA_value
    integer :: unit_in=20

        if (allocated(array)) deallocate(array)
        if (allocated(x_array)) deallocate(x_array)
        if (allocated(y_array)) deallocate(y_array)

    write(unit_logfile,'(a)') ' Opening DEM data file: '//trim(filename_ascii_sub)
    open(unit_in,file=filename_ascii_sub,access='sequential',form='formatted',status='old',readonly)
    
        rewind(unit_in)

        read(unit_in,*)temp_str,ncols_sub
        !write(*,*)trim(temp_str),ncols
        read(unit_in,*)temp_str,nrows_sub
        !write(*,*)trim(temp_str),nrows
        read(unit_in,*)temp_str,xllcorner
        !write(*,*)trim(temp_str),xllcorner
        read(unit_in,*)temp_str,yllcorner
        !write(*,*)trim(temp_str),yllcorner
        read(unit_in,*)temp_str,cellsize_sub
        !write(*,*)trim(temp_str),cellsize
        read(unit_in,*)temp_str,NODATA_value
        !write(*,*)trim(temp_str),NODATA_value
        write(unit_logfile,'(2a10,4a12)')'ncols','nrows','xllcorner','yllcorner','cellsize','NODATA_val'
        write(unit_logfile,'(2i10,4f12.1)')ncols_sub,nrows_sub,xllcorner,yllcorner,cellsize_sub,NODATA_value
    
        !Reduce for testing
        !nrows=1000
    
        if (.not.allocated(array)) allocate(array(ncols_sub,nrows_sub))
        if (.not.allocated(x_array)) allocate(x_array(ncols_sub))
        if (.not.allocated(y_array)) allocate(y_array(nrows_sub))
    
        read(unit_in,*) ((array(ii,jj),ii=1,ncols_sub),jj=nrows_sub,1,-1)
    
        close(unit_in)

        j=1
        !write(*,'(<ncols>f10.1)') (array(ii,j),ii=1,ncols)
        !write(*,*)
        !j=nrows
        !write(*,'(<ncols>f10.1)') (array(ii,j),ii=1,ncols)
        write(unit_logfile,'(a,f6.1,a,f6.1)') ' Min array val: ',minval(array),' Max array val: ',maxval(array)

        !Set position arrays
        do i=1,ncols_sub
            x_array(i)=xllcorner+cellsize_sub/2.+(i-1)*cellsize_sub
        enddo
        do j=1,nrows_sub
            y_array(j)=yllcorner+cellsize_sub/2.+(j-1)*cellsize_sub
        enddo

    end subroutine read_esri_ascii_terrain_file
