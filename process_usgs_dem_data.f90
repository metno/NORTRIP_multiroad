!process_usgs_dem_data.f90
    
    subroutine process_usgs_dem_data
    
    implicit none
    
    character(256) filename_dem
    integer :: unit_in=20
    character(1024) temp_str
    character(6*3000) templong_str
    integer i,j,ii,jj
    double precision val_temp
    integer*4 i_temp
    real real_temp
    integer utm_zone
    integer*2, allocatable :: array(:,:)
    integer n_col,n_row
    
    filename_dem='C:\BEDRE_BYLUFT\Data\DEM data\Terrengdata_6602_UTM33_50m_DEM\6602_50m_33.dem'
    filename_dem='C:\BEDRE_BYLUFT\Data\DEM data\Terrengdata_6605_2_UTM32_10m_DEM\6605_2_10m_z32.dem'
    
    open(unit_in,file=filename_dem,access='sequential',form='formatted',status='old',readonly)
    
    rewind(unit_in)
    !Read header
    read(unit_in,'(A162)',advance='NO') temp_str
    write(*,'(A)') trim(temp_str)
    !read(unit_in,'(i)',advance='NO') i_temp
    !write(*,*) i_temp
    !read(unit_in,'(15D)',advance='NO') val_temp
    !write(*,*) val_temp

    
    read(unit_in,'(A6)',advance='NO') temp_str
    write(*,'(A)') trim(temp_str)
    READ (temp_str,'(i)') utm_zone
    write(*,*) 'UTM zone: ',utm_zone
    read(unit_in,'(A361)',advance='NO') temp_str
    write(*,'(A)') trim(temp_str)
    read(unit_in,'(A6)',advance='NO') temp_str
    write(*,'(A)') trim(temp_str)
    read(unit_in,'(A6)',advance='NO') temp_str
    write(*,'(A)') trim(temp_str)
    read(unit_in,'(A6)',advance='NO') temp_str  
    write(*,'(A)') trim(temp_str)
    read(unit_in,'(A191)',advance='NO') temp_str  !Positions
    write(*,'(A)') trim(temp_str)
    read(unit_in,'(A48)',advance='NO') temp_str  !Min and max
    write(*,'(A)') trim(temp_str)
    read(unit_in,'(A30)',advance='NO') temp_str  !Don't know
    write(*,'(A)') trim(temp_str)
    read(unit_in,'(A12)',advance='NO') temp_str  !x grid resolution
    write(*,'(A)') trim(temp_str)
    read(unit_in,'(A12)',advance='NO') temp_str  !y grid resolution
    write(*,'(A)') trim(temp_str)
    read(unit_in,'(A18)',advance='NO') temp_str  !Don't know
    write(*,'(A)') trim(temp_str)
    read(unit_in,'(A6)',advance='NO') temp_str  !Number of columns
    write(*,'(A)') trim(temp_str)
    READ (temp_str,'(i)') n_col
    
    read(unit_in,'(A160)',advance='NO') temp_str  !Don't know
    write(*,'(A)') trim(temp_str)
    
    !read(unit_in,'(A6)',advance='NO') temp_str  !Numbers
    !write(*,'(A)') trim(temp_str)
    !stop
    
    allocate(array(n_col,n_col))
    
    read(unit_in,'(A24)',advance='NO') temp_str  !Numbers
    write(*,'(A)') trim(temp_str)
    read(unit_in,'(A120)',advance='NO') temp_str  !Numbers
    write(*,'(A)') trim(temp_str)
    !read(unit_in,'(A<2001*6>)',advance='NO') templong_str  !Numbers
    !write(*,'(A)') trim(templong_str)
    !read(unit_in,'(A120)',advance='NO') temp_str  !Numbers
    !write(*,'(A)') trim(temp_str)

    do j=1,1
    do i=1,1
    
    !read(unit_in,'(A6)',advance='NO') temp_str  !Numbers
    !write(*,'(i,A,A)') i,' : ',trim(temp_str)
    !read(unit_in,'(i)',advance='NO') i_temp  !Numbers
    !write(*,'(i,i)') i,i_temp
    read(unit_in,'(<n_col>i)',advance='NO') (array(ii,j),ii=1,n_col)  !Numbers
    write(*,'(<n_col>i6)') (array(ii,j),ii=1,2001)
        !read(templong_str,*) (array(ii,j),ii=1,2001)
        !write(*,'(<2001>i6)') (array(ii,j),ii=1,2001)
    enddo
    enddo
    
    
    
    close(unit_in)
    
    end subroutine process_usgs_dem_data