!read_netcdf_terrain_file.f90
    
    subroutine read_netcdf3_terrain_file(filename_nc_sub,ncols_sub,nrows_sub,cellsize_sub)
    
    use NORTRIP_multiroad_index_definitions

    implicit none
    
    include 'netcdf.inc'

    integer status_nc      !Error message
    integer id_nc
    integer dim_id_nc(2)
    character(256) dimname_temp
    integer var_id_nc
    
    character(*) filename_nc_sub
    character(256) temp_str
    integer i,j,ii,jj
    integer ncols_sub,nrows_sub
    real cellsize_sub

    !Open the netcdf file for reading
    write(unit_logfile,'(2A)') ' Opening netcdf terrain file: ',trim(filename_nc_sub)
    status_nc = NF_OPEN (filename_nc_sub, NF_NOWRITE, id_nc)
    if (status_nc .NE. NF_NOERR) write(unit_logfile,'(A,I)') 'ERROR opening netcdf file: ',status_nc

    !Find out the x,y and time dimmensions of the file by looking at pressure variable
    status_nc = NF_INQ_DIMID (id_nc,dim_name_nc(x_index),dim_id_nc(x_index))
    status_nc = NF_INQ_DIM (id_nc,dim_id_nc(x_index),dimname_temp,dim_length_nc(x_index))
    status_nc = NF_INQ_DIMID (id_nc,dim_name_nc(y_index),dim_id_nc(y_index))
    status_nc = NF_INQ_DIM (id_nc,dim_id_nc(y_index),dimname_temp,dim_length_nc(y_index))
    
    ncols_sub=dim_length_nc(x_index)
    nrows_sub=dim_length_nc(y_index)
    dim_start_nc=1
 
    if (.not.allocated(array)) allocate(array(ncols_sub,nrows_sub))
    if (.not.allocated(x_array)) allocate(x_array(ncols_sub))
    if (.not.allocated(y_array)) allocate(y_array(nrows_sub))
    allocate (var1d_nc(num_dims_nc,max(ncols_sub,nrows_sub))) !x and y and time maximum dimmensions
   
    !Read the x, y and time values
    do i=1,num_dims_terrain_nc
        status_nc = NF_INQ_VARID (id_nc, trim(dim_name_terrain_nc(i)), var_id_nc)
        status_nc = NF_GET_VARA_REAL (id_nc, var_id_nc, dim_start_nc(i), dim_length_nc(i), var1d_nc(i,:))
        
        write(unit_logfile,'(3A,2f12.2)') ' ',trim(dim_name_terrain_nc(i)),' (min, max in km): ' &
            ,minval(var1d_nc(i,1:dim_length_nc(i))/1000.),maxval(var1d_nc(i,1:dim_length_nc(i))/1000.)     
    enddo

    x_array=var1d_nc(x_index,1:nrows_sub)
    y_array=var1d_nc(y_index,1:ncols_sub)
   
    !Assumes x and y are the same
    cellsize_sub=x_array(2)-x_array(1)
    
    i=terrain_index
    status_nc = NF_INQ_VARID (id_nc, trim(var_name_terrain_nc(i)), var_id_nc)
    status_nc = NF_GET_VARA_REAL (id_nc, var_id_nc, dim_start_nc(1:2), dim_length_nc(1:2), array(:,:))
    
    !write(unit_logfile,'(3A,2f12.4)') ' ',trim(var_name_terrain_nc(i)),' (min, max): ' &
    !            ,minval(array(:,:)) &
    !            ,maxval(array(:,:)) 

    
    write(unit_logfile,'(2a10,1a12)')'ncols','nrows','cellsize'
    write(unit_logfile,'(2i10,1f12.1)')ncols_sub,nrows_sub,cellsize_sub
    write(unit_logfile,'(a,f6.1,a,f6.1)') ' Min array val: ',minval(array),' Max array val: ',maxval(array)

    deallocate (var1d_nc)
    
    end subroutine read_netcdf3_terrain_file