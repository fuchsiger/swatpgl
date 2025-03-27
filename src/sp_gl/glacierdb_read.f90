      subroutine glacierdb_read
      
      use input_file_module
      use maximum_data_module
      use hru_module
      
      implicit none

      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: msno                 !           |
      integer :: igl                  !none       |counter
      
      msno = 0
      eof = 0
      imax = 0
      
      
      !! read snow database data from snow.sno
      inquire (file=in_hyd%glacier, exist=i_exist)
      if (.not. i_exist .or. in_hyd%glacier == "null") then
        allocate (gldb(0:0))
      else 
      do 
        open (107,file=in_hyd%glacier)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof == 0)
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do
          
        rewind (107)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        
       allocate (gldb(0:imax))
    
        do igl = 1, imax
          read (107,*,iostat=eof) gldb(igl)         
          if (eof < 0) exit
        end do

      exit
      enddo
      
      endif
      close (107)
      
     db_mx%gl = imax
      
      return
      end subroutine glacierdb_read