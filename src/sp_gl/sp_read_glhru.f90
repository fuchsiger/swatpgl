!! Timo Subroutine
!! Read in glacier HRUs parameters
    subroutine sp_read_glhru    
    use hru_module
    use input_file_module
    use maximum_data_module    
    integer:: IU, ierr, cnt, idum
    real:: tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10, tmp11, tmp12, tmp13, tmp14, tmp15

    !! a) Read in HRU Glacier Parameters
    open(newunit=IU1, file='glacier_hrus.gl',status='old',action='read') ! Variante 1
    read(iu1,'(A)') glpars
    ierr=0
    cnt=0
    do while (ierr==0) ! Loop when Subbasin Assignment is not done
        read(iu1, *, iostat=ierr) idum, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10, tmp11 
    cnt=cnt+1
    end do
    !do while (ierr==0) ! Loop when Subbasin Assignment is not done
    !    read(iu1, *, iostat=ierr) idum, tmp1, tmp2, tmp3, tmp4, tmp5 
    !cnt=cnt+1
    !end do
    cnt = cnt - 1
    !allocate(glhruids(cnt),glmlttmp(cnt),glmfmx(cnt),glmfmn(cnt),f_frz(cnt),f_accu(cnt))
    !allocate(glhruids(cnt),hrus_sub_assign(cnt),glmtmp(cnt),glmfmx(cnt),glmfmn(cnt),f_frz(cnt),f_accu(cnt),tfac_s(cnt), &
    !       tfac_i(cnt),rfac_s(cnt),rfac_i(cnt),srfac_s(cnt),srfac_i(cnt),pr_fac(cnt),gl_lag(cnt),f_exp(cnt))
    allocate(glhruids(cnt),hrus_sub_assign(cnt),glmtmp(cnt),glmfmx(cnt),glmfmn(cnt),f_frz(cnt),f_accu(cnt), &
            tfac_i(cnt),rfac_i(cnt),srfac_i(cnt),gl_lag(cnt))
    hrus_sub_assign = 0
    glhruids = 0
    glmtmp = 0
    glmfmx = 0
    glmfmn = 0
    f_frz = 0
    f_accu = 0
    tfac_s = 0
    tfac_i = 0
    rfac_s = 0
    rfac_i = 0
    srfac_s = 0
    srfac_i = 0
    pr_fac = 0
    gl_lag = 0
    f_exp = 0
    rewind(iu1)
    read(iu1,'(A)') titldum
    do i=1, cnt       
        !read(iu1, *, iostat=ierr) glhruids(i),hrus_sub_assign(i),glmtmp(i),glmfmx(i),glmfmn(i),f_frz(i),f_accu(i),tfac_s(cnt), &
        !                 tfac_i(i),rfac_s(i),rfac_i(i),srfac_s(i),srfac_i(i),pr_fac(i),gl_lag(i),f_exp(i)
        read(iu1, *, iostat=ierr) glhruids(i),hrus_sub_assign(i),glmtmp(i),glmfmx(i),glmfmn(i),f_frz(i),f_accu(i), &
                         tfac_i(i),rfac_i(i),srfac_i(i),gl_lag(i)
    end do
    ! Read in Glacier Parameters to DB
    deallocate(gldb)
    allocate(gldb(cnt))
    do i = 1,cnt ! Loop over HRU (all lines in swat_gles file)
        gldb(i)%glhruids = glhruids(i)
        gldb(i)%glmtmp = glmtmp(i)
        gldb(i)%glmfmx = glmfmx(i)
        gldb(i)%glmfmn = glmfmn(i)
        gldb(i)%f_frz = f_frz(i)
        gldb(i)%f_accu = f_accu(i)        
        !gldb(i)%tfac_s = tfac_s(i)
        gldb(i)%tfac_i = tfac_i(i)
        !gldb(i)%rfac_s = rfac_s(i)
        gldb(i)%rfac_i = rfac_i(i)
        !gldb(i)%rfac_s = srfac_s(i)
        gldb(i)%rfac_i = srfac_i(i)
        !gldb(i)%pr_fac = pr_fac(i)
        gldb(i)%gl_lag = gl_lag(i)
        !gldb(i)%f_exp = f_exp(i)
    end do
    return
    end