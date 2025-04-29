!!! Timo Subroutine
!!! Read in glacier ES initialization
    subroutine sp_read_gles    
    use basin_module
    use hru_module
    use landuse_data_module
    use plant_data_module
    use topography_data_module
    use input_file_module
    use maximum_data_module 
    use aquifer_module
    use hydrology_data_module
    use hydrograph_module, only : ob, sp_ob
    integer:: IU, ierr, cnt, idum, k, es_cnt,cnt2
    integer:: sb_cnt, hru_cnt, counter, ios
    character (len=4) :: cropname, count2
    logical, dimension(:), allocatable :: mask, mask2
    integer, dimension(:), allocatable :: mask3
    integer :: vv
    integer :: subt
    integer :: idx
    real :: vol1, vol2, diff, dst, count_nonzero
    real :: vol_check, hru_es_frac, subtest
    character(len=14) :: hedgl(6)
    
    !! Initialize Variables
    sb_cnt = 0.
    es_cnt = 0.
    hru_cnt = 0.
    esdist = 0.
    vol_check = 0.
    vol1 = 0.
    vol2 = 0.
    !! e) Prepare Annual Output Summary File for Writing SWAT+GL Outputs
    open(unit=89, file="gl_mb_aa.txt")
    hedgl = (/" Sub","  ES","  yr","   dM_m3","   GWE_mm"," Agl_km2"/)
    write (89,8999) hedgl
    8999 format (3a4,2a14,1a10) 
     
    !! Read in swat_gles.gl File
    !! a) Glacier ES Infos per Subbasin
    open(newunit=IU, file='swat_gles.gl',status='old',action='read') ! Variante 1
    read(iu,'(A)') titldum
    ierr=0
    cnt=0
    do while (ierr==0)
        read(iu, *, iostat=ierr) idum, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6 
        cnt=cnt+1
    end do
    cnt = cnt - 1
    allocate(glsubs(cnt),esids(cnt),es_glw(cnt),&
           es_gla(cnt),es_glv(cnt),es_glfr(cnt),&
           es_elup(cnt))
    es_elup=0.
    es_gla=0.
    es_glv=0.
    es_glfr=0.
    esids=0.
    es_glw=0.
    glsubs=0.   
    
    rewind(iu)
    read(iu,'(A)') titldum
    do i=1, cnt       
            read(iu, *, iostat=ierr) glsubs(i),esids(i),es_glw(i),es_gla(i),es_glv(i),es_glfr(i),es_elup(i)
    end do
    !! Compute number of glaciated subbasins
    allocate(mask(cnt))
    allocate(mask3(cnt))
    mask = .true.
    mask3 = 0
    cnt2=1
    do i = 1, cnt-1
        if (mask(i)) then
            mask(i+1:cnt) = mask(i+1:cnt) .and. (glsubs(i+1:cnt) /= glsubs(i))
            if (i>1) then
                cnt2 = cnt2+1
            endif
            mask3(i) = cnt2
        else
            mask3(i) = cnt2
            if (i==cnt-1) then
                mask3(i+1) = cnt2
            endif 
        end if
    end do
    sb_cnt = count(mask)
    !sb_cnt = 92
    ! Allocate Subbasin Variables
    allocate(subs_gl(sb_cnt),gla_sub(sb_cnt),glw_sub(sb_cnt),glww_sub(sb_cnt),glfr_sub_real(sb_cnt), & 
            glv_sub(sb_cnt),tot_glhru_sub(sb_cnt),gla_sub_real(sb_cnt),glww_sub_real(sb_cnt))
    subs_gl=0.
    gla_sub=0.
    glw_sub=0.
    glww_sub=0.
    glv_sub=0.
    gla_sub_real=0.
    glv_sub_real=0.
    glw_sub_real=0.
    glww_sub_real=0.
    glfr_sub_real=0.

    !! b) Identify Glacier Subs 
    !! Compute Subbasin Storages
    do i=1, cnt       
    ! subs_gl(glsubs(i))=1
    ! Calculate Subbasin Number of ES, Subbasin Glacier Area (km²), Subbasin Total GWE
    if (es_gla(i)>0) then
        !subs_gl(glsubs(i))=glsubs(i) ! Glaciated Subbasin IDs
        !gla_sub(glsubs(i))=gla_sub(glsubs(i))+es_gla(i) ! Glacier Area per Sub
        !glv_sub(glsubs(i))=glv_sub(glsubs(i))+es_glv(i) ! Glacier Volume per Sub (Ice)
        !glw_sub(glsubs(i))=glw_sub(glsubs(i))+es_glw(i)! Glacier GWE per Sub
        !glww_sub(glsubs(i))=glww_sub(glsubs(i))+(es_glw(i)*es_glfr(i)) ! Glacier Weighted GWE per Sub
        subs_gl(mask3(i))=glsubs(i) ! Glaciated Subbasin IDs
        gla_sub(mask3(i))=gla_sub(mask3(i))+es_gla(i) ! Glacier Area per Sub
        glv_sub(mask3(i))=glv_sub(mask3(i))+es_glv(i) ! Glacier Volume per Sub (Ice)
        glw_sub(mask3(i))=glw_sub(mask3(i))+es_glw(i)! Glacier GWE per Sub
        glww_sub(mask3(i))=glww_sub(mask3(i))+(es_glw(i)*es_glfr(i)) ! Glacier Weighted GWE per Sub
    end if
    end do    
    
    !! c) Assign ES Data to ES Object/DB 
    allocate(es_obj(sb_cnt),es_obj_real(sb_cnt))
    es_cnt = MAXVAL(esids) ! Number of ES per Subbasin
    allocate(e_norm(es_cnt),dh(es_cnt))
    do i = 1,cnt ! Loop over all ES (all lines in swat_gles file)
        if (i>1 .AND. esdist==0) then
            if (es_elup(i-1)>0) esdist=(es_elup(i)-es_elup(i-1))/2
        end if
        !! Allocate ES_Obj 
        if (esids(i)==1) then
            allocate(es_obj(mask3(i))%es_id(es_cnt))
            allocate(es_obj(mask3(i))%es_gla(es_cnt))
            allocate(es_obj(mask3(i))%es_gla_scale(es_cnt))
            allocate(es_obj(mask3(i))%es_gla_init(es_cnt))
            allocate(es_obj(mask3(i))%es_glv(es_cnt))
            allocate(es_obj(mask3(i))%es_glw(es_cnt))
            allocate(es_obj(mask3(i))%es_glw_init(es_cnt))
            allocate(es_obj(mask3(i))%es_glfr(es_cnt))
            allocate(es_obj(mask3(i))%es_glww(es_cnt))
            allocate(es_obj(mask3(i))%es_elup(es_cnt))
            
            allocate(es_obj_real(mask3(i))%es_id(es_cnt))
            allocate(es_obj_real(mask3(i))%es_gla(es_cnt))
            allocate(es_obj_real(mask3(i))%es_gla_scale(es_cnt))
            allocate(es_obj_real(mask3(i))%es_gla_init(es_cnt))
            allocate(es_obj_real(mask3(i))%es_glv(es_cnt))
            allocate(es_obj_real(mask3(i))%es_glw(es_cnt))
            allocate(es_obj_real(mask3(i))%es_glw_init(es_cnt))
            allocate(es_obj_real(mask3(i))%es_glfr(es_cnt))
            allocate(es_obj_real(mask3(i))%es_glww(es_cnt))
            allocate(es_obj_real(mask3(i))%es_elup(es_cnt))
        end if
        
        ! Initialize with Read in Data
        es_obj(mask3(i))%sub_id = glsubs(i) 
        es_obj(mask3(i))%es_mx_elev = 0.
        es_obj(mask3(i))%es_mn_elev = 0.
        es_obj(mask3(i))%es_id(esids(i)) = esids(i)
        es_obj(mask3(i))%es_gla(esids(i)) = es_gla(i)
        es_obj(mask3(i))%es_gla_scale(esids(i)) = es_gla(i)
        es_obj(mask3(i))%es_gla_init(esids(i)) = es_gla(i)
        es_obj(mask3(i))%es_glv(esids(i)) = es_glv(i)
        es_obj(mask3(i))%es_glw(esids(i)) = es_glw(i)
        es_obj(mask3(i))%es_glw_init(esids(i)) = es_glw(i)
        es_obj(mask3(i))%es_glfr(esids(i)) = es_glfr(i)
        es_obj(mask3(i))%es_glww(esids(i)) = es_glw(i)*es_glfr(i)
        es_obj(mask3(i))%es_elup(esids(i)) = es_elup(i)
        
        ! Initialize with 0 Values
        es_obj_real(mask3(i))%sub_id = glsubs(i)
        es_obj_real(mask3(i))%es_mx_elev = 0.
        es_obj_real(mask3(i))%es_mn_elev = 0. 
        es_obj_real(mask3(i))%es_id(esids(i)) = esids(i)
        es_obj_real(mask3(i))%es_gla(esids(i)) = 0.
        es_obj_real(mask3(i))%es_gla_scale(esids(i)) = 0.
        es_obj_real(mask3(i))%es_gla_init(esids(i)) = 0.
        es_obj_real(mask3(i))%es_glv(esids(i)) = 0.
        es_obj_real(mask3(i))%es_glw(esids(i)) = 0.
        es_obj_real(mask3(i))%es_glw_init(esids(i)) = 0.
        es_obj_real(mask3(i))%es_glfr(esids(i)) = 0.
        es_obj_real(mask3(i))%es_glww(esids(i)) = 0.
        es_obj_real(mask3(i))%es_elup(esids(i)) = es_elup(i)
    end do
    ! Max & Min Elev of Glacier ES per Subbasin
    do i = 1,sb_cnt ! Loop Over Subbasin
        es_obj_real(i)%es_mx_elev = maxval(es_obj(i)%es_elup)
        if (count(es_obj(i)%es_elup > 0) > 0) then
            es_obj_real(i)%es_mn_elev = minval(pack(es_obj(i)%es_elup, es_obj(i)%es_elup > 0))
        else
            es_obj_real(i)%es_mn_elev = -9999.0  ! Assign missing value if no valid data exists
        end if
        es_obj(i)%es_mx_elev = es_obj_real(i)%es_mx_elev
        es_obj(i)%es_mn_elev = es_obj_real(i)%es_mn_elev
    end do
    
    ! Glacier HRUs per Subbasin  
    hru_cnt = size(hrus_sub_assign) ! Total Number of GLacier HRUs
    allocate(mask2(hru_cnt))
    do i = 1,sb_cnt ! Loop Over Subbasin
        vv = 0        
        mask2 = (hrus_sub_assign==subs_gl(i)) ! Glacier HRU IDs (mask)
        vv = count(mask2) !! Number of Glacier HRUs in Subbasin
        tot_glhru_sub(i) = vv 
    end do
    
    !! d) Recalculate ES & Subbasin Glacier Areas based on HRUs 
    !! Count Glacier HRUs per Subbasin

    do j = 1,hru_cnt ! Loop over All Glacier HRUs
            subt = hrus_sub_assign(j)
            idx = 0
            do i = 1, size(subs_gl)
                if (subs_gl(i) == subt) then
                    !idx = i
                    idx = i
                    exit
                end if
            end do
            if (idx == 0) cycle  ! Skip if subbasin not in glacier list

            cropname=hru(glhruids(j))%LAND_USE_MGT_C(1:4) ! Crop Name to get ES #
            if (cropname(len_trim(cropname):len_trim(cropname)) < '0' .or. cropname(len_trim(cropname):len_trim(cropname)) > '9') then
                cropname = cropname(3:len_trim(cropname)-1)   ! Exclude the last character
            else
                cropname = cropname(3:len_trim(cropname))     ! Include all characters starting from the 3rd
            end if
            read(cropname,"(I)") counter ! Get Index of ES (e.g. ES6 = 6)
            ! Update Subbasin & ES with actual/real values (HRU-derived)
            gla_sub_real(idx) = gla_sub_real(idx) + ob(glhruids(j))%area_ha/100 ! Subbasin Glacier Area Actual (km²)
            !glfr_sub_real(i) = glfr_sub_real(i)/ob(glhruids(j))%area_ha/100    ! Calculate Glacierized Subbasin Fraction wrt Subbasin Area 
            es_obj_real(idx)%es_gla(counter) = es_obj_real(idx)%es_gla(counter) + ob(glhruids(j))%area_ha/100 ! ES Glacier Area Actual (km²)
            es_obj_real(idx)%es_gla_scale(counter) = es_obj_real(idx)%es_gla(counter)
            es_obj_real(idx)%es_gla_init(counter) = es_obj_real(idx)%es_gla(counter)
            fac_area = es_obj(idx)%es_gla(counter)/es_obj_real(idx)%es_gla(counter) ! As the original WGWE corresponds to the initiliazed area it must be updated with the actual area 
                            
            es_obj_real(idx)%es_glww(counter) = es_obj(idx)%es_glww(counter) * fac_area ! Actual ES WGWE (based on HRUs and updated Gl. A) 
            es_obj_real(idx)%es_glw(counter) = es_obj(idx)%es_glw(counter) * fac_area ! Actual ES GWE (based on HRUs and updated Gl. A) 
            es_obj_real(idx)%es_glw_init(counter) = es_obj(idx)%es_glw(counter) * fac_area ! Actual ES GWE (based on HRUs and updated Gl. A) 
            glww_sub_real(idx) = glww_sub_real(idx) + es_obj_real(idx)%es_glww(counter) ! Actual Subbasin WGWE (based on HRUs and updated Gl. A)
            es_obj_real(idx)%es_glfr(counter) = es_obj_real(idx)%es_gla(counter)/gla_sub_real(idx) ! Fraction of ES of Glacierized Subbasin Area
            es_obj_real(idx)%es_glv(counter) = es_obj_real(idx)%es_gla(counter) * es_obj_real(idx)%es_glw(counter)/1.0e6/0.917 

    end do
    !end do
    !! Make Mass Balance Correction
    do i = 1, sb_cnt
        vol1 = sum(es_obj_real(i)%es_glv)
        vol2  = sum(es_obj(i)%es_glv)

        if (vol2 > vol1) then
            diff = vol2 - vol1

            ! Count non-zero elements in es_obj_real(i)%es_glv
            count_nonzero = 0
            do j = 1, es_cnt
                if (es_obj_real(i)%es_glv(j) > 0.0) count_nonzero = count_nonzero + 1
            end do

            if (count_nonzero > 0) then
                dst = diff / count_nonzero

                ! Add the missing volume equally to non-zero entries
                do j = 1, es_cnt
                    if (es_obj_real(i)%es_glv(j) > 0.0) then
                        es_obj_real(i)%es_glv(j) = es_obj_real(i)%es_glv(j) + dst
                        es_obj_real(i)%es_glw(j) = es_obj_real(i)%es_glw(j) + (dst/es_obj_real(i)%es_gla(j)*1.0e6)
                    end if
                end do
            end if
        end if
    end do
    
    !! Initialize Subbasin Mass Balance Array
    allocate(glmb_a(sb_cnt))
    
    !! Write Initial Results to gl_mb_aa.txt
    do i = 1,sb_cnt ! Loop Over Subbasin
        do j = 1,es_cnt ! Loop over Glacier HRUs within that subbasin
            !write (89,8998) i,j,pco%yrc_start,0.000,es_obj_real(glsubs(i))%es_glw(j),es_obj_real(glsubs(i))%es_gla(j) ! 6 Variables
            write (89,8998) i,j,pco%yrc_start,0.000,es_obj_real(i)%es_glw(j),es_obj_real(i)%es_gla(j) ! 6 Variables
            !write (89,8998) i,j,pco%yrc_start,0.000,es_obj_real(mask3(i))%es_glw(j),es_obj_real(mask3(i))%es_gla(j) ! 6 Variables
            8998 format (i4,2x,i2,2x,i4,1x,e12.4,1x,g12.3,1x,e10.3)
        end do
    end do
    
    !! e) Create Glacier HRUs
    !! Initialize Glacier HRUS (HRU Areas & GWEs based on different Scales such as Basin, Total Gl. A, Subbasin etc.)
    counter=0
    cnt=0
    allocate(hru_glww2(hru_cnt), hru_glww(hru_cnt), hru_gla(hru_cnt), hru_es_id(hru_cnt), hru_fr_es(hru_cnt))
    allocate(hru_gl_obj(hru_cnt))
    hru_gla=0   
    hru_glww=0
    hru_es_id=0
    hru_fr_es=0
    hru_glww2=0

    do j = 1,hru_cnt ! Loop over all Glacier HRUs 
        !mask2 = (hrus_sub_assign==subs_gl(i)) ! Glacier HRU IDs (mask)        
        subt = hrus_sub_assign(j) ! Subbasin in wich Glacier HRU is located
        idx = 0
        do i = 1, size(subs_gl)
            if (subs_gl(i) == subt) then
                idx = i
                exit
            end if
        end do
        if (idx == 0) cycle  ! Skip if subbasin not in glacier list
        
        !! If Land Use of HRU = Glacier
        !if (mask2(j)) then ! If HRU is Glacier HRU (and part of the glacier mask)
        cropname=hru(glhruids(j))%LAND_USE_MGT_C(1:4) ! Crop Name to get ES #
        if (cropname(len_trim(cropname):len_trim(cropname)) < '0' .or. cropname(len_trim(cropname):len_trim(cropname)) > '9') then
            cropname = cropname(3:len_trim(cropname)-1)   ! Exclude the last character
        else
            cropname = cropname(3:len_trim(cropname))     ! Include all characters starting from the 3rd
        end if
        read(cropname,"(I)") counter ! Get Index of ES (e.g. ES6 = 6)
        hru_es_frac = ob(glhruids(j))%area_ha/100/es_obj_real(i)%es_gla(counter) !! HRU Fraction of ES
        hru_fr_es(j) = hru_es_frac ! HRU fraction of ES                                                              
        hru_es_id(j) = counter ! Assign ES ID to HRU

                
        ! HRU GWE on ES Scale
        hru_gla(j) = es_obj_real(i)%es_gla(counter) * hru_es_frac !! Timo Glacier Area of GHRU based on HRU Fraction of ES    
                        
        ! Option 2 (unweighted ES for glacier)
        hru_glww(j) = es_obj_real(i)%es_glw(counter) * hru_es_frac !! Timo Weighted GWE of GHRU based on HRU Fraction of ES
        hru_glww2(j) = es_obj_real(i)%es_glw(counter) !! All HRUs of ES get same GWE of that ES 
                                            
        ! Assign Glacier HRU Object
        hru_gl_obj(j)%hru_id = glhruids(j)
        hru_gl_obj(j)%hru_gla   = hru_gla(j)
        hru_gl_obj(j)%hru_glww2 = hru_glww2(j)
        hru_gl_obj(j)%hru_glww  = hru_glww(j)
        hru_gl_obj(j)%hru_fr_es = hru_fr_es(j)
        hru_gl_obj(j)%hru_es_id = hru_es_id(j)
        hru_gl_obj(j)%hru_sub_id = subs_gl(idx)
        hru_gl_obj(j)%mask_ind = idx
        !end if 
    end do
    !end do
    !end if
    allocate(hru_isgl(sp_ob%HRU),albedo_dd(sp_ob%HRU))
    allocate(hru_index_map(sp_ob%HRU))
    hru_isgl = 0. 
    hru_index_map = 0.
    albedo_dd = 0. 
   
    ! f) Assign All HRUs the Potential Position in the Glacier specific HRU Array 
    cnt = 0
    do j = 1,sp_ob%HRU         
        if (ANY(glhruids == j)) then
            hru_isgl(j) = 1
            cnt = cnt + 1
            do i = 1, hru_cnt
                if (j == glhruids(i)) then
                    hru_index_map(j) = i 
                end if 
            end do
        else
            hru_isgl(j) = 0
        end if
    end do
    
    return
    end