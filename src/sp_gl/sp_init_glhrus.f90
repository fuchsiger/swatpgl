    subroutine sp_init_glhrus
    !! SWAT+GL Initialize HRU's
    !! Not activated yet
    use hru_module
    
    integer:: j, ii, cnt, ja, ib
    real:: vv  
    
    ! --------------------------------
    !! Annual Volume HRU Redistribution 
    !! Initialize Glacier HRUS (HRU Areas & GWEs based on different Scales such as Basin, Total Gl. A, Subbasin etc.)  
    do j = 1,size(hru_gl_obj) ! Loop over all Glacier HRUs 
        i = hrus_sub_assign(j)
        ja = hru_gl_obj(j)%hru_es_id
                
        ! Assign Glacier HRU Object
        ! hru_gl_obj(j)%hru_gla = hru_gla(j) 
        hru_gl_obj(j)%hru_glww2 = es_obj_real(i)%es_glw(ja)
        hru_gl_obj(j)%hru_glww  = es_obj_real(i)%es_glw(ja) * hru_gl_obj(j)%hru_fr_es
    end do      
    
    !! Update Glacier Area etc. 
    !! Update Glacier Area of ES & Subbasin based on actual HRU discretization
    do ib = 1, size(glmb_a)
        if (gla_sub_real(ib) > 0) then
            vv=0
            do ii = 1, maxval(esids)
                if (es_obj_real(ib)%es_gla_scale(ii) > 0) then
                    vv = vv + es_obj_real(ib)%es_gla_scale(ii)
                end if
            end do
            gla_sub_real(ib) = vv ! Update Area in case something has changed 
        endif
    end do
    ! ------------------------------------------------   
    return
end