    subroutine sp_deltah
    !! Timo Subroutine Delta-h Parametrization 
    
    use hru_module
    use time_module
    
    real::vv, sum_v, v_control, v_control2, v_control3, esmn, xx
    integer::j, ii
    vv = 0.
    xx = 0.
    ! Calculate Subbasin Ice thickness Change
    ! Subbasin Loop
    do j = 1,size(glmb_a)
        v_control=0
        v_control2=0
        v_control3=0
        sum_v=0
        dh=0.
        e_norm=0.
        vv = glmb_a(j)%glmb ! Glacier Mass Balance Change as Volume (dM in [m³])
        ! Check if Glacier Subbasin
        !if (subs_gl(j)==1) then
            ! Calculate normalized elevation of each ES
            ! Loop Over ES in that Subbasin
            do ii = 1, maxval(esids)
                ! Check if Glacier ES
                if (es_obj_real(j)%es_gla(ii) > 0) then
                ! Normalize ES Elevation
                e_norm(ii)=(es_obj_real(j)%es_mx_elev-(es_obj_real(j)%es_elup(ii)-esdist))/  &
                            (es_obj_real(j)%es_mx_elev-(es_obj_real(j)%es_mn_elev-(2*esdist)))

                !! Delta-h Empirical Functions based on Huss et al. 2010
                if (gla_sub_real(j)>20) then
                    ! Large Valley Glacier >20 km²
                    dh(ii) = (e_norm(ii) - 0.02)**6 + 0.12 *(e_norm(ii)-0.02)                    
                elseif (gla_sub_real(j) > 5 .AND. gla_sub_real(j) <= 20) then
                    ! Medium Valley Glacier 5-20 km²
                    dh(ii) = (e_norm(ii) - 0.05)**4 + 0.19 *(e_norm(ii)-0.05) + 0.01
                else
                    ! Small Valley Glacier <5 km²
                    dh(ii) = (e_norm(ii) - 0.3)**2 + 0.6 *(e_norm(ii)-0.3) + 0.09
                end if
                ! Calculate Scaling factor fS 
                ! (positive for accumulation, negative for ablation)
                sum_v = sum_v + (es_obj_real(j)%es_gla(ii) * dh(ii) * 1.0E6) ! Calculate unscaled volume (dh is dimensionless and 10^6 is conversion factor for km² in m²)
                fS=vv/sum_v ! fS in m          
                end if
            end do
  
            ! Update Initial Glacier Thickness
            do ii = 1, maxval(esids)
                if (es_obj_real(j)%es_gla(ii) > 0) then
                    es_obj_real(j)%es_glww(ii)= es_obj_real(j)%es_glww(ii) + (fS * dh(ii) * 1000 ) ! fS in m (*1000 for mm)
                    es_obj_real(j)%es_glw(ii) = es_obj_real(j)%es_glw(ii) + (fS * dh(ii) * 1000) ! fS in m (*1000 for mm)
                    
                    ! If GWE gets 0 -> Area = 0 (glacier receedes) 
                    !if (es_gl_wm2(ii,j)<0.) then ! If GWE gets 0 -> Area = 0 (glacier receedes) 
                    if (es_obj_real(j)%es_glw(ii) < 0.) then
                        es_obj_real(j)%es_gla(ii) = 0.
                        es_obj_real(j)%es_gla_scale(ii) = 0.
                        es_obj_real(j)%es_glw(ii) = 0.

                        ! Adapt Min Elevation in Case it is necessary
                        if (es_obj_real(j)%es_elup(ii) == es_obj_real(j)%es_mn_elev) then
                            if (ii < maxval(esids)) then
                                es_obj_real(j)%es_mn_elev = es_obj_real(j)%es_elup(ii+1)
                            end if
                        end if
                    end if
                    ! ES Area scaling (if glacier not receeded)
                    if (es_obj_real(j)%es_glw(ii) > 0.) then
                        v_control = es_obj_real(j)%es_glw(ii) * es_obj_real(j)%es_gla_scale(ii)*1000
                        es_obj_real(j)%es_gla_scale(ii) = es_obj_real(j)%es_gla_init(ii) * (es_obj_real(j)%es_glw(ii)/es_obj_real(j)%es_glw_init(ii))**0.5
                        es_obj_real(j)%es_glw(ii) = v_control/(es_obj_real(j)%es_gla_scale(ii)*1000)
                    end if 
                    ! Control Vol
                    v_control2 = v_control2 + es_obj_real(j)%es_glw(ii) * es_obj_real(j)%es_gla(ii)*1000 ! Control Vol of Subb after ES adaption
                end if
                !end if
                ! Write Results to annual summary file (gl_mb_aa.txt)
                !if (time%curyr > time%nyskip) then
                !    write (89,8998) j, ii, pco%yrc_start, gla_sub_real(ib), vv, es_obj_real(j)%es_glw(ii), es_obj_real(j)%es_gla_scale(ii) ! Scaled Area & Ice Thickness
                !    8998 format (i4,2x,i2,2x,i4,1x,e10.3,1x,e12.4,1x,g12.3,1x,e10.3)                   
                !end if
                
                write (89,8998) j,ii,time%yrc,vv,es_obj_real(j)%es_glw(ii),es_obj_real(j)%es_gla_scale(ii) ! 6 Variables
                8998 format (i4,2x,i2,2x,i4,1x,e12.4,1x,g12.3,1x,e10.3)
            end do
        !end if
    end do
    
    ! ------------------------------------------------------------
    !! Annual Volume HRU Redistribution 
    !! Initialize Glacier HRUS (HRU Areas & GWEs based on different Scales such as Basin, Total Gl. A, Subbasin etc.)
    call sp_init_glhrus
    
    return
end