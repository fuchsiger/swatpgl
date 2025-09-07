subroutine sp_deltah
  !! Timo Subroutine Delta-h Parametrization

  use hru_module
  use time_module

  implicit none

  real :: vv, sum_v, v_control, v_control2, v_control3
  real :: esmn, xx, vol_rem, eps, vv2, vva, vva2
  real :: fS, fS_rem, vol_rem_iter, sum_v_rem
  integer :: j, ii, max_iter, iter

  vv = 0.0
  xx = 0.0

  ! Loop over subbasins
  do j = 1, size(glmb_a)

    v_control  = 0.0
    v_control2 = 0.0
    v_control3 = 0.0
    sum_v      = 0.0
    dh         = 0.0
    e_norm     = 0.0
    vol_rem    = 0.0
    vv         = glmb_a(j)%glmb   ! Glacier Mass Balance Change as Volume [m³]
    vv2        = 0.0   ! Glacier Mass Balance Change as Surface Elevation Change [m]
    vva        = 0.0 ! Subbasin Glacier Area before dh application (mass redistribution & pot. recession)
    vva2       = 0.0 ! Subbasin Glacier Area after dh application (mass redistribution & pot. recession)
    eps        = 1.0e-6
    max_iter   = 10
    iter       = 0

    ! Loop over ES within this subbasin
    do ii = 1, maxval(esids)
      ! In Parallel Calculate Subbasin Glacier Area before mass redistribution (and potential recession) 
      vva = vva + (es_obj_real(j)%es_gla_scale(ii)*1.0E6)
      if (es_obj_real(j)%es_gla(ii) > 0.0) then
        ! Normalize elevation
        e_norm(ii) = (es_obj_real(j)%es_mx_elev - (es_obj_real(j)%es_elup(ii) - esdist)) / &
                     (es_obj_real(j)%es_mx_elev - (es_obj_real(j)%es_mn_elev - 2.0 * esdist))

        ! Delta-h: empirical glacier thinning function (Huss et al., 2010)
        if (gla_sub_real(j) > 20.0) then
          dh(ii) = (e_norm(ii) - 0.02)**6 + 0.12 * (e_norm(ii) - 0.02)
        elseif (gla_sub_real(j) > 5.0 .and. gla_sub_real(j) <= 20.0) then
          dh(ii) = (e_norm(ii) - 0.05)**4 + 0.19 * (e_norm(ii) - 0.05) + 0.01
        else
          dh(ii) = (e_norm(ii) - 0.3)**2 + 0.6 * (e_norm(ii) - 0.3) + 0.09
        end if

        sum_v = sum_v + (es_obj_real(j)%es_gla_scale(ii) * dh(ii) * 1.0e6)
      end if

    end do

    ! Scaling factor in meters (if ice free end loop and write 0s)
    !if (sum_v == 0.0) sum_v = sum_v + eps
    if (sum_v <= 0.0) then 
        ! set everything to zero
        do ii = 1, maxval(esids)
            es_obj_real(j)%es_gla(ii)       = 0.0
            es_obj_real(j)%es_gla_scale(ii) = 0.0
            es_obj_real(j)%es_glw(ii)       = 0.0
            vv2                             = 0.0
            glmb_a(j)%bs                    = 0.0
            glmb_a(j)%bw                    = 0.0
            
        end do

        ! write 0 lines (ice-free)        
        do ii = 1, maxval(esids)
            write(89,8998) es_obj_real(j)%sub_id, ii, time%yrc, vv, vv2, glmb_a(j)%bs, glmb_a(j)%bw, &
                           es_obj_real(j)%es_glw(ii), es_obj_real(j)%es_gla_scale(ii)
        end do
        cycle   ! skip redistribution / melt update
    end if
    fS = vv / sum_v

    ! Update initial glacier thickness
    do ii = 1, maxval(esids)

      if (es_obj_real(j)%es_gla_scale(ii) > 0.0) then
        es_obj_real(j)%es_glww(ii) = es_obj_real(j)%es_glww(ii) + fS * dh(ii) * 1000.0
        es_obj_real(j)%es_glw(ii)  = es_obj_real(j)%es_glw(ii)  + fS * dh(ii) * 1000.0
        
        ! Glacier recession handling
        if (es_obj_real(j)%es_glw(ii) < 0.0) then
          vol_rem = vol_rem - (es_obj_real(j)%es_glw(ii) * es_obj_real(j)%es_gla_scale(ii) * 1000.0)
          v_control2 = v_control2 + vol_rem
          es_obj_real(j)%es_gla(ii)       = 0.0
          es_obj_real(j)%es_gla_scale(ii) = 0.0
          es_obj_real(j)%es_glw(ii)       = 0.0
          if (es_obj_real(j)%es_elup(ii) == es_obj_real(j)%es_mn_elev) then
            if (ii < maxval(esids)) then
              es_obj_real(j)%es_mn_elev = es_obj_real(j)%es_elup(ii+1)
            end if
          end if
        end if
      end if

    end do

    ! Redistribute excess melt volume if needed
    do while (vol_rem > eps .and. iter < max_iter)

      sum_v_rem = 0.0

      do ii = 1, maxval(esids)
        if (es_obj_real(j)%es_gla(ii) > 0.0) then
          sum_v_rem = sum_v_rem + (es_obj_real(j)%es_gla_scale(ii) * dh(ii) * 1.0e6)
        end if
      end do

      if (sum_v_rem <= 0.0) exit

      fS_rem       = -vol_rem / sum_v_rem ! Vol Rem should be negative in case of losing thus sign has to be changed
      vol_rem_iter = 0.0

      do ii = 1, maxval(esids)
        if (es_obj_real(j)%es_gla_scale(ii) > 0.0) then
          es_obj_real(j)%es_glw(ii)  = es_obj_real(j)%es_glw(ii)  + fS_rem * dh(ii) * 1000.0
          es_obj_real(j)%es_glww(ii) = es_obj_real(j)%es_glww(ii) + fS_rem * dh(ii) * 1000.0
          v_control3 = v_control3 + fS_rem * dh(ii) * es_obj_real(j)%es_gla_scale(ii) * 1000000
          if (es_obj_real(j)%es_glw(ii) < 0.0) then
            vol_rem_iter = vol_rem_iter - es_obj_real(j)%es_glw(ii) * es_obj_real(j)%es_gla_scale(ii) * 1000.0
            es_obj_real(j)%es_gla(ii)       = 0.0
            es_obj_real(j)%es_gla_scale(ii) = 0.0
            es_obj_real(j)%es_glw(ii)       = 0.0

            if (es_obj_real(j)%es_elup(ii) == es_obj_real(j)%es_mn_elev) then
              if (ii < maxval(esids)) then
                es_obj_real(j)%es_mn_elev = es_obj_real(j)%es_elup(ii+1)
              end if
            end if
          end if
        end if
      end do

      vol_rem = vol_rem_iter
      iter = iter + 1

    end do

    ! Recalculate ES glacier areas
    do ii = 1, maxval(esids)
      if (es_obj_real(j)%es_glw(ii) > 0.0) then
        v_control = es_obj_real(j)%es_glw(ii) * es_obj_real(j)%es_gla_scale(ii) * 1000.0
        !v_control = es_obj_real(j)%es_glw(ii) * es_obj_real(j)%es_gla(ii) * 1000.0
        es_obj_real(j)%es_gla_scale(ii) = es_obj_real(j)%es_gla_init(ii) * &
                                          (es_obj_real(j)%es_glw(ii) / es_obj_real(j)%es_glw_init(ii))**0.5
        es_obj_real(j)%es_glw(ii) = v_control / (es_obj_real(j)%es_gla_scale(ii) * 1000.0)
        ! In Parallel Calculate Subbasin Glacier Area after mass redistribution (and potential recession) 
        vva2 = vva2 + (es_obj_real(j)%es_gla_scale(ii)*1.0E6)
      end if
      ! Calculate specific mass balance      
      vv2 = vv/((vva + vva2)/2)

    end do
    ! Calculate specific MB for the whole subbasin
    vv2 = vv / ((vva + vva2) / 2.0) ! Specific Mass Balance
    glmb_a(j)%bs = glmb_a(j)%glmb_s / ((vva + vva2) / 2.0) ! Specific Mass Balance
    glmb_a(j)%bw = glmb_a(j)%glmb_w / ((vva + vva2) / 2.0) ! Specific Mass Balance
    glmb_a(j)%bn = vv2

    ! Write output for each ES and the subbasin-wide mass balance values (vv and vv2)
    do ii = 1, maxval(esids)
        write(89,8998) es_obj_real(j)%sub_id, ii, time%yrc, vv, vv2, glmb_a(j)%bs, glmb_a(j)%bw, &
                         es_obj_real(j)%es_glw(ii), es_obj_real(j)%es_gla_scale(ii)
    end do
    !8998 format(i4, 2x, i2, 2x, i4, 1x, e12.4,1x, f8.3, 1x, f8.3, 1x, f8.3, 1x, g12.3, 1x, e10.3)  
8998 format(i4, 2x, i2, 2x, i4, 1x, e12.4,1x, f8.3,1x, f8.3,1x, f8.3,1x, g12.3,1x, e10.3)
     
  end do
  
  ! Initialize annual HRU redistribution
  call sp_init_glhrus

  return
end subroutine sp_deltah

