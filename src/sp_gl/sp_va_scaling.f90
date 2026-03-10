subroutine sp_va_scaling
  !! SWAT+GL Subroutine Volume-Area Scaling Parametrization
  !! Based on Ohmura et al. (1992) global parameters
  !! V = c * A^γ where c = 0.037 and γ = 1.36

  use hru_module
  use time_module
  use basin_module, only: glcode, glpars

  implicit none

  real :: vv, vva, vva2, v_control
  real :: vol_new, area_new, area_old
  real :: c_va, gamma_va  ! V-A scaling parameters (Ohmura et al.)
  real :: eps
  integer :: j, ii

  ! V-A scaling parameters from glpars (default: Ohmura et al. 1992)
  c_va = glpars%va_c      ! [km³/km²] - scaling constant
  gamma_va = glpars%va_gamma ! [-] - scaling exponent

  vv = 0.0

  ! Loop over subbasins
  do j = 1, size(glmb_a)

    vva = 0.0   ! Subbasin Glacier Area before evolution
    vva2 = 0.0  ! Subbasin Glacier Area after evolution
    eps = 1.0e-6

    ! Calculate initial subbasin glacier area
    do ii = 1, maxval(esids)
      if (es_obj_real(j)%es_gla_scale(ii) > 0.0) then
        vva = vva + (es_obj_real(j)%es_gla_scale(ii) * 1.0e6) ! Convert km² to m²
      end if
    end do

    ! Get annual mass balance change as volume [m³]
    vv = glmb_a(j)%glmb

    ! Check if glacier exists and has mass balance
    if (vva <= 0.0 .or. abs(vv) < eps) then
      ! No glacier or no change - set to zero
      do ii = 1, maxval(esids)
        es_obj_real(j)%es_gla(ii) = 0.0
        es_obj_real(j)%es_gla_scale(ii) = 0.0
        es_obj_real(j)%es_glw(ii) = 0.0
      end do
      glmb_a(j)%bs = 0.0
      glmb_a(j)%bw = 0.0
      glmb_a(j)%bn = 0.0
      
      ! Write output for ice-free state
      do ii = 1, maxval(esids)
        write(89,8998) es_obj_real(j)%sub_id, ii, time%yrc, vv, 0.0, glmb_a(j)%bs, glmb_a(j)%bw, &
                       es_obj_real(j)%es_glw(ii), es_obj_real(j)%es_gla_scale(ii)
      end do
      cycle
    end if

    ! Calculate total glacier volume before evolution [km³]
    ! V = A * W / 1000 / rho_ice where W is in mm and A in km²
    ! Simplified: V_km3 = A_km2 * W_m / 1000 (assuming W is water equivalent in m)
    ! Actually: V (km³) = A (km²) * W (m) / 1000 (for ice density 917 kg/m³)
    ! But we store W as mm, so: V (km³) = A (km²) * W (mm) / 1000 / 1000 = A (km²) * W (mm) / 1e6
    
    ! Using es_glw in mm and es_gla_scale in km²
    ! Volume in km³ = Area (km²) * GWE (mm) / 1,000,000
    area_old = 0.0
    do ii = 1, maxval(esids)
      if (es_obj_real(j)%es_gla_scale(ii) > 0.0 .and. es_obj_real(j)%es_glw(ii) > 0.0) then
        area_old = area_old + es_obj_real(j)%es_gla_scale(ii)
      end if
    end do

    ! Calculate new volume after mass balance [km³]
    ! vv is in m³, convert to km³
    vol_new = (area_old * sum(es_obj_real(j)%es_glw(:), &
              mask=es_obj_real(j)%es_gla_scale(:)>0.0 .and. es_obj_real(j)%es_glw(:)>0.0) / 1.0e6) + (vv / 1.0e9)

    ! Check if volume became negative (glacier disappeared)
    if (vol_new <= 0.0) then
      ! Glacier completely melted
      do ii = 1, maxval(esids)
        es_obj_real(j)%es_gla(ii) = 0.0
        es_obj_real(j)%es_gla_scale(ii) = 0.0
        es_obj_real(j)%es_glw(ii) = 0.0
        es_obj_real(j)%es_glww(ii) = 0.0
      end do
      glmb_a(j)%bs = 0.0
      glmb_a(j)%bw = 0.0
      glmb_a(j)%bn = 0.0
      
      ! Write output for ice-free state
      do ii = 1, maxval(esids)
        write(89,8998) es_obj_real(j)%sub_id, ii, time%yrc, vv, 0.0, glmb_a(j)%bs, glmb_a(j)%bw, &
                       es_obj_real(j)%es_glw(ii), es_obj_real(j)%es_gla_scale(ii)
      end do
      cycle
    end if

    ! Apply V-A scaling to get new area [km²]
    ! A = (V / c)^(1/gamma)
    area_new = (vol_new / c_va) ** (1.0 / gamma_va)

    ! Calculate area change factor
    if (area_old > 0.0) then
      v_control = area_new / area_old
    else
      v_control = 0.0
    end if

    ! Update glacier properties for each elevation band
    ! Distribute area change proportionally based on initial areas
    do ii = 1, maxval(esids)
      if (es_obj_real(j)%es_gla_scale(ii) > 0.0) then
        ! Store old values
        vva = vva + (es_obj_real(j)%es_gla_scale(ii) * 1.0e6)
        
        ! Update glacier area
        es_obj_real(j)%es_gla_scale(ii) = es_obj_real(j)%es_gla_scale(ii) * v_control
        
        ! Update glacier water equivalent (thickness)
        ! If area shrinks, thickness increases (volume conservation)
        ! If area expands, thickness decreases
        if (es_obj_real(j)%es_gla_scale(ii) > 0.0) then
          ! Recalculate GWE to conserve volume
          ! V_new = V_old * (A_new / A_old) = A_old * W_old / 1e6 * ratio
          ! W_new = V_new / A_new * 1e6 = W_old * ratio / ratio = W_old
          ! Actually, with V-A scaling, we need to recalculate properly
          
          ! Volume is conserved: V_new = V_old + delta_V
          ! W_new = V_new / A_new * 1e6
          if (area_new > 0.0) then
            es_obj_real(j)%es_glw(ii) = es_obj_real(j)%es_glw(ii) * (area_old / area_new)
            es_obj_real(j)%es_glww(ii) = es_obj_real(j)%es_glww(ii) * (area_old / area_new)
          end if
          
          ! Check for glacier recession (thickness became negative)
          if (es_obj_real(j)%es_glw(ii) < 0.0) then
            es_obj_real(j)%es_gla(ii) = 0.0
            es_obj_real(j)%es_gla_scale(ii) = 0.0
            es_obj_real(j)%es_glw(ii) = 0.0
            es_obj_real(j)%es_glww(ii) = 0.0
          end if
        else
          es_obj_real(j)%es_gla(ii) = 0.0
          es_obj_real(j)%es_glw(ii) = 0.0
          es_obj_real(j)%es_glww(ii) = 0.0
        end if
        
        ! Calculate new area for output
        vva2 = vva2 + (es_obj_real(j)%es_gla_scale(ii) * 1.0e6)
      end if
    end do

    ! Calculate specific mass balance [m/a]
    if ((vva + vva2) / 2.0 > 0.0) then
      glmb_a(j)%bn = vv / ((vva + vva2) / 2.0)  ! Specific MB in m
      glmb_a(j)%bs = glmb_a(j)%glmb_s / ((vva + vva2) / 2.0)  ! Specific Summer MB
      glmb_a(j)%bw = glmb_a(j)%glmb_w / ((vva + vva2) / 2.0)  ! Specific Winter MB
    else
      glmb_a(j)%bn = 0.0
      glmb_a(j)%bs = 0.0
      glmb_a(j)%bw = 0.0
    end if

    ! Write output for each ES and the subbasin-wide mass balance values
    do ii = 1, maxval(esids)
      write(89,8998) es_obj_real(j)%sub_id, ii, time%yrc, vv, glmb_a(j)%bn, glmb_a(j)%bs, glmb_a(j)%bw, &
                     es_obj_real(j)%es_glw(ii), es_obj_real(j)%es_gla_scale(ii)
    end do

  end do

  ! Initialize annual HRU redistribution
  call sp_init_glhrus

  return

8998 format(i4, 2x, i2, 2x, i4, 1x, e12.4, 1x, f8.3, 1x, f8.3, 1x, f8.3, 1x, g12.3, 1x, e10.3)

end subroutine sp_va_scaling