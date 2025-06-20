      subroutine cal_parm_select (ielem, ly, chg_parm, chg_typ, chg_val, absmin, absmax, num_db)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine finds the current paramter value based on 
!!    user defined change

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    val_cur     |variable      |current parameter value
!!                               |the standard temperature (20 degrees C)
!!    chg         |data type     |contains information on variable change
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      
      use basin_module
      use channel_data_module 
      use reservoir_data_module
      use hru_module, only : hru, isol, cn2, brt, tconc, gldb, hru_index_map
      use soil_module
      use channel_module
      use conditional_module
      use sd_channel_module
      use reservoir_module
      use aquifer_module
      use hru_lte_module
      use organic_mineral_mass_module
      use hydrograph_module
      use pesticide_data_module
      use plant_module
      
      implicit none

      character(len=16), intent (in) :: chg_parm            !                |               
      character(len=16), intent (in) :: chg_typ             !variable        |type of change (absval, abschg, pctchg)
      real, intent (in) :: chg_val                          !                |      
      real, intent (in) :: absmin                           !                |minimum range for variable 
      real, intent (in) :: absmax                           !                |maximum change for variable
      integer, intent (in) :: ielem                         !                | 
      integer, intent (in) :: num_db                        !                | 
      integer, intent (in) :: ly                            !                |
      integer :: jj                                         !                |soil layer counter
      integer :: ipl                                        !                |soil layer counter
      integer :: ihru                                       !                |hru counter
      real :: exp                                           !                | 
      real :: c_val                                         !                | 
      real :: abmax                                         !                | 
      real :: chg_par                                       !variable        |new parameter value
      real :: perc_ln_func                                  !none       |function to convert perco to perc_lim
      real :: rock                                          !                | 
      integer :: glid                                       ! SWAT+GL 
      
      select case (chg_parm)

      case ("cn2")
        cn2(ielem) = chg_par (cn2(ielem), chg_typ, chg_val, absmin, absmax)
        call curno (cn2(ielem), ielem)

      !! HRU  
      case ("biomix") 
        hru(ielem)%hyd%biomix = chg_par (hru(ielem)%hyd%biomix,           &
                          chg_typ, chg_val, absmin, absmax)
        
      case ("cn3_swf")
        !! don't change for tile  *********************Mike
        if (hru(ielem)%tiledrain == 0) then
          hru(ielem)%hyd%cn3_swf = chg_par (hru(ielem)%hyd%cn3_swf,         &
                         chg_typ, chg_val, absmin, absmax)
          call curno (cn2(ielem), ielem)
        end if
        
      case ("usle_p")
        hru(ielem)%lumv%usle_p = chg_par (hru(ielem)%lumv%usle_p,         &
                          chg_typ, chg_val, absmin, absmax)
        
      case ("ovn")
        hru(ielem)%luse%ovn = chg_par (hru(ielem)%luse%ovn,               &
                          chg_typ, chg_val, absmin, absmax)
        
      case ("elev")
        hru(ielem)%topo%elev = chg_par (hru(ielem)%topo%elev,             &
                          chg_typ, chg_val, absmin, absmax)
        
      case ("slope")
        hru(ielem)%topo%slope = chg_par (hru(ielem)%topo%slope,           & 
                          chg_typ, chg_val, absmin, absmax)
        
      case ("slope_len")
        hru(ielem)%topo%slope_len = chg_par(hru(ielem)%topo%slope_len,    &
                          chg_typ, chg_val, absmin, absmax)
        
      case ("lat_ttime")
        hru(ielem)%hyd%lat_ttime = chg_par(hru(ielem)%hyd%lat_ttime,      &
                          chg_typ, chg_val, absmin, absmax)
        hru(ielem)%hyd%lat_ttime = 1. - Exp(-1. / hru(ielem)%hyd%lat_ttime)
            
      case ("lat_sed")
        hru(ielem)%hyd%lat_sed = chg_par (hru(ielem)%hyd%lat_sed,         & 
                          chg_typ, chg_val, absmin, absmax)
        
      case ("lat_len")
        hru(ielem)%topo%lat_len = chg_par (hru(ielem)%topo%lat_len,     &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("latq_co")
        hru(ielem)%hyd%latq_co = chg_par (hru(ielem)%hyd%latq_co,       &
                          chg_typ, chg_val, absmin, absmax)
        
      case ("canmx")
        hru(ielem)%hyd%canmx = chg_par (hru(ielem)%hyd%canmx,           & 
                         chg_typ, chg_val, absmin, absmax)
        
      case ("esco")
        hru(ielem)%hyd%esco = chg_par (hru(ielem)%hyd%esco,             & 
                         chg_typ, chg_val, absmin, absmax)
         
      case ("epco")
        hru(ielem)%hyd%epco = chg_par (hru(ielem)%hyd%epco,             & 
                         chg_typ, chg_val, absmin, absmax)             
        !! set epco parameter for each crop
        do ipl = 1, pcom(ielem)%npl
          pcom(ielem)%plcur(ipl)%epco = hru(ielem)%hyd%epco
        end do
        
      case ("erorgn")
        hru(ielem)%hyd%erorgn = chg_par (hru(ielem)%hyd%erorgn,         & 
                         chg_typ, chg_val, absmin, absmax)
        
      case ("erorgp")
        hru(ielem)%hyd%erorgp = chg_par (hru(ielem)%hyd%erorgp,         &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("dis_stream")
        hru(ielem)%topo%dis_stream = chg_par(hru(ielem)%topo%dis_stream,&
                         chg_typ, chg_val, absmin, absmax)
        
      case ("perco")
        !! don't change for tile  *********************Mike
        if (hru(ielem)%tiledrain == 0) then
        hru(ielem)%hyd%perco = chg_par (hru(ielem)%hyd%perco,           &
                         chg_typ, chg_val, absmin, absmax)
        if (hru(ielem)%hyd%perco > 1.e-9) then
          perc_ln_func = 1.0052 * log(-log(hru(ielem)%hyd%perco - 1.e-6)) + 5.6862
          hru(ielem)%hyd%perco_lim = exp(-perc_ln_func)
          hru(ielem)%hyd%perco_lim = amin1 (1., hru(ielem)%hyd%perco_lim)
        else
          hru(ielem)%hyd%perco_lim = 0.
        end if
        end if
                
      case ("petco")
        hru(ielem)%hyd%pet_co = chg_par (hru(ielem)%hyd%pet_co,     &
                         chg_typ, chg_val, absmin, absmax)

      case ("lat_orgn")
        hru(ielem)%hyd%lat_orgn = chg_par (hru(ielem)%hyd%lat_orgn,     &
                         chg_typ, chg_val, absmin, absmax)
      
      case ("lat_orgp")
        hru(ielem)%hyd%lat_orgp = chg_par (hru(ielem)%hyd%lat_orgp,     & 
                         chg_typ, chg_val, absmin, absmax)
        
      case ("field_len")
        hru(ielem)%field%length = chg_par(hru(ielem)%field%length,      &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("field_wid")
        hru(ielem)%field%wid = chg_par(hru(ielem)%field%wid,            &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("field_ang")
        hru(ielem)%field%ang = chg_par(hru(ielem)%field%ang,            &
                         chg_typ, chg_val, absmin, absmax)

       case ("snofall_tmp")
        hru(ielem)%sno%falltmp = chg_par(hru(ielem)%sno%falltmp,        &
                         chg_typ, chg_val, absmin, absmax)
               
      case ("snomelt_tmp")
        hru(ielem)%sno%melttmp = chg_par(hru(ielem)%sno%melttmp,        &
                         chg_typ, chg_val, absmin, absmax)
               
      case ("snomelt_max")
        hru(ielem)%sno%meltmx = chg_par(hru(ielem)%sno%meltmx,        &
                         chg_typ, chg_val, absmin, absmax)
               
      case ("snomelt_min")
        hru(ielem)%sno%meltmn = chg_par(hru(ielem)%sno%meltmn,        &
                         chg_typ, chg_val, absmin, absmax)
               
      case ("snomelt_lag")
        hru(ielem)%sno%timp = chg_par(hru(ielem)%sno%timp,            &
                         chg_typ, chg_val, absmin, absmax)
                     
      case ("tile_dep")
        hru(ielem)%lumv%sdr_dep = chg_par(hru(ielem)%lumv%sdr_dep,      &
                         chg_typ, chg_val, absmin, absmax)
        !! define soil layer the drainage tile is in
        if (hru(ielem)%lumv%sdr_dep > 0) then
          do jj = 1, soil(ielem)%nly
            if (hru(ielem)%lumv%sdr_dep < soil(ielem)%phys(jj)%d) hru(ielem)%lumv%ldrain = jj
            if (hru(ielem)%lumv%sdr_dep < soil(ielem)%phys(jj)%d) exit
          end do
        else
          hru(ielem)%lumv%ldrain = 0
        end if    
        
      case ("tile_dtime")
        hru(ielem)%sdr%time = chg_par(hru(ielem)%sdr%time,             &
                         chg_typ, chg_val, absmin, absmax)
        !! setting tile lage time
        if (hru(ielem)%lumv%ldrain > 0 .and. hru(ielem)%sdr%lag > 0.01) then
          hru(ielem)%lumv%tile_ttime = 1. - Exp(-24. / hru(ielem)%sdr%lag)
        else
          hru(ielem)%lumv%tile_ttime = 0.
        end if
               
      case ("tile_lag")
        hru(ielem)%sdr%lag = chg_par(hru(ielem)%sdr%lag,               &
                         chg_typ, chg_val, absmin, absmax)

      case ("tile_rad")
        hru(ielem)%sdr%radius = chg_par(hru(ielem)%sdr%radius,         &
                         chg_typ, chg_val, absmin, absmax)
                      
      case ("tile_dist")
        hru(ielem)%sdr%dist = chg_par(hru(ielem)%sdr%dist,             &
                         chg_typ, chg_val, absmin, absmax)
                      
      case ("tile_drain")
        hru(ielem)%sdr%drain_co = chg_par(hru(ielem)%sdr%drain_co,     &
                         chg_typ, chg_val, absmin, absmax)
                      
      case ("tile_pump")
        hru(ielem)%sdr%pumpcap = chg_par(hru(ielem)%sdr%pumpcap,       &
                         chg_typ, chg_val, absmin, absmax)
                      
      case ("tile_latk")
        hru(ielem)%sdr%lag = chg_par(hru(ielem)%sdr%latksat,           &
                         chg_typ, chg_val, absmin, absmax)
                      
      !! HRU SWAT+GL Extension   
      case("glmtmp")
        glid = hru_index_map(ielem)
        if (glid>0) then
        gldb(glid)%glmtmp = chg_par(gldb(glid)%glmtmp,            &
                         chg_typ, chg_val, absmin, absmax)
        end if
        
      case("glmfmx")
        glid = hru_index_map(ielem)
        if (glid>0) then
        gldb(glid)%glmfmx = chg_par(gldb(glid)%glmfmx,            &
                         chg_typ, chg_val, absmin, absmax)  
        end if
        
      case("glmfmn")
        glid = hru_index_map(ielem)
        if (glid>0) then
        gldb(glid)%glmfmn = chg_par(gldb(glid)%glmfmn,            &
                         chg_typ, chg_val, absmin, absmax)
        end if
        
      case("f_frz")
        glid = hru_index_map(ielem)
        if (glid>0) then
        gldb(glid)%f_frz = chg_par(gldb(glid)%f_frz,            &
                         chg_typ, chg_val, absmin, absmax)
        end if
        
      case("f_accu")
        glid = hru_index_map(ielem)
        if (glid>0) then
        gldb(glid)%f_accu = chg_par(gldb(glid)%f_accu,            &
                         chg_typ, chg_val, absmin, absmax)
        end if
        
      case("gl_lag")
        glid = hru_index_map(ielem)
        if (glid>0) then
        gldb(glid)%gl_lag = chg_par(gldb(glid)%gl_lag,            &
                         chg_typ, chg_val, absmin, absmax)
        endif
        
      case("tfac_s")
        !glid = hru_index_map(ielem)
        !if (glid>0) then
        !gldb(glid)%tfac_s = chg_par(gldb(glid)%tfac_s,            &
        !                 chg_typ, chg_val, absmin, absmax)
        !end if
        hru(ielem)%sno%tfac_s = chg_par(hru(ielem)%sno%tfac_s,        &
                         chg_typ, chg_val, absmin, absmax)
        
      case("tfac_i")
        glid = hru_index_map(ielem)
        if (glid>0) then
        gldb(glid)%tfac_i = chg_par(gldb(glid)%tfac_i,            &
                         chg_typ, chg_val, absmin, absmax)
        end if
        
      case("rfac_s")
        !glid = hru_index_map(ielem)
        !if (glid>0) then
        !gldb(glid)%rfac_s = chg_par(gldb(glid)%rfac_s,            &
        !                 chg_typ, chg_val, absmin, absmax)
        !end if
        hru(ielem)%sno%rfac_s = chg_par(hru(ielem)%sno%rfac_s,        &
                         chg_typ, chg_val, absmin, absmax)
        
      case("rfac_i")
        glid = hru_index_map(ielem)
        if (glid>0) then
        gldb(glid)%rfac_i = chg_par(gldb(glid)%rfac_i,            &
                         chg_typ, chg_val, absmin, absmax)
        end if
        
      case("srfac_s")
        !glid = hru_index_map(ielem)
        !if (glid>0) then        
        !gldb(glid)%srfac_s = chg_par(gldb(glid)%srfac_s,            &
        !                 chg_typ, chg_val, absmin, absmax)
        !end if
        hru(ielem)%sno%srfac_s = chg_par(hru(ielem)%sno%srfac_s,        &
                         chg_typ, chg_val, absmin, absmax)
        
      case("srfac_i")
        glid = hru_index_map(ielem)
        if (glid>0) then
        gldb(glid)%srfac_i = chg_par(gldb(glid)%srfac_i,            &
                         chg_typ, chg_val, absmin, absmax)
        end if
        
      case("pr_fac")
        !glid = hru_index_map(ielem)
        !if (glid>0) then
        !gldb(glid)%pr_fac = chg_par(gldb(glid)%pr_fac,            &
        !                 chg_typ, chg_val, absmin, absmax)
        !end if
        hru(ielem)%sno%pr_fac = chg_par(hru(ielem)%sno%pr_fac,        &
                         chg_typ, chg_val, absmin, absmax)
                
      case("f_exp")
        !glid = hru_index_map(ielem)
        !if (glid>0) then
        !gldb(glid)%f_exp = chg_par(gldb(glid)%f_exp,            &
        !                 chg_typ, chg_val, absmin, absmax)
        !end if
        hru(ielem)%sno%f_exp = chg_par(hru(ielem)%sno%f_exp,        &
                         chg_typ, chg_val, absmin, absmax)
      !! SOL
      case ("klat")
          soil(ielem)%phys(ly)%klat = chg_par(soil(ielem)%phys(ly)%klat,      &
                         chg_typ, chg_val, absmin, absmax)
          flag_klat = 1 ! Klat independent from k
          ! soil(ielem)%phys(ly)%hk = (soil(ielem)%phys(ly)%ul - soil(ielem)%phys(ly)%fc) / soil(ielem)%phys(ly)%k
          ! if (soil(ielem)%phys(ly)%hk < 1.) soil(ielem)%phys(ly)%hk = 1.
      
      case ("anion_excl")
        soil(isol)%anion_excl = chg_par(soil(isol)%anion_excl,         &
                         chg_typ, chg_val, absmin, absmax)
         
      case ("crk")
         soil(isol)%crk = chg_par(soil(isol)%crk,                      &
                         chg_typ, chg_val, absmin, absmax)
         
      case ("z")
          soil(ielem)%phys(ly)%d = chg_par(soil(ielem)%phys(ly)%d,     &
                         chg_typ, chg_val, absmin, absmax)
          call soil_awc_init (ielem)
          call curno (cn2(ielem), ielem)
         
      case ("bd")
          soil(ielem)%phys(ly)%bd = chg_par(soil(ielem)%phys(ly)%bd,    &
                         chg_typ, chg_val, absmin, absmax)
          call soil_awc_init (ielem)
          call curno (cn2(ielem), ielem)
         
      case ("awc")
          soil(ielem)%phys(ly)%awc = chg_par(soil(ielem)%phys(ly)%awc,  &
                         chg_typ, chg_val, absmin, absmax)
          call soil_awc_init (ielem)
          call curno (cn2(ielem), ielem)
        
      case ("k")
          soil(ielem)%phys(ly)%k = chg_par(soil(ielem)%phys(ly)%k,      &
                         chg_typ, chg_val, absmin, absmax)
          soil(ielem)%phys(ly)%hk = (soil(ielem)%phys(ly)%ul - soil(ielem)%phys(ly)%fc) / soil(ielem)%phys(ly)%k
          if (soil(ielem)%phys(ly)%hk < 1.) soil(ielem)%phys(ly)%hk = 1.
         
      case ("cbn")
          soil1(ielem)%tot(ly)%c = chg_par(soil1(ielem)%tot(ly)%c,    &
                         chg_typ, chg_val, absmin, absmax)
         
      case ("clay")
          soil(ielem)%phys(ly)%clay = chg_par(soil(ielem)%phys(ly)%clay, &
                         chg_typ, chg_val, absmin, absmax)
          call soil_awc_init (ielem)
          call soil_text_init (ielem)
          call curno (cn2(ielem), ielem)
         
      case ("silt")
          soil(ielem)%phys(ly)%silt = chg_par(soil(ielem)%phys(ly)%silt, &
                         chg_typ, chg_val, absmin, absmax)
          call soil_text_init (ielem)
         
      case ("sand")
          soil(ielem)%phys(ly)%sand = chg_par(soil(ielem)%phys(ly)%sand, &
                         chg_typ, chg_val, absmin, absmax)
          call soil_text_init (ielem)
         
      case ("rock")
          soil(ielem)%phys(ly)%rock = chg_par(soil(ielem)%phys(ly)%rock, &
                         chg_typ, chg_val, absmin, absmax)
          if (ly == 1) then
            rock = Exp(-.053 * soil(ielem)%phys(1)%rock)
            hru(ielem)%lumv%usle_mult = rock * soil(ielem)%ly(1)%usle_k *       &
                                 hru(ielem)%lumv%usle_p * hru(ielem)%lumv%usle_ls * 11.8
          end if

      case ("alb")
          soil(ielem)%ly(ly)%alb = chg_par(soil(ielem)%ly(ly)%alb,       &
                         chg_typ, chg_val, absmin, absmax)

      case ("usle_k")
          soil(ielem)%ly(ly)%usle_k = chg_par(soil(ielem)%ly(ly)%usle_k, &
                         chg_typ, chg_val, absmin, absmax)
          rock = Exp(-.053 * soil(ielem)%phys(1)%rock)
          hru(ielem)%lumv%usle_mult = rock * soil(ielem)%ly(1)%usle_k *       &
                                 hru(ielem)%lumv%usle_p * hru(ielem)%lumv%usle_ls * 11.8

      case ("ec")
          soil(ielem)%ly(ly)%ec = chg_par(soil(ielem)%ly(ly)%ec,         &
                         chg_typ, chg_val, absmin, absmax)
         
      case ("cal")
          soil(ielem)%ly(ly)%cal = chg_par(soil(ielem)%ly(ly)%cal,       &
                         chg_typ, chg_val, absmin, absmax)
      
      case ("ph")
           soil(ielem)%ly(ly)%ph = chg_par(soil(ielem)%ly(ly)%ph,        &
                         chg_typ, chg_val, absmin, absmax)
    
       
      !! BSN
      case ("plaps") ! HRU or Basin Plaps - TIMO SWAT+GL
        if (bsn_cc%lapse==1) then
            bsn_prm%plaps = chg_par(bsn_prm%plaps,                         &
                         chg_typ, chg_val, absmin, absmax)
        elseif (bsn_cc%lapse==2) then
            hru(ielem)%hyd%plaps = chg_par(hru(ielem)%hyd%plaps,                         &
                    chg_typ, chg_val, absmin, absmax)
        end if
      case ("tlaps") ! HRU or Basin Tlaps - TIMO SWAT+GL
        if (bsn_cc%lapse==1) then
          bsn_prm%tlaps = chg_par(bsn_prm%tlaps,                         &
                         chg_typ, chg_val, absmin, absmax)
        elseif (bsn_cc%lapse==2) then
            hru(ielem)%hyd%tlaps = chg_par(hru(ielem)%hyd%tlaps,                         &
                    chg_typ, chg_val, absmin, absmax)
        end if                                  
      case ("surlag") ! HRU or Basin Surlag - TIMO SWAT+GL
        if (bsn_cc%surlag_sd==0) then
          bsn_prm%surlag = chg_par(bsn_prm%surlag,                         &
                        chg_typ, chg_val, absmin, absmax)
        do ihru = 1, sp_ob%hru
            brt(ihru) = 1. - Exp(-bsn_prm%surlag / tconc(ihru))
        end do
        elseif (bsn_cc%surlag_sd==1) then
            hru(ielem)%hyd%surlag = chg_par(hru(ielem)%hyd%surlag,                         &
                         chg_typ, chg_val, absmin, absmax)
        end if
      case ("adj_pkr")
        bsn_prm%adj_pkr = chg_par(bsn_prm%adj_pkr,                      &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("prf")
        bsn_prm%prf = chg_par(bsn_prm%prf,                              &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("evrch")
        bsn_prm%evrch = chg_par(bsn_prm%evrch,                          &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("evlai")
        bsn_prm%evlai = chg_par(bsn_prm%evlai,                          &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("ffcb")
        bsn_prm%ffcb = chg_par(bsn_prm%ffcb,                            &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("cmn")
        bsn_prm%cmn = chg_par(bsn_prm%cmn,                              &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("nperco")
        bsn_prm%nperco = chg_par(bsn_prm%nperco,                        &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("pperco")
        bsn_prm%pperco = chg_par(bsn_prm%pperco,                        &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("phoskd")
        bsn_prm%phoskd = chg_par(bsn_prm%phoskd,                        &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("psp")
        bsn_prm%psp = chg_par(bsn_prm%psp,                              &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("rsdco")
        bsn_prm%rsdco = chg_par(bsn_prm%rsdco,                          &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("percop")
        bsn_prm%percop = chg_par(bsn_prm%percop,                        &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("msk_co1")
        bsn_prm%msk_co1= chg_par(bsn_prm%msk_co1,                       &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("msk_co2")
        bsn_prm%msk_co2 = chg_par(bsn_prm%msk_co2,                      &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("msk_x")
        bsn_prm%msk_x = chg_par(bsn_prm%msk_x,                          &
                         chg_typ, chg_val, absmin, absmax)
                    
      case ("nperco_lchtile")
        bsn_prm%msk_x = chg_par(bsn_prm%nperco_lchtile,                          &
                         chg_typ, chg_val, absmin, absmax)                         

      case ("cdn")
        bsn_prm%cdn = chg_par(bsn_prm%cdn,                              &
                         chg_typ, chg_val, absmin, absmax)
         
      case ("tb_adj")
        bsn_prm%tb_adj = chg_par(bsn_prm%tb_adj,                        &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("sdnco")
        bsn_prm%sdnco = chg_par(bsn_prm%sdnco,                          &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("n_updis")
        bsn_prm%n_updis = chg_par(bsn_prm%n_updis,                      &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("p_updis")
        bsn_prm%p_updis = chg_par(bsn_prm%p_updis,                      &
                         chg_typ, chg_val, absmin, absmax)
        
      case ("dorm_hr")
        bsn_prm%dorm_hr = chg_par(bsn_prm%dorm_hr,                      &
                         chg_typ, chg_val, absmin, absmax)
    !! SWAT+GL BSN Extension
      case("tmix_ul")
        glpars(1)%tmix_ul = chg_par(glpars(1)%tmix_ul,            &
                         chg_typ, chg_val, absmin, absmax)

    !!     SWQ
      case ("rs1")
          ch_nut(ielem)%rs1 = chg_par(ch_nut(ielem)%rs1,                &
                         chg_typ, chg_val, absmin, absmax)
         
       case ("rs2")
          ch_nut(ielem)%rs2 = chg_par(ch_nut(ielem)%rs2,                &
                         chg_typ, chg_val, absmin, absmax)
        
       case ("rs3")
          ch_nut(ielem)%rs3 = chg_par(ch_nut(ielem)%rs3,                &
                         chg_typ, chg_val, absmin, absmax) 
  
       case ("rs4")
          ch_nut(ielem)%rs4 = chg_par(ch_nut(ielem)%rs4,                &
                         chg_typ, chg_val, absmin, absmax)
        
       case ("rs5")
          ch_nut(ielem)%rs5 = chg_par(ch_nut(ielem)%rs5,                &
                         chg_typ, chg_val, absmin, absmax)
        
       case ("rs6")
          ch_nut(ielem)%rs6 = chg_par(ch_nut(ielem)%rs6,                &
                         chg_typ, chg_val, absmin, absmax) 
        
       case ("rs7")
          ch_nut(ielem)%rs7 = chg_par(ch_nut(ielem)%rs7,                &
                         chg_typ, chg_val, absmin, absmax)
        
       case ("rk1")
          ch_nut(ielem)%rk1 = chg_par(ch_nut(ielem)%rk1,                &
                         chg_typ, chg_val, absmin, absmax)
        
       case ("rk2")
          ch_nut(ielem)%rk2 = chg_par(ch_nut(ielem)%rk2,                &
                         chg_typ, chg_val, absmin, absmax)
        
       case ("rk3")
          ch_nut(ielem)%rk3 = chg_par(ch_nut(ielem)%rk3,                &
                         chg_typ, chg_val, absmin, absmax) 
        
       case ("rk4")
          ch_nut(ielem)%rk4 = chg_par(ch_nut(ielem)%rk4,                &
                         chg_typ, chg_val, absmin, absmax) 
        
       case ("rk5")
          ch_nut(ielem)%rs2 = chg_par(ch_nut(ielem)%rs2,                &
                         chg_typ, chg_val, absmin, absmax) 
        
       case ("rk6")
          ch_nut(ielem)%rk6 = chg_par(ch_nut(ielem)%rk6,                &
                         chg_typ, chg_val, absmin, absmax)
        
       case ("bc1")
          ch_nut(ielem)%bc1 = chg_par(ch_nut(ielem)%bc1,                &
                         chg_typ, chg_val, absmin, absmax)
        
       case ("bc2")
          ch_nut(ielem)%bc2 = chg_par(ch_nut(ielem)%bc2,                &
                         chg_typ, chg_val, absmin, absmax)
        
       case ("bc3")
          ch_nut(ielem)%bc3 = chg_par(ch_nut(ielem)%bc3,                &
                         chg_typ, chg_val, absmin, absmax)
        
        case ("bc4")
          ch_nut(ielem)%bc4 = chg_par(ch_nut(ielem)%bc4,                &
                         chg_typ, chg_val, absmin, absmax)
        case ("rch_dox")
          ch(ielem)%rch_dox = chg_par(ch(ielem)%rch_dox,                &
                         chg_typ, chg_val, absmin, absmax)
        
        case ("rch_cbod")
          ch(ielem)%rch_cbod = chg_par(ch(ielem)%rch_cbod,              &
                         chg_typ, chg_val, absmin, absmax)
        
        case ("algae")
          ch(ielem)%algae = chg_par(ch(ielem)%algae,                    &
                         chg_typ, chg_val, absmin, absmax)
        
        case ("organicn")
          ch(ielem)%organicn = chg_par(ch(ielem)%organicn,              &
                         chg_typ, chg_val, absmin, absmax)
        
        case ("ammonian")
          ch(ielem)%ammonian = chg_par(ch(ielem)%ammonian,              &
                         chg_typ, chg_val, absmin, absmax) 
        
        case ("nitriten")
          ch(ielem)%nitriten = chg_par(ch(ielem)%nitriten,              &
                         chg_typ, chg_val, absmin, absmax)
        
        case ("organicp")
          ch(ielem)%organicp = chg_par(ch(ielem)%organicp,              &
                         chg_typ, chg_val, absmin, absmax) 
        
        case ("disolvp")
          ch(ielem)%disolvp = chg_par(ch(ielem)%disolvp,                &
                         chg_typ, chg_val, absmin, absmax)
          
!!     PST
        case ("pst_koc")
          pestdb(ielem)%koc = chg_par(pestdb(ielem)%koc,                &
                         chg_typ, chg_val, absmin, absmax) 
          
        case ("pst_washoff")
          pestdb(ielem)%washoff = chg_par(pestdb(ielem)%washoff,        &
                         chg_typ, chg_val, absmin, absmax) 
          
        case ("pst_foliar_hlife")
          pestdb(ielem)%foliar_hlife = chg_par(pestdb(ielem)%foliar_hlife, &
                         chg_typ, chg_val, absmin, absmax) 

        case ("pst_soil_hlife")
          pestdb(ielem)%soil_hlife = chg_par(pestdb(ielem)%soil_hlife,    &
                         chg_typ, chg_val, absmin, absmax) 
                    
        case ("pst_solub")
          pestdb(ielem)%solub = chg_par(pestdb(ielem)%solub,    &
                         chg_typ, chg_val, absmin, absmax) 
        
        case ("pst_aq_hlife")
          pestdb(ielem)%aq_hlife = chg_par(pestdb(ielem)%aq_hlife,        &
                         chg_typ, chg_val, absmin, absmax)
        
        case ("pst_aq_volat")
          pestdb(ielem)%aq_volat = chg_par(pestdb(ielem)%aq_volat,        &
                         chg_typ, chg_val, absmin, absmax) 
 
        case ("pst_aq_settle")
          pestdb(ielem)%aq_settle = chg_par(pestdb(ielem)%aq_settle,        &
                         chg_typ, chg_val, absmin, absmax) 
        
        case ("pst_aq_resus")
          pestdb(ielem)%aq_resus = chg_par(pestdb(ielem)%aq_resus,        &
                         chg_typ, chg_val, absmin, absmax)

        case ("pst_ben_hlife")
          pestdb(ielem)%ben_hlife = chg_par(pestdb(ielem)%ben_hlife,  &
                         chg_typ, chg_val, absmin, absmax) 
        
        case ("pst_ben_bury")
          pestdb(ielem)%ben_bury = chg_par(pestdb(ielem)%ben_bury,  &
                         chg_typ, chg_val, absmin, absmax)
        
        case ("pst_ben_act_dep")
          pestdb(ielem)%ben_act_dep = chg_par(pestdb(ielem)%ben_act_dep,  &
                         chg_typ, chg_val, absmin, absmax)
        
!!      channel hydrology and sediment parms
         case ("chw")
            sd_ch(ielem)%chw = chg_par(sd_ch(ielem)%chw,                  &
                         chg_typ, chg_val, absmin, absmax)
       
         case ("chd")
            sd_ch(ielem)%chd = chg_par(sd_ch(ielem)%chd,                  &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("chs")
            sd_ch(ielem)%chs = chg_par(sd_ch(ielem)%chs,                  &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("chl")
            sd_ch(ielem)%chl = chg_par(sd_ch(ielem)%chl,                  &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("chn")
            sd_ch(ielem)%chn = chg_par(sd_ch(ielem)%chn,                  &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("chk")
            sd_ch(ielem)%chk = chg_par(sd_ch(ielem)%chk,                  &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("cherod")
            sd_ch(ielem)%cherod = chg_par(sd_ch(ielem)%cherod,            &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("cov")
            sd_ch(ielem)%cov = chg_par(sd_ch(ielem)%cov,                  &
                        chg_typ, chg_val, absmin, absmax)
            
        ! case ("wd_rto")
        !    sd_ch(ielem)%wd_rto = chg_par(sd_ch(ielem)%wd_rto,            &
        !                 chg_typ, chg_val, absmin, absmax)
        
         case ("flood_sedfrac")
            sd_ch(ielem)%chseq = chg_par(sd_ch(ielem)%chseq,              &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("d50")
            sd_ch(ielem)%d50 = chg_par(sd_ch(ielem)%d50,  &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("ch_clay")
            sd_ch(ielem)%ch_clay = chg_par(sd_ch(ielem)%ch_clay,            &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("carbon")
            sd_ch(ielem)%carbon = chg_par(sd_ch(ielem)%carbon,            &
                         chg_typ, chg_val, absmin, absmax)     
 
         case ("ch_bd")
            sd_ch(ielem)%ch_bd = chg_par(sd_ch(ielem)%ch_bd, chg_typ, chg_val, absmin, absmax)
        
         case ("chss")
            sd_ch(ielem)%chss = chg_par(sd_ch(ielem)%chss, chg_typ, chg_val, absmin, absmax)
        
         case ("bankfull_flo")
            sd_ch(ielem)%bankfull_flo = chg_par(sd_ch(ielem)%bankfull_flo,      &
                         chg_typ, chg_val, absmin, absmax)
            
         case ("fps")
            sd_ch(ielem)%fps = chg_par(sd_ch(ielem)%fps, chg_typ, chg_val, absmin, absmax)
        
         case ("fpn")
            sd_ch(ielem)%fpn = chg_par(sd_ch(ielem)%fpn, chg_typ, chg_val, absmin, absmax)

         case ("hc_kh")
            sd_ch(ielem)%hc_kh = chg_par(sd_ch(ielem)%hc_kh,            &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("hc_hgt")
            sd_ch(ielem)%hc_hgt = chg_par(sd_ch(ielem)%hc_hgt,            &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("hc_ini")
            sd_ch(ielem)%hc_ini = chg_par(sd_ch(ielem)%hc_ini,            &
                         chg_typ, chg_val, absmin, absmax)
         case ("ch_n_conc")
             sd_ch(ielem)%n_conc = chg_par (sd_ch(ielem)%n_conc, chg_typ, chg_val, absmin, absmax)
          
        case ("ch_p_conc")
             sd_ch(ielem)%p_conc = chg_par (sd_ch(ielem)%p_conc, chg_typ, chg_val, absmin, absmax)
          
        case ("ch_p_bio")
             sd_ch(ielem)%p_bio = chg_par (sd_ch(ielem)%p_bio, chg_typ, chg_val, absmin, absmax)
            
      !!RES
         case ("esa")
           res_ob(ielem)%esa = chg_par(res_ob(ielem)%esa,             &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("evol")
           res_ob(ielem)%evol = chg_par(res_ob(ielem)%evol,           &
                         chg_typ, chg_val, absmin, absmax) 
        
         case ("psa")
           res_ob(ielem)%psa = chg_par(res_ob(ielem)%psa,             &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("pvol")
           res_ob(ielem)%pvol = chg_par(res_ob(ielem)%pvol,           &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("nsed")
           res_prm(ielem)%sed%nsed = chg_par(res_prm(ielem)%sed%nsed,           &
                         chg_typ, chg_val, absmin, absmax)
           
         case ("sed_stlr")
           res_prm(ielem)%sed%sed_stlr = chg_par(res_prm(ielem)%sed%sed_stlr,           &
                         chg_typ, chg_val, absmin, absmax)
           
         case ("velsetlr")
           res_prm(ielem)%sed%velsetlr = chg_par(res_prm(ielem)%sed%velsetlr,           &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("k_res")
           res_hyd(ielem)%k = chg_par(res_hyd(ielem)%k,                 &
                         chg_typ, chg_val, absmin, absmax)

         case ("evrsv")
            res_hyd(ielem)%evrsv = chg_par(res_hyd(ielem)%evrsv,        &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("vol")
            res(ielem)%flo = chg_par(res(ielem)%flo,                    &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("sed")
            res(ielem)%sed = chg_par(res(ielem)%sed,                    &
                         chg_typ, chg_val, absmin, absmax)

         case ("orgp")
            res(ielem)%sedp = chg_par(res(ielem)%sedp,                  &
                          chg_typ, chg_val, absmin, absmax)
       
         case ("orgn")
            res(ielem)%orgn = chg_par(res(ielem)%orgn,                  &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("solp")
            res(ielem)%solp = chg_par(res(ielem)%solp,                  &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("no3")
            res(ielem)%no3 = chg_par(res(ielem)%no3,                    &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("nh3")
            res(ielem)%nh3 = chg_par(res(ielem)%nh3,                    &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("no2")
            res(ielem)%no2 = chg_par(res(ielem)%no2,                    &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("psetlr1")
            res_prm(ielem)%nut%psetlr1 = chg_par(res_prm(ielem)%nut%psetlr1,    &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("psetlr2")
            res_prm(ielem)%nut%psetlr2 = chg_par(res_prm(ielem)%nut%psetlr2,    &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("nsetlr1")
            res_prm(ielem)%nut%nsetlr1 = chg_par(res_prm(ielem)%nut%nsetlr1,    &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("nsetlr2")
            res_prm(ielem)%nut%nsetlr2 = chg_par(res_prm(ielem)%nut%nsetlr2,    &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("chlar")
            res_prm(ielem)%nut%chlar = chg_par(res_prm(ielem)%nut%chlar,        &
                         chg_typ, chg_val, absmin, absmax)
        
         case ("seccir")
            res_prm(ielem)%nut%seccir = chg_par(res_prm(ielem)%nut%seccir,      &
                         chg_typ, chg_val, absmin, absmax)
            
      !! res decision tables
        case ("drawdown_days")
          if (ly <= dtbl_res(ielem)%acts)then
            dtbl_res(ielem)%act(ly)%const = chg_par(dtbl_res(ielem)%act(ly)%const,      &
                         chg_typ, chg_val, absmin, absmax)
          end if
        case ("withdraw_rate")
          if (ly <= dtbl_res(ielem)%acts)then
            dtbl_res(ielem)%act(ly)%const2 = chg_par(dtbl_res(ielem)%act(ly)%const2,    &
                         chg_typ, chg_val, absmin, absmax)
          end if
            
      !!AQU
         case ("flo_init_mm")
            aqu_dat(ielem)%flo = chg_par(aqu_dat(ielem)%flo,                &
                         chg_typ, chg_val, absmin, absmax)
         case ("dep_bot")
            aqu_dat(ielem)%dep_bot = chg_par(aqu_dat(ielem)%dep_bot,        &
                         chg_typ, chg_val, absmin, absmax)

         case ("dep_wt_init")
            aqu_dat(ielem)%dep_wt = chg_par(aqu_dat(ielem)%dep_wt,          &
                         chg_typ, chg_val, absmin, absmax)

         case ("no3_init")
            aqu_dat(ielem)%no3 = chg_par(aqu_dat(ielem)%no3,                &
                         chg_typ, chg_val, absmin, absmax)
            
         case ("minp_init")
            aqu_dat(ielem)%minp = chg_par(aqu_dat(ielem)%minp,              &
                         chg_typ, chg_val, absmin, absmax)
               
         case ("cbn_init")
            aqu_dat(ielem)%cbn = chg_par(aqu_dat(ielem)%cbn,                &
                         chg_typ, chg_val, absmin, absmax)
                           
         case ("flo_dist")
            aqu_dat(ielem)%flo_dist = chg_par(aqu_dat(ielem)%flo_dist,      &
                         chg_typ, chg_val, absmin, absmax)
         
         case ("bf_max")
            aqu_dat(ielem)%bf_max = chg_par(aqu_dat(ielem)%bf_max,          &
                         chg_typ, chg_val, absmin, absmax)
   
         case ("alpha")
            aqu_dat(ielem)%alpha = chg_par(aqu_dat(ielem)%alpha,            &
                         chg_typ, chg_val, absmin, absmax)
            aqu_prm(ielem)%alpha_e = Exp(-aqu_dat(ielem)%alpha)

         case ("revap_co")
            aqu_dat(ielem)%revap_co = chg_par(aqu_dat(ielem)%revap_co,      &
                         chg_typ, chg_val, absmin, absmax)
                  
         case ("deep_seep")
            aqu_dat(ielem)%seep = chg_par(aqu_dat(ielem)%seep,              &
                         chg_typ, chg_val, absmin, absmax)
            
         case ("sp_yld")
            aqu_dat(ielem)%spyld = chg_par(aqu_dat(ielem)%spyld,            &
                         chg_typ, chg_val, absmin, absmax)
            aqu_d(ielem)%stor = 1000. * (aqu_dat(ielem)%dep_bot - aqu_d(ielem)%dep_wt) * aqu_dat(ielem)%spyld
                    
         case ("hlife_n")
            aqu_dat(ielem)%hlife_n = chg_par(aqu_dat(ielem)%hlife_n,        &
                         chg_typ, chg_val, absmin, absmax)
            aqu_prm(ielem)%nloss = Exp(-.693 / (aqu_dat(ielem)%hlife_n + .1))

         case ("flo_min")
            aqu_dat(ielem)%flo_min = chg_par(aqu_dat(ielem)%flo_min,        &
                         chg_typ, chg_val, absmin, absmax)
 
         case ("revap_min")
            aqu_dat(ielem)%revap_min = chg_par(aqu_dat(ielem)%revap_min,    &
                         chg_typ, chg_val, absmin, absmax)
               
      !!LTE
         case ("cn2_lte")
            hlt_db(ielem)%cn2 = chg_par (hlt_db(ielem)%cn2, chg_typ, chg_val, absmin, absmax)
            
         case ("awc_lte")
            c_val = chg_val * hlt(ielem)%soildep
            abmax = absmax * hlt(ielem)%soildep
            hlt(ielem)%awc = chg_par (hlt(ielem)%awc, chg_typ, c_val, absmin, abmax)
            
         case ("etco_lte")
            hlt_db(ielem)%etco = chg_par (hlt_db(ielem)%etco, chg_typ, chg_val, absmin, absmax)
                       
         case ("tc_lte")
            hlt_db(ielem)%tc = chg_par (hlt_db(ielem)%tc, chg_typ, chg_val, absmin, absmax)
            
         case ("soildep_lte")
            hlt_db(ielem)%soildep = chg_par (hlt_db(ielem)%soildep, chg_typ, chg_val, absmin, absmax)  
        
         case ("slope_lte")
            hlt_db(ielem)%slope = chg_par (hlt_db(ielem)%slope, chg_typ, chg_val, absmin, absmax)
            
         case ("slopelen_lte")
            hlt_db(ielem)%slopelen = chg_par (hlt_db(ielem)%slopelen, chg_typ, chg_val, absmin, absmax)
        
         case ("sy_lte")
            hlt_db(ielem)%sy = chg_par (hlt_db(ielem)%sy, chg_typ, chg_val, absmin, absmax)
            
         case ("abf_lte")
            hlt_db(ielem)%abf = chg_par (hlt_db(ielem)%abf, chg_typ, chg_val, absmin, absmax)
            
         case ("revapc_lte")
            hlt_db(ielem)%revapc = chg_par (hlt_db(ielem)%revapc, chg_typ, chg_val, absmin, absmax)
            
         case ("percc_lte")
            hlt_db(ielem)%percc = chg_par (hlt_db(ielem)%percc, chg_typ, chg_val, absmin, absmax)
            
         case ("sw_lte")
            hlt_db(ielem)%sw = chg_par (hlt_db(ielem)%sw, chg_typ, chg_val, absmin, absmax)
            
         case ("gw_lte")
            hlt_db(ielem)%gw = chg_par (hlt_db(ielem)%gw, chg_typ, chg_val, absmin, absmax)
            
         case ("gwflow_lte")
            hlt_db(ielem)%gwflow = chg_par (hlt_db(ielem)%gwflow, chg_typ, chg_val, absmin, absmax)
            
         case ("gwdeep_lte")
            hlt_db(ielem)%gwdeep = chg_par (hlt_db(ielem)%gwdeep, chg_typ, chg_val, absmin, absmax)
            
        case ("snow_lte")
            hlt_db(ielem)%snow = chg_par (hlt_db(ielem)%snow, chg_typ, chg_val, absmin, absmax)
            
        case ("uslek_lte")
            hlt_db(ielem)%uslek = chg_par (hlt_db(ielem)%uslek, chg_typ, chg_val, absmin, absmax)
            
        case ("uslec_lte")
            hlt_db(ielem)%uslec = chg_par (hlt_db(ielem)%uslec, chg_typ, chg_val, absmin, absmax)
            
        case ("uslep_lte")
            hlt_db(ielem)%uslep = chg_par (hlt_db(ielem)%uslep, chg_typ, chg_val, absmin, absmax)
            
        case ("uslels_lte")
            hlt_db(ielem)%uslels = chg_par (hlt_db(ielem)%uslels, chg_typ, chg_val, absmin, absmax)

        !! initial soil properties
        case ("lab_p")
          do jj = 1, soil(ielem)%nly
            soil1(ielem)%mp(ly)%lab = chg_par (soil1(ielem)%mp(ly)%lab, chg_typ, chg_val, absmin, absmax)
          end do
          
        case ("hum_c_n")
          do jj = 1, soil(ielem)%nly
            soil1(ielem)%hact(ly)%n = chg_par (soil1(ielem)%hact(ly)%n, chg_typ, chg_val, absmin, absmax)
          end do
          
        case ("hum_c_p")
          do jj = 1, soil(ielem)%nly
            soil1(ielem)%hact(ly)%p = chg_par (soil1(ielem)%hact(ly)%p, chg_typ, chg_val, absmin, absmax)
          end do
          
      end select

      return
      end subroutine cal_parm_select