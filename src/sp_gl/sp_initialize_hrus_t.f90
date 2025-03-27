!    subroutine initialize_hrus_t
!    !! Timo Initialize HRU's
!    !! Not activated yet
!    use parm
!    
!    integer::j,ii,cnt,ja,ib
!  
!    ! --------------------------------
!    !! Annual Volume HRU Redistribution 
!    !! Initialize Glacier HRUS (HRU Areas & GWEs based on different Scales such as Basin, Total Gl. A, Subbasin etc.)
!    cnt=0               
!    do ib=1,subtot ! Subbasin Loop
!    do ii = 1,hrutot(ib) ! HRU Loop (per Subbasin)
!        cnt=cnt+1       ! Total HRU Counter
!        if (hru_gl(cnt)==1) then ! If HRU glacierized
!        do ja = 1, numes ! Loop over ES in that Sub
!            if (hru_es_id(cnt)==ja) then ! If HRU belongs to current ES
!            if (es_gl_a2(ja,ib)==0) then ! IF ES is not glacierized anymore
!                test2=0 ! HRU/ES Fraction = 0
!            else                
!                test2=hru_km(ii)/es_gl_a2(ja,ib) !! HRU Fraction of ES (HRU/ES)                   
!            end if
!            ! HRU GWE on ES Scale
!            hru_gla(cnt)=es_gl_a2(ja,ib)*test2 !! Timo Glacier Area of GHRU based on HRU Fraction of ES
!            ! Option 1
!            !hru_glww(cnt)=es_gl_wm2(ja,ib)*test2 !! Timo Weighted GWE of GHRU based on HRU Fraction of ES
!            !hru_glww2(cnt)=es_gl_wm2(ja,ib) !! All HRUs of ES get same GWE of that ES 
!            ! Option 2
!            hru_glww(cnt)=es_gl_m2(ja,ib)*test2 !! Timo Weighted GWE of GHRU based on HRU Fraction of ES
!            hru_glww2(cnt)=es_gl_m2(ja,ib) !! All HRUs of ES get same GWE of that ES 
!
!            end if
!        end do
!        end if
!    end do
!    end do
!    
!    ! ------------------------------------------------   
!    return
!end