!! SWAT+GL Subroutine 
!! Read in glacier information & glacierized area of all subbasins
       subroutine sp_read_glcontrol
       use basin_module
       real:: tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9
       real:: test1, test2, test3, test4, test5, test6, test7, test8
       integer:: ierr, cnt
              
        !! ===============================================================================================================================
        !! a) Read in SWAT+GL Codes File
        open(newunit=IU1, file='swatgl_codes.gl',status='old',action='read') 
        read(iu1,'(A)') sglcontrol
        ierr=0
        cnt=0
        do while (ierr==0)
            read(iu1, *, iostat=ierr) tmp1, tmp2, tmp3, tmp4, tmp5, tmp6
        cnt = cnt + 1
        end do
        cnt = cnt - 1
        
        sm_model = 0
        ros_flag = 0
        gm_init = 0
        fgmlt_cor = 0
        pmix_flag = 0
        acc_mod = 0

        rewind(iu1)
        read(iu1,'(A)') titldum
        do i=1, cnt       
            read(iu1, *, iostat=ierr) sm_model, ros_flag, gm_init, fgmlt_cor, pmix_flag, acc_mod   
        end do
        
        !! Allocate         
        allocate(glcode(cnt))
        
        glcode%sm_model = sm_model
        glcode%ros_flag = ros_flag
        glcode%gm_init = gm_init
        glcode%fgmlt_cor = fgmlt_cor
        glcode%pmix_flag = pmix_flag
        glcode%acc_mod = acc_mod
        
        !! ===============================================================================================================================
        !! b) Read in SWAT+GL Parameters File
        
        open(newunit=IU1, file='swatgl_parameters.gl',status='old',action='read') 
        read(iu1,'(A)') sglpars
        ierr=0
        cnt=0
        do while (ierr==0)
            read(iu1, *, iostat=ierr) tmp7, tmp8, tmp9
        cnt = cnt + 1
        end do
        cnt = cnt - 1
        
        tmix_ul = 0.
        pfac = 0.
        pthr = 0.
        rewind(iu1)
        read(iu1,'(A)') titldum
        do i=1, cnt       
            read(iu1, *, iostat=ierr) tmix, pfac, pthr
        end do
        
        !! Allocate         
        allocate(glpars(cnt))

        glpars%tmix_ul = tmix_ul
        glpars%pfac = pfac
        glpars%pthr = pthr

        !glpars%exp_fac = exp_fac
      
    return
    end