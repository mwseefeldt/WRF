!WRF:MEDIATION_LAYER:ADT_BARRIER
!

! RASM (Juanxiong He): add flag to trigger radiation related processing
SUBROUTINE solve_interface ( grid, rasm_rad_fl ) 

   USE module_domain
   USE module_timing
   USE module_driver_constants
   USE module_configure
   USE module_wrf_error
#if( EM_CORE == 1 )
   USE module_trajectory, only : trajectory_driver
#endif

   IMPLICIT NONE

   INTERFACE
#if (EM_CORE == 1 && DA_CORE != 1)
# include  "solve_em.int"
#endif
   END INTERFACE

   TYPE(domain) , INTENT(INOUT)  :: grid
   TYPE (grid_config_rec_type)   :: config_flags

   ! RASM (Juanxiong He): flag to trigger radiation related processing
   LOGICAL, INTENT(INOUT), OPTIONAL              ::  rasm_rad_fl
   LOGICAL                                       ::  lrasm_rad_fl

   INTEGER     :: idum1, idum2
   CHARACTER(len=256) :: dbg_msg

   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
   CALL set_scalar_indices_from_config ( grid%id , idum1 , idum2 )

   ! RASM (Juanxiong He): flag to trigger radiation related processing
   lrasm_rad_fl = .false.
   IF (present(rasm_rad_fl)) THEN
      lrasm_rad_fl = rasm_rad_fl
   ENDIF

#if (EM_CORE == 1 && DA_CORE != 1)
   
#if ( WRFPLUS == 1 )
   IF      ( config_flags%dyn_opt == dyn_em_tl ) THEN
      CALL solve_em_tl  ( grid , config_flags  &
!
# include "actual_new_args.inc"
!
               )
   ELSE IF ( config_flags%dyn_opt == dyn_em_ad ) THEN
      CALL solve_em_ad  ( grid , config_flags  &
!
# include "actual_new_args.inc"
!
               )
   ELSE
#endif
      ! RASM (Juanxiong He): add flag to trigger radiation related processing
      CALL solve_em  ( grid , config_flags, lrasm_rad_fl  &
!
# include "actual_new_args.inc"
!
               )
#if ( WRFPLUS == 1 )
   END IF
#endif

# if ( WRF_CHEM == 1 )
   IF ( config_flags%chem_opt > 0 .or. config_flags%tracer_opt > 0 ) THEN

     CALL chem_driver  ( grid , config_flags  &
!
# include "actual_new_args.inc"
!
               )
   ENDIF
# endif
#endif

# if ( EM_CORE == 1 && WRFPLUS != 1 )
   if( grid%traj_opt /= no_trajectory ) then
     write(dbg_msg,'(''solve_xface('',i2.2,''): Before call to trajectory_driver'')') grid%id
     call wrf_debug( 200,trim(dbg_msg) )
     call trajectory_driver( grid )
     write(dbg_msg,'(''solve_xface('',i2.2,''): After  call to trajectory_driver'')') grid%id
     call wrf_debug( 200,trim(dbg_msg) )
   endif
# endif

END SUBROUTINE solve_interface

