*
* ----------------------------------------------------------------------
*--  file hack_register_variables.f
*--  Initialization for User Develpment Code
* $Log$
* Revision 1.1  1994/07/25 18:03:44  cdaq
* Initial revision
*
*
      subroutine hack_register_variables(ABORT,err)
*
* ----------------------------------------------------------------------
*-- first declare variables and store them in a common block (extra file)
      implicit none
      logical ABORT
      character*(*) err
      include 'gen_data_structures.cmn'
      include 'hack_.cmn'
**      include 'hack_u.cmn'
**      include 'hack_collect.cmn'
*
      integer size
*EXAMP      integer i
*
*-- >>>>>>>>>> insert additional user declarations here <<<<<<<<<
* ----------------------------------------------------------------------
      ABORT = .FALSE.
      err = ' '
* ----------------------------------------------------------------------
*-- second register the variables declared above.
*
      type *,'HACKING_INIT: Registering Event variables and Parameters'
*
      call regparmint('hack_enable',hack_enable,0)     !turn on/off User D.code
      call regeventintarray
     & ('hack_eventint',hack_eventint,max_user_par,0)  !User Development array
      call regeventrealarray
     & ('hack_eventreal',hack_eventreal,max_user_par,0)!User Development array
      call regparmrealarray
     & ('hack_parmreal',hack_parmreal,max_user_par,0)  !User Development array
*
      size = 16*4
      call regeventintarray
     & ('hack_hmssc_au',hack_hmssc_au,size,0) !HMS Scintillator data
      call regeventintarray
     & ('hack_hmssc_ad',hack_hmssc_ad,size,0) !HMS Scintillator data
      call regeventintarray
     & ('hack_hmssc_tu',hack_hmssc_tu,size,0) !HMS Scintillator data
      call regeventintarray
     & ('hack_hmssc_td',hack_hmssc_td,size,0) !HMS Scintillator data
      call regeventintarray
     & ('hack_hmssc_go',hack_hmssc_go,size,0) !HMS Scintillator data
*
**      call regparmint('hack_col_ic',hack_col_ic,0)     !identify centroids to get
*
**      call regeventintarray
**     & ('meantime',meantime,size,0) !HMS Scintillator time between planes
**      call regeventintarray
**     & ('diftime',diftime,size,0) !HMS Scintillator time between planes
**      call regeventintarray
**     & ('hack_time',hack_time,4,0) !HMS Scintillator time between planes
**      call regeventintarray
**     & ('hack_chan',hack_chan,4,0)
**      call regeventintarray
**     & ('hack_hits',hack_hits,4,0) 
**      call regeventint('dif_t_x1y1',dif_t_x1y1,0)
**      call regeventint('dif_t_x1x2',dif_t_x1x2,0)
**      call regeventint('dif_t_x1y2',dif_t_x1y2,0)
**      call regeventint('dif_t_y1x2',dif_t_y1x2,0)
**      call regeventint('dif_t_y1y2',dif_t_y1y2,0)
**      call regeventint('dif_t_x2y2',dif_t_x2y2,0)
*-- >>>>>>>>>> insert additional user code here <<<<<<<<<
*EXAMP  Examples of register calls for parameters and event specific variables
*EXAMP
*EXAMP	call regparmint('iparm_const',iparm_const,0)
*EXAMP	call regparmreal('rparm_const',rparm_const,0)
*EXAMP	call regtestintarray('t',t,100,'Test result array')
*
      return
      end
