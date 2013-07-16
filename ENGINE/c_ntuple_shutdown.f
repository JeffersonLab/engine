      subroutine c_Ntuple_shutdown(ABORT,err)
*----------------------------------------------------------------------
*
*     Final shutdown of the COIN Ntuple
*
*     Purpose : Flushes and closes the COIN Ntuple
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 8-Apr-1994  K.B.Beard, HU: added Ntuples
* $Log: c_ntuple_shutdown.f,v $
* Revision 1.5  2004/02/17 17:26:34  jones
* Changes to enable possiblity of segmenting rzdat files
*
* Revision 1.4  1998/12/01 15:33:33  saw
* (SAW) Clean out archaic g_build_note stuff
*
* Revision 1.3  1994/06/29 03:24:56  cdaq
* (KBB) Remove HDELET call
*
* Revision 1.2  1994/06/17  03:00:30  cdaq
* (KBB) Upgrade
*
* Revision 1.1  1994/04/12  16:14:33  cdaq
* Initial revision
*
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*17 here
      parameter (here='c_Ntuple_shutdown')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'c_ntuple.cmn'
      INCLUDE 'gen_routines.dec'
*
      logical FAIL
      character*80 why,directory,name
      character*1000 msg
      integer io,id,cycle,m
*
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*

      IF(.NOT.c_Ntuple_exists) RETURN       !nothing to do
c

      call c_ntuple_close(ABORT,err)

*
      IF(c_Ntuple_exists) then
         ABORT = .true.
      endif
      c_Ntuple_ID= 0
      c_Ntuple_name= ' '
      c_Ntuple_file= ' '
      c_Ntuple_title= ' '
      c_Ntuple_size= 0
      do m=1,CMAX_Ntuple_size
        c_Ntuple_tag(m)= ' '
        c_Ntuple_contents(m)= 0.
      enddo
*
      IF(ABORT) call G_add_path(here,err)
*
      RETURN
      END      
