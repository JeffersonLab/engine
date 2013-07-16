      subroutine s_Ntuple_shutdown(ABORT,err)
*----------------------------------------------------------------------
*
*     Final shutdown of the SOS Ntuple
*
*     Purpose : Flushes and closes the SOS Ntuple
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 8-Apr-1994  K.B.Beard, HU: added Ntuples
* $Log: s_ntuple_shutdown.f,v $
* Revision 1.6  2004/02/17 17:26:34  jones
* Changes to enable possiblity of segmenting rzdat files
*
* Revision 1.5  1998/12/01 16:02:39  saw
* (SAW) Clean out archaic g_build_note stuff
*
* Revision 1.4  1996/01/16 16:38:45  cdaq
* (SAW) Comment out an info message
*
* Revision 1.3  1994/06/29 03:30:25  cdaq
* (KBB) Remove HDELET call
*
* Revision 1.2  1994/06/17  02:57:45  cdaq
* (KBB) Upgrade
*
* Revision 1.1  1994/04/12  16:16:53  cdaq
* Initial revision
*
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*17 here
      parameter (here='s_Ntuple_shutdown')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 's_ntuple.cmn'
      INCLUDE 'gen_routines.dec'
*
*
      logical FAIL
      character*80 why,directory,name
      character*1000 msg
      integer io,id,cycle,m
*
      err= ' '
      ABORT = .FALSE.
*

      IF(.NOT.s_Ntuple_exists) RETURN       !nothing to do
c

      call s_ntuple_close(ABORT,err)

*
      IF(s_Ntuple_exists) then
         ABORT = .true.
      endif
      s_Ntuple_ID= 0
      s_Ntuple_name= ' '
      s_Ntuple_file= ' '
      s_Ntuple_title= ' '
      s_Ntuple_size= 0
      do m=1,SMAX_Ntuple_size
        s_Ntuple_tag(m)= ' '
        s_Ntuple_contents(m)= 0.
      enddo
*
      IF(ABORT) call G_add_path(here,err)
*--------------------------------------------------------
      RETURN
      END      
