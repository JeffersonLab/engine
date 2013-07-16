      subroutine h_Ntuple_shutdown(ABORT,err)
*----------------------------------------------------------------------
*
*     Final shutdown of the HMS Ntuple
*
*     Purpose : Flushes and closes the HMS Ntuple
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 8-Apr-1994  K.B.Beard, HU: added Ntuples
* $Log: h_ntuple_shutdown.f,v $
* Revision 1.6  2004/02/17 17:26:34  jones
* Changes to enable possiblity of segmenting rzdat files
*
* Revision 1.5  1998/12/01 15:56:25  saw
* (SAW) Clean out archaic g_build_note stuff
*
* Revision 1.4  1996/01/16 17:01:06  cdaq
* (SAW) Comment out an info message
*
* Revision 1.3  1994/06/29 03:27:43  cdaq
* (KBB) Remove HDELET call
*
* Revision 1.2  1994/06/17  02:59:12  cdaq
* (KBB) Upgrade
*
* Revision 1.1  1994/04/12  16:15:43  cdaq
* Initial revision
*
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*17 here
      parameter (here='h_Ntuple_shutdown')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'h_ntuple.cmn'
      INCLUDE 'gen_routines.dec'
*
*
      logical FAIL
      character*80 why,directory,name
      character*1000 msg
      integer io,id,cycle,m
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*

      IF(.NOT.h_Ntuple_exists) RETURN       !nothing to do
c

      call h_ntuple_close(ABORT,err)

*
      IF(h_Ntuple_exists) then
         ABORT = .true.
      endif
      h_Ntuple_ID= 0
      h_Ntuple_name= ' '
      h_Ntuple_file= ' '
      h_Ntuple_title= ' '
      h_Ntuple_size= 0
      do m=1,HMAX_Ntuple_size
        h_Ntuple_tag(m)= ' '
        h_Ntuple_contents(m)= 0.
      enddo
*
      IF(ABORT) call G_add_path(here,err)
*
      RETURN
      END      
