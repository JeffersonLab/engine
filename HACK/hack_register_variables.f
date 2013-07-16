      subroutine hack_register_variables(ABORT,err)
*
* ----------------------------------------------------------------------
*--  file hack_register_variables.f
*--  Initialization for User Develpment Code
* $Log: hack_register_variables.f,v $
* Revision 1.4  1995/10/11 14:12:40  cdaq
* (SAW) Call makereg generated r_hack_ instead of making explicit reg calls
*
* Revision 1.3  1995/08/09 18:49:38  cdaq
* (JRA) Comment out obnoxious print statement
*
* Revision 1.2  1995/07/28  14:22:29  cdaq
* (SAW) Change type to print for f2c compatibility
*
* Revision 1.1  1994/07/25  18:03:44  cdaq
* Initial revision
* ----------------------------------------------------------------------
      implicit none
      save
      logical ABORT
      character*(*) err

* ----------------------------------------------------------------------

      ABORT = .FALSE.
      err = ' '

      call r_hack_

      return
      end
