      subroutine g_analyze_scalers(event,ABORT,err)
*
* $Log$
* Revision 1.5  1995/07/27 19:04:54  cdaq
* (SAW) Use specific bit manipulation routines for f2c compatibility
*
* Revision 1.4  1995/04/06  20:04:33  cdaq
* (JRA) Handle overflows and save them in real variables
*
* Revision 1.3  1994/07/07  15:24:24  cdaq
* (SAW) Fix bugs
*
c Revision 1.2  1994/07/07  15:23:16  cdaq
c (SAW) Correct pointers for actual bank structure
c
c Revision 1.1  1994/06/22  21:02:17  cdaq
c Initial revision
c
*
      implicit none
      save
      integer*4 event(*)
*
      character*17 here
      parameter (here='g_analyze_scalers')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'gen_scalers.cmn'
*
      integer nroll(max_num_scalers)
      integer index
      real*4 realscal
*
      integer*4 jiand, jishft           ! Declare to help f2c
*
*     Scaler events have a header in from of each scaler.  High 16 bits
*     will contain the address (the switch settings).  Address for hall C
*     will be of the form DANN, where NN is the scaler number.  The low 16
*     bits will contain the number of scaler values to follow (this should
*     be no larger than 16, but we will allow more.)
*
*
*     NOTE that the variables scalers(i) is REAL!!!!!
*     this is so that we can record the correct value when the 
*     hardware scalers (32 bit <> I*4) overflow.
*

      integer evtype, evlen, pointer
      integer scalid, countinmod, address, counter
      
*
      evtype = jishft(event(2),-16)
*
*     Should check against list of known scaler events
*
      evlen = event(1) + 1
      if(evlen.gt.3) then           ! We have a scaler bank
         pointer = 3
*
         do while(pointer.lt.evlen)
*
            scalid = jiand(jishft(event(pointer),-16),'FF'x)
            countinmod = jiand(event(pointer),'FFFF'x)
*
*     Might want to check that count is not to big.
*
            if(countinmod.ne.16) then
               err = 'Scaler module header word has count<>16'
               ABORT = .true.
               call g_add_path(here,err)
               return                   ! Safest action
            endif
*
            address = scalid*16
            do counter = 1,countinmod
              index=address+counter
              realscal=float(event(pointer+counter))
              if (realscal.lt.-0.5) then
c                  type *,realscal
                  realscal=realscal+4294967296.
c                  type *,realscal
              endif
              if ( (realscal+float(nroll(index))*4294967296.) .ge.
     &              scalers(index) ) then       ! 2**32 = 4.295e+9
                scalers(index) = realscal + nroll(index)*4294967296.
              else                              !32 bit scaler rolled over.
                nroll(index)=nroll(index)+1
                scalers(index) = realscal + nroll(index)*4294967296.
              endif
            enddo
            pointer = pointer + countinmod + 1 ! Add 17 to pointer
         enddo
      else
         err = 'Event not big enough to contain scalers'
         ABORT = .true.
         call g_add_path(here,err)
         return
      endif

      return
      end

