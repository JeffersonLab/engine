      subroutine g_sub_run_number(string,number)
*
* $Log: g_sub_run_number.f,v $
* Revision 1.4  1996/09/05 21:06:54  saw
* (SAW) fix a bug
*
* Revision 1.3  1996/01/17 19:26:27  cdaq
* (SAW) Fix so it works on more platforms
*
* Revision 1.2  1995/07/20 19:08:00  cdaq
* (SAW) Build result into temporary string, then copy back to string.
*
* Revision 1.1  1995/07/14  16:31:30  cdaq
* Initial revision
*
      implicit none
      character*(*) string
      integer number
*
*     If a %d is found in the string, replace it with the number.
*
*     We should probably return an error if the new string wont fit.  For
*     now we just don't do the substitution.
*
*     This is not very fancy, it  should really do something like replace
*     <runnumber> by the run number, but replace %d by the run number is
*     what run control does for log file names, so we do the same as CODA
*     for now.
*
*
      integer iper
      character*10 snum                 ! String to hold the run number
      character*132 stemp
      integer inum,reallen
      
      iper = index(string,'%')

      if(iper.eq.0) return

      if(string(iper+1:iper+1).ne.'d') return ! %d not found

      write(snum,'(i10)') number
      inum = 1
      do while(snum(inum:inum).eq.' ')
        inum = inum + 1
      enddo

      reallen = iper+2
      do while(string(reallen:reallen).ne.' ')
        reallen = reallen + 1
      enddo
      reallen = reallen-1
      
      if(reallen+(10-inum+1)-2.gt.len(string)) return ! Would be too long
      if(reallen+(10-inum+1)-2.gt.len(stemp)) return ! Would be too long
      if(iper+2.gt.reallen) then        ! Line ends with %d
        stemp = string(1:iper-1)//snum(inum:10)
      else if (iper.eq.1) then
        stemp = snum(inum:10)//string(iper+2:reallen)
      else
        stemp = string(1:iper-1)//snum(inum:10)//string(iper+2:reallen)
      endif
      string = stemp

c      print *,number
c      print *,snum
c      print *,string

      return
      end

      

