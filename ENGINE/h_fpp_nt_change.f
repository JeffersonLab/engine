      subroutine h_fpp_nt_change(ABORT,err)
*----------------------------------------------------------------------
*
*     closes one HMS FPP Ntuple file and opens another
*
*     Purpose : switching from one file to the next
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*----------------------------------------------------------------------

      implicit none
      save

      character*15 here
      parameter (here='h_fpp_nt_change')

      logical ABORT
      character*(*) err

      INCLUDE 'h_fpp_ntuple.cmn'
      include 'gen_run_info.cmn'

      character*1 ifile
      character*80 file
      character*1000 pat

      integer*4 ilo,fn_len
      
*     functions
      integer g_important_length

*--------------------------------------------------------


      call h_fpp_nt_close(ABORT,err)


      
      if (h_fpp_nt_exists) then
        ABORT = .true.
      endif

      call NO_nulls(h_fpp_nt_file)     !replace null characters with blanks

      file= h_fpp_nt_file
      call NO_nulls(file)     !replace null characters with blanks
      call g_sub_run_number(file,gen_run_number)
      
      h_fpp_nt_filesegments = h_fpp_nt_filesegments + 1

      if (h_fpp_nt_filesegments .le. 9) then
        ifile = char(ichar('0')+h_fpp_nt_filesegments)
      else
        ifile = char(ichar('a')+h_fpp_nt_filesegments-10)
      endif
 
      fn_len = g_important_length(file)
      ilo=index(file,'.hbook')
      if ((ilo.le.1).or.(ilo.gt.fn_len-5)) then
        ilo=index(file,'.rzdat')
      endif

      if ((ilo.gt.1).and.(ilo.lt.fn_len)) then
        file = file(1:ilo-1) // '.' // ifile // file(ilo:fn_len)
      else
        ABORT = .true.
      endif



      IF (.not.ABORT) call h_fpp_nt_open(file,ABORT,err)


      IF(ABORT) THEN
        err= ':unable to change HMS FPP Ntuple file segment'
        call G_add_path(here,err)
      ELSE
        pat= ':changed HMS FPP Ntuple file segment'
        call G_add_path(here,pat)
        call G_log_message('INFO: '//pat)
      ENDIF


      RETURN
      END  
