      subroutine h_fpp_nt_init(ABORT,err)
*----------------------------------------------------------------------
*
*     Creates an HMS FPP Ntuple
*
*     Purpose : Books an HMS FPP Ntuple; defines structure of it
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*13 here
      parameter (here='h_fpp_nt_init')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'h_fpp_ntuple.cmn'
      INCLUDE 'gen_routines.dec'
      include 'hms_data_structures.cmn'
      include 'gen_run_info.cmn'
*
      character*80 default_name
      parameter (default_name= 'hFPPntuple')
c
      character*80 file
      character*80 name
      character*1000 pat,msg
      integerilo,fn_len,m
      character*1 ifile

      INCLUDE 'h_fpp_ntuple.dte'
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      IF(h_fpp_nt_exists) THEN
        call h_fpp_nt_shutdown(ABORT,err)
        If(ABORT) Then
          call G_add_path(here,err)
          RETURN
        EndIf
      ENDIF
*
      call NO_nulls(h_fpp_nt_file)     !replace null characters with blanks
*
*-if name blank, just forget it
      IF(h_fpp_nt_file.EQ.' ') RETURN   !do nothing

      h_fpp_nt_ID= default_h_fpp_nt_ID
      h_fpp_nt_name= default_name

      IF(h_fpp_nt_title.EQ.' ') THEN
        msg= name//' '//h_fpp_nt_file
        call only_one_blank(msg)
        h_fpp_nt_title= msg
      ENDIF

      file= h_fpp_nt_file
      call g_sub_run_number(file,gen_run_number)

*     * only needed if using more than one file      
      if (HFPP_nt_max_segmentevents .gt. 0) then
         h_fpp_nt_filesegments = 1

         ifile = char(ichar('0')+h_fpp_nt_filesegments)
 
         fn_len = g_important_length(file)
         ilo=index(file,'.hbook')
         if ((ilo.le.1).or.(ilo.gt.fn_len-5)) then
           ilo=index(file,'.rzdat')
         endif  

         if ((ilo.gt.1).and.(ilo.lt.fn_len)) then
           file = file(1:ilo-1) // '.' // ifile // file(ilo:fn_len)
         else
           ABORT = .true.
          RETURN
         endif
         write(*,*) ' Using segmented hms FPP rzdat files first filename: ',file
      else
         write(*,*) ' Not using segmented hms FPP rzdat files.'  
      endif

      call h_fpp_nt_open(file,ABORT,err)      

      IF(ABORT) THEN
        err= ':unable to create HMS FPP Ntuple'
        call G_add_path(here,err)
      ELSE
        pat= ':created HMS FPP Ntuple'
        call G_add_path(here,pat)
        call G_log_message('INFO: '//pat)
      ENDIF

      RETURN
      END  
