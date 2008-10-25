      subroutine sem_analyze_pedestal(ABORT,err)
      implicit none
      save

      INCLUDE 'sem_data_structures.cmn'

      logical ABORT
      character*(*) err
      character*18 here
      parameter (here='sem_analyze_pedestal')
*
      integer*4 ihit,counter,plane

      logical atleastone




      integer PLANE_TBPM
      parameter (PLANE_TBPM=2)
      
*-------------------------------------------------------

      atleastone = .false.
 
      do ihit = 1 , N_TBPM_TOT_HITS

        plane   = N_TBPM_ADDR1(ihit)
        counter = N_TBPM_ADDR2(ihit)

        if (plane .ne. PLANE_TBPM) then
          write(6,*) ' !!!!! bad SEM plane ID=',plane,
     >       ' !!!!   (should be=',PLANE_TBPM,')   counter=',counter

        elseif (counter .gt. num_tbpm) then
          write(6,*) ' !!!!! bad SEM counter ID=,counter,
     >         ' !!!!   (should not exceed ', num_tbpm, ')'

        else
          atleastone = .true.
          ndet_ped_tbpm_sum2(counter) = ndet_ped_tbpm_sum2(counter) +
     &         N_TBPM_RAW_DATA(counter)* N_TBPM_RAW_DATA(counter)
          ndet_ped_tbpm_sum(counter) = ndet_ped_tbpm_sum(counter) +
     &         N_TBPM_RAW_DATA(counter)

        endif

      enddo

      if (atleastone) ndet_tbpm_ped_counts = ndet_tbpm_ped_counts + 1
      if(ABORT) then
         call G_add_path(here,err)
         return
      endif
*

      end

