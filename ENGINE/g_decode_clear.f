      subroutine g_decode_clear(ABORT, error)
*
*     Purpose and Methods:
*
*     Clear the decoding maps.
*
*     Inputs:
*
*     None
*
*     Outputs:
*
*     ABORT
*     error
*
*-
*-     by Steve Wood
*-      modified by Kevin Beard Dec.3,1993
*-    $Log$
*-    Revision 1.2  1994/06/18 02:46:58  cdaq
*-    (SAW) Add code for miscleaneous data and uninstrumented channels
*-
* Revision 1.1  1994/02/04  21:49:17  cdaq
* Initial revision
*
*-
      implicit none
      SAVE
      logical ABORT
      character*(*) error
      integer roc,slot,i
*
      include 'gen_decode_common.cmn'
      include 'gen_detectorids.par'
*
*     Clear out slotpointer and subaddcount and mask arrays.
*
      do roc=0, G_DECODE_MAXROCS-1
         do slot=1, G_DECODE_MAXSLOTS
            g_decode_slotpointer(roc+1,slot) = -1 ! Uninstrumented SLOT
            g_decode_subaddcnt(roc+1,slot) = 16 ! Dflt for "uninstrmntd" slots
            g_decode_slotmask(roc+1,slot) = 'FFF'x         ! Default mask
         enddo
      enddo
*
      do i=1,G_DECODE_MAXWORDS          ! Clear out the Detector ID map
         g_decode_didmap(i) = UNINST_ID  ! with the uninstrumented ID
      enddo
*
      g_decode_nextpointer = 1
      ABORT= .FALSE.
      error= ' '
      return
      end
