c ########################################################################################
      subroutine hsv_init(history, cm_all)
c
      !implicit none
c
      real*8, intent(inout) :: history(*)
      real(kind=8), dimension(2,*), intent(in) :: cm_all      
c
      history(1) = cm_get_pair('hsv_version_____',cm_all)   
      !call cm_get_pair_SR( 'hsv_version_____', cm_all, history(1) )   
c
      return
      end subroutine
   