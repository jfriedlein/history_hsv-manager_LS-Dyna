c ########################################################################################
      subroutine hsv_set_scalar(scalar, nameX, history)
c
      !implicit none
c
      real*8, intent(in) :: scalar
      character(len=5), intent(in) :: nameX
      real*8, intent(inout) :: history(*)
      integer :: start_index
c
      start_index = hsv_get_sth(nameX, 'start_',history)

      if ( hsv_get_sth(nameX, 'length',history) /= 1 ) then
           write(*,*) "hsv_set_scalar<< Selected nameX (",nameX,
     &      " is not a scalar. Check for typos or choose a different
     &      subroutine that also handles this."
      endif
c
      history(start_index) = scalar
c
      return
      end subroutine
      
c ########################################################################################
      subroutine hsv_set_symTen2(symTen2, nameX, history)
c
      use Tensor
      !implicit none
c
      type(Tensor2), intent(in) :: symTen2
      character(len=5), intent(in) :: nameX
      real*8, intent(inout) :: history(*)
      integer :: start_index, end_index
c
      start_index = hsv_get_sth(nameX, 'start_',history)
      end_index =   hsv_get_sth(nameX, 'end__',history)

      if ( hsv_get_sth(nameX, 'length',history) /= 6 ) then
           write(*,*) "hsv_set_symTen2<< Selected nameX (",nameX,
     &      " is not a sym tensor. Check for typos or choose a different
     &      subroutine that also handles this."
      endif
c
      history(start_index:end_index) = asarray(voigt(symTen2),6)
c
      return
      end subroutine

c ########################################################################################
c This function is not needed. Usually we don't modify the given deformation gradient.
      subroutine hsv_set_unsymTen2(unsymTen2, nameX, history)
c
      use Tensor
      !implicit none
c
      type(Tensor2), intent(in) :: unsymTen2
      character(len=5), intent(in) :: nameX
      real*8, intent(inout) :: history(*)
      integer :: start_index, end_index
c
      start_index = hsv_get_sth(nameX, 'start_',history)
      end_index =   hsv_get_sth(nameX, 'end__',history)

      if ( hsv_get_sth(nameX, 'length',history) /= 9 ) then
           write(*,*) "hsv_get_symTen2<< Selected nameX (",nameX,
     &   " is not an unsym tensor. Check for typos or choose a different
     &   function that also handles this."
      endif
c
      history(start_index:end_index) =
     & unsymTensor_2_to_vec9_LSTC(unsymTen2)
c
      return
      end subroutine
      
c ########################################################################################
      subroutine hsv_set_6x6(es, nameX, history)
c
      use Tensor
      !implicit none
c
      real*8, dimension(6,6), intent(in) :: es
      character(len=5), intent(in) :: nameX
      real*8, intent(inout) :: history(*)
      integer :: start_index, end_index, i, j
c
      start_index = hsv_get_sth(nameX, 'start_',history)
      end_index =   hsv_get_sth(nameX, 'end__',history)

      if ( hsv_get_sth(nameX, 'length',history) /= 36 ) then
           write(*,*) "hsv_set_6x6<< Selected nameX (",nameX,
     &      " is not a 6x6. Check for typos or choose a different
     &      subroutine that also handles this."
      endif
c
      forall(i=1:6,j=1:6) history(start_index-1+j+(i-1)*6) = es(i,j)
c
      return
      end subroutine  
 
c ########################################################################################
      subroutine hsv_set_sym6x6(es, nameX, history)
c
      use Tensor
      !implicit none
c
      real*8, dimension(6,6), intent(in) :: es
      character(len=5), intent(in) :: nameX
      real*8, intent(inout) :: history(*)
      integer :: start_index
c
      start_index = hsv_get_sth(nameX, 'start_',history)

      if ( hsv_get_sth(nameX, 'length',history) /= 21 ) then
           write(*,*) "hsv_set_sym6x6<< Selected nameX (",nameX,
     &      " is not a sym 6x6. Check for typos or choose a different
     &      subroutine that also handles this."
      endif
c
      history(start_index)    = es(1,1)
      history(start_index+1)  = es(1,2)
      history(start_index+2)  = es(2,2)
      history(start_index+3)  = es(1,3)
      history(start_index+4)  = es(2,3)
      history(start_index+5)  = es(3,3)
      history(start_index+6)  = es(1,4)
      history(start_index+7)  = es(2,4)
      history(start_index+8)  = es(3,4)
      history(start_index+9)  = es(4,4)
      history(start_index+10)  = es(1,5)
      history(start_index+11)  = es(2,5)
      history(start_index+12)  = es(3,5)
      history(start_index+13)  = es(4,5)
      history(start_index+14)  = es(5,5)
      history(start_index+15)  = es(1,6)
      history(start_index+16)  = es(2,6)
      history(start_index+17)  = es(3,6)
      history(start_index+18)  = es(4,6)
      history(start_index+19)  = es(5,6)
      history(start_index+20)  = es(6,6)
c
      return
      end subroutine  

c ########################################################################################
      subroutine hsv_set_ten4_as_6x6(ten4, nameX, history)
c
      use Tensor
      !implicit none
c
      type(Tensor4), intent(in) :: ten4
      character(len=5), intent(in) :: nameX
      real*8, intent(inout) :: history(*)
      integer :: start_index, end_index
c
      start_index = hsv_get_sth(nameX, 'start_',history)
      end_index =   hsv_get_sth(nameX, 'end__',history)

      if ( hsv_get_sth(nameX, 'length',history) /= 36 ) then
           write(*,*) "hsv_get_symTen2<< Selected nameX (",nameX,
     &      " is not a 6x6. Check for typos or choose a different
     &      subroutine that also handles this."
      endif
c
      history(start_index:end_index) = ten_4_to_list_36(ten4)
c
      return
      end subroutine  
