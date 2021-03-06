c ########################################################################################
      subroutine hsv_set_scalar(scalar, nameX, history)
c
      !implicit none
c
      double precision scalar
      character(len=5), intent(in) :: nameX
      dimension history(*)
      integer :: start_index
c
      start_index = hsv_get_sth(nameX, 'start_')

      if ( hsv_get_sth(nameX, 'length') /= 1 ) then
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
      type(Tensor2) :: symTen2
      character(len=5), intent(in) :: nameX
      dimension history(*)
      integer :: start_index, end_index
c
      start_index = hsv_get_sth(nameX, 'start_')
      end_index =   hsv_get_sth(nameX, 'end__')

      if ( hsv_get_sth(nameX, 'length') /= 6 ) then
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
      dimension history(*)
      integer :: start_index, end_index
c
      start_index = hsv_get_sth(nameX, 'start_')
      end_index =   hsv_get_sth(nameX, 'end__')

      if ( hsv_get_sth(nameX, 'length') /= 9 ) then
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
      double precision, dimension(6,6), intent(in) :: es
      character(len=5), intent(in) :: nameX
      dimension history(*)
      integer :: start_index, end_index, i, j
c
      start_index = hsv_get_sth(nameX, 'start_')
      end_index =   hsv_get_sth(nameX, 'end__')

      if ( hsv_get_sth(nameX, 'length') /= 36 ) then
           write(*,*) "hsv_get_symTen2<< Selected nameX (",nameX,
     &      " is not a 6x6. Check for typos or choose a different
     &      subroutine that also handles this."
      endif
c
      forall(i=1:6,j=1:6) history(start_index-1+j+(i-1)*6) = es(i,j)
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
      dimension history(*)
      integer :: start_index, end_index
c
      start_index = hsv_get_sth(nameX, 'start_')
      end_index =   hsv_get_sth(nameX, 'end__')

      if ( hsv_get_sth(nameX, 'length') /= 36 ) then
           write(*,*) "hsv_get_symTen2<< Selected nameX (",nameX,
     &      " is not a 6x6. Check for typos or choose a different
     &      subroutine that also handles this."
      endif
c
      history(start_index:end_index) = ten_4_to_list_36(ten4)
c
      return
      end subroutine  