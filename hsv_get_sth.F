c ########################################################################################
c @todo Why are these function "integer function"s, what is returned?
      integer function hsv_get_sth(nameX, sth_string, history)
c
      implicit none
c
      ! Name of the history variable, e.g. "alpha"
      character(len=5), intent(in) :: nameX
      ! The hsv-property that shall be returned, e.g. "start_" for the starting index
      character(len=6), intent(in) :: sth_string
      real*8, intent(in), dimension(*) :: history
      integer hsv_version
c
      hsv_version = int( history(1) )
      ! @todo Sometimes we get a zero as hsv-version
      !hsv_version=1
c      
      select case( hsv_version )
         case( 0 ) 
            write(*,*) "hsv_get_sth<<
     &From 01.04.2022 you need to init 
     &the hsv with the hsv_version stored as material parameter. 
     &Please call hsv_init(*) at the beginning of your material model."
            write(*,*) "           <<
     &Attempting to call with nameX=",nameX,
     &" and sth_string=",sth_string
            call cstop("E r r o r   T e r m i n a t i o n")
         case( 1 )
            hsv_get_sth = hsv_get_sth_V1(nameX, sth_string)
         case( 2 )
            hsv_get_sth = hsv_get_sth_V2(nameX, sth_string)
         case( 3 )
            hsv_get_sth = hsv_get_sth_V3(nameX, sth_string)      
         case( 4 )
            hsv_get_sth = hsv_get_sth_V4(nameX, sth_string)   
         case( 5 )
            hsv_get_sth = hsv_get_sth_V5(nameX, sth_string)         
         case( 6 )
            hsv_get_sth = hsv_get_sth_V6(nameX, sth_string)             
         case default
            write( *, * ) 'hsv_get_sth<< 
     &undefined hsv_version (',hsv_version,")"
            call cstop("E r r o r   T e r m i n a t i o n")
      end select 
c
c @todo Centralise the "do i=1,n_entries" and everything that is equal for all versions     
c @todo Can we implement this object-oriented and reuse for instance alpha for all hsv-variants?
c
      end function hsv_get_sth


c##########################################################################
      integer function hsv_get_sth_V1(nameX, sth_string)
c isotropic and anisotropic plasticity
c damage and gradient-enhancement      
c no kinematic hardening      
c nhv=31      
c
      implicit none
c
      character(len=5), intent(in) :: nameX
      character(len=6), intent(in) :: sth_string
      character(len=10), dimension(:), allocatable :: list_name
      integer, dimension(:), allocatable :: list_start_index
      integer, dimension(:), allocatable :: list_length
      integer, dimension(:), allocatable :: list_end_index
      integer, dimension(:), allocatable :: list_pair
      integer :: i, len_X, j, n_entries
      len_X = len(nameX)
c
c USER-INPUT: Update the number of entities (including "es" and "defoGrad")
      n_entries=6
c
      allocate(list_name(n_entries))
      allocate(list_start_index(n_entries))
      allocate(list_length(n_entries))
      allocate(list_end_index(n_entries))
      allocate(list_pair(n_entries))
c
c @note The max size of hsv is 200, however this can be increased in "nhisparm.inc"
c Init the strings with blanks
      !do i=1,5
      !    list_name(i) = '          '
      !enddo
c @todo Try to implement the below convention to unify the parameters also for different code versions      
c       CONVENTION:
c       es . alpha . eps_p . damag . defoGrad
c       10    02      03      09      45
c       1002030945
c       CONTENT:
c       1    . 2     . 3  4  5  6  7  8 . 9     . 10    . 11 ...  31   . 32 33 34  35 36 37  38 39 40
c       hsv1 . alpha . eps_p1 ...eps_p6 . damag . d_loc . es1 ... es21 . defoGrad1 ... defoGrad9
c @note Use nhv=31, where nhv corresponds to the index of the last entry "es", here es21
c
c @WARNING: Most of the numbers are not updated automatically, so check every number
c after you changed something.
c @todo clean this use index "j" to avoid typing the same thing several times
c @todo Check that the "nhv" is large enough for everything      
c
c @note The order in the list_* is irrelevant and only locally used
c Tangent in Voigt 6x6 notation
      list_name(1) = 'esSYM'
      list_start_index(1) = 11
      list_length(1) = 21
      list_end_index(1) =  list_start_index(1) + list_length(1)-1
      list_pair(1) = 01
c Internal variable alpha
      list_name(2) = 'alpha'
      list_start_index(2) = 02
      list_length(2) = 1
      list_end_index(2) =  list_start_index(2) + list_length(2)-1
      list_pair(2) = 02    
c Plastic strain tensor epsp
      list_name(3) = 'eps_p'
      list_start_index(3) = 03
      list_length(3) = 6
      list_end_index(3) =  list_start_index(3) + list_length(3)-1
      list_pair(3) = 03       
c damage damag
      list_name(4) = 'damag'
      list_start_index(4) = 09
      list_length(4) = 1
      list_end_index(4) =  list_start_index(4) + list_length(4)-1
      list_pair(4) = 04 
c local damage
      list_name(5) = 'd_loc'
      list_start_index(5) = 10
      list_length(5) = 1
      list_end_index(5) =  list_start_index(5) + list_length(5)-1
      list_pair(5) = 05 
c @todo Apply the following quicker assignment using "j=6" for all entries
c  and apply everything that solely relies on "j" afterwards in a loop over all "j"
c Deformation gradient F
      j = 6
      list_pair(j) = j
      list_name(j) = 'defoG'
      list_start_index(j) = 32
      list_length(j) = 9
      list_end_index(j) =  list_start_index(j) + list_length(j)-1
c
c
      do i=1,n_entries
          ! Look for the nameX in the list of names to find the index 'i'
          if ( index(nameX,trim(list_name(i))) /= 0 ) then
              if ( index(sth_string, 'start_') /= 0 ) then
                  hsv_get_sth_V1 = list_start_index(i)
                    exit
              else if ( index(sth_string, 'length') /= 0 ) then
                  hsv_get_sth_V1 = list_length(i)
                    exit
              else if ( index(sth_string, 'end__') /= 0 ) then
                  hsv_get_sth_V1 = list_end_index(i)
                    exit
              else if ( index(sth_string, 'ipair_') /= 0 ) then
                  hsv_get_sth_V1 = list_pair(i)
                    exit
              else
                  write(*,*) "hsv_get_sth_V1<<
     &Could not find sth_string"
              endif
          elseif ( i == n_entries ) then
              write(*,*) "hsv_get_sth_V1<< Could not find the nameX"
              write(*,*) "nameX: ",nameX
              write(*,*) "sth_string: ",sth_string
              pause
          endif
      enddo

      end function hsv_get_sth_V1

c      integer function hsv_get_sth_V2(nameX, sth_string)
c two back stresses
c @note Use nhsv=43, where nhsv corresponds to the index of the last entry es36
c       1    . 2     . 3  4  5  6  7  8 . 9     . 10    . 11 ... 16 . 17 ... 22 . 23 ...  43 . 44 45 46  47 48 49  50 51 52
c       hsv1 . alpha . eps_p1 ...eps_p6 . damag . d_loc . B_1_ij    . B_2_ij    . es1...es21 . defoGrad1 ... defoGrad9
c
c      end function hsv_get_sth_V2

c##########################################################################
      integer function hsv_get_sth_V2(nameX, sth_string)
c isotropic and anisotropic plasticity
c damage and gradient-enhancement      
c no kinematic hardening    
c storing triax and Lode  
c epsInit and epsFail 
c
      implicit none
c
      character(len=5), intent(in) :: nameX
      character(len=6), intent(in) :: sth_string
      character(len=10), dimension(:), allocatable :: list_name
      integer, dimension(:), allocatable :: list_start_index
      integer, dimension(:), allocatable :: list_length
      integer, dimension(:), allocatable :: list_end_index
      integer, dimension(:), allocatable :: list_pair
      integer :: i, len_X, j, n_entries
      len_X = len(nameX)
c
c USER-INPUT: Update the number of entities (including "es" and "defoGrad")
      n_entries=12
c
      allocate(list_name(n_entries))
      allocate(list_start_index(n_entries))
      allocate(list_length(n_entries))
      allocate(list_end_index(n_entries))
      allocate(list_pair(n_entries))
c
c @note The max size of hsv is 200, however this can be increased in "nhisparm.inc"
c Init the strings with blanks
      !do i=1,5
      !    list_name(i) = '          '
      !enddo
c       CONTENT:
c       1    . 2     . 3  4  5  6  7  8 . 9     . 10    . 11    . 12   . 13    . 14    . 15      . 16      . 17 ... 37    . 38 39 40  41 42 43  44 45 46
c       hsv1 . alpha . eps_p1 ...eps_p6 . damag . d_loc . triax . Lode . a_til . Hpacc . epsInit . epsFail . es1 ... es21 . defoGrad1    ...   defoGrad9
c @note Use nhv=37, where nhv corresponds to the index of the last entry "es", here es21
c
c @WARNING: Most of the numbers are not updated automatically, so check every number
c after you changed something.
c @todo clean this use index "j" to avoid typing the same thing several times
c @todo Check that the "nhv" is large enough for everything      
c
c @note The order in the list_* is irrelevant and only locally used
c Tangent in Voigt 6x6 notation
      list_name(1) = 'esSYM'
      list_start_index(1) = 17
      list_length(1) = 21
      list_end_index(1) =  list_start_index(1) + list_length(1)-1
      list_pair(1) = 01
c Internal variable alpha
      list_name(2) = 'alpha'
      list_start_index(2) = 02
      list_length(2) = 1
      list_end_index(2) =  list_start_index(2) + list_length(2)-1
      list_pair(2) = 02    
c Plastic strain tensor epsp
      list_name(3) = 'eps_p'
      list_start_index(3) = 03
      list_length(3) = 6
      list_end_index(3) =  list_start_index(3) + list_length(3)-1
      list_pair(3) = 03       
c damage damag
      list_name(4) = 'damag'
      list_start_index(4) = 09
      list_length(4) = 1
      list_end_index(4) =  list_start_index(4) + list_length(4)-1
      list_pair(4) = 04 
c local damage
      list_name(5) = 'd_loc'
      list_start_index(5) = 10
      list_length(5) = 1
      list_end_index(5) =  list_start_index(5) + list_length(5)-1
      list_pair(5) = 05 
c @todo Apply the following quicker assignment using "j=6" for all entries
c  and apply everything that solely relies on "j" afterwards in a loop over all "j"
c Deformation gradient F
      j = 6
      list_pair(j) = j
      list_name(j) = 'defoG'
      list_start_index(j) = 38
      list_length(j) = 9
      list_end_index(j) =  list_start_index(j) + list_length(j)-1
c
      j = 7
      list_pair(j) = j
      list_name(j) = 'Triax'
      list_start_index(j) = 11
      list_length(j) = 1
      list_end_index(j) =  list_start_index(j) + list_length(j)-1
c
      j = 8
      list_pair(j) = j
      list_name(j) = 'LodeP'
      list_start_index(j) = 12
      list_length(j) = 1
      list_end_index(j) =  list_start_index(j) + list_length(j)-1      
c
      j = 9
      list_pair(j) = j
      list_name(j) = 'a_til'
      list_start_index(j) = 13
      list_length(j) = 1
      list_end_index(j) =  list_start_index(j) + list_length(j)-1    
c
      j = 10
      list_pair(j) = j
      list_name(j) = 'Hpacc'
      list_start_index(j) = 14
      list_length(j) = 1
      list_end_index(j) =  list_start_index(j) + list_length(j)-1    
c
      j = 11
      list_pair(j) = j
      list_name(j) = 'epsIn'
      list_start_index(j) = 15
      list_length(j) = 1
      list_end_index(j) =  list_start_index(j) + list_length(j)-1    
c
      j = 12
      list_pair(j) = j
      list_name(j) = 'epsFa'
      list_start_index(j) = 16
      list_length(j) = 1
      list_end_index(j) =  list_start_index(j) + list_length(j)-1    
c    
      do i=1,n_entries
          ! Look for the nameX in the list of names to find the index 'i'
          if ( index(nameX,trim(list_name(i))) /= 0 ) then
              if ( index(sth_string, 'start_') /= 0 ) then
                  hsv_get_sth_V2 = list_start_index(i)
                    exit
              else if ( index(sth_string, 'length') /= 0 ) then
                  hsv_get_sth_V2 = list_length(i)
                    exit
              else if ( index(sth_string, 'end__') /= 0 ) then
                  hsv_get_sth_V2 = list_end_index(i)
                    exit
              else if ( index(sth_string, 'ipair_') /= 0 ) then
                  hsv_get_sth_V2 = list_pair(i)
                    exit
              else
                  write(*,*) "hsv_get_sth_V2<<
     &Could not find sth_string"
              endif
          elseif ( i == n_entries ) then
              write(*,*) "hsv_get_sth_V2<< Could not find the nameX"
              write(*,*) "nameX: ",nameX
              write(*,*) "sth_string: ",sth_string
              pause
          endif
      enddo

      end function hsv_get_sth_V2


c##########################################################################
      integer function hsv_get_sth_V3(nameX, sth_string)
c
c isotropic and anisotropic plasticity
c damage and gradient-enhancement      
c kinematic hardening with up to three back stresses
c nhv=49
c  
      implicit none
c
      character(len=5), intent(in) :: nameX
      character(len=6), intent(in) :: sth_string
      character(len=10), dimension(:), allocatable :: list_name
      integer, dimension(:), allocatable :: list_start_index
      integer, dimension(:), allocatable :: list_length
      integer, dimension(:), allocatable :: list_end_index
      integer, dimension(:), allocatable :: list_pair
      integer :: i, len_X, j, n_entries
      len_X = len(nameX)
c
c USER-INPUT: Update the number of entities (including "es" and "defoGrad")
      n_entries=8+1
c
      allocate(list_name(n_entries))
      allocate(list_start_index(n_entries))
      allocate(list_length(n_entries))
      allocate(list_end_index(n_entries))
      allocate(list_pair(n_entries))
c
c three back stresses
c @note Use nhv=49, where nhv corresponds to the index of the last entry es, here es21
c       1    . 2     . 3  4  5  6  7  8 . 9     . 10    . 11 ... 16 . 17 ... 22 . 23 ... 28 . 29 ...  49 . 50 ...              58
c       hsv1 . alpha . eps_p1 ...eps_p6 . damag . d_loc . B_1_ij    . B_2_ij    . B_3_ij    . es1...es21 . defoGrad1 ... defoGrad9
c
c @WARNING: Most of the numbers are not updated automatically, so check every number
c after you changed something.
c @todo clean this use index "j" to avoid typing the same thing several times
c @todo Check that the "nhv" is large enough for everything      
c
c @note The order in the list_* is irrelevant and only locally used
c Tangent in Voigt sym 6x6 notation
      list_name(1) = 'esSYM'
      list_start_index(1) = 11+6+6+6
      list_length(1) = 21
      list_end_index(1) =  list_start_index(1) + list_length(1)-1
      list_pair(1) = 01
c Internal variable alpha
      list_name(2) = 'alpha'
      list_start_index(2) = 02
      list_length(2) = 1
      list_end_index(2) =  list_start_index(2) + list_length(2)-1
      list_pair(2) = 02    
c Plastic strain tensor epsp
      list_name(3) = 'eps_p'
      list_start_index(3) = 03
      list_length(3) = 6
      list_end_index(3) =  list_start_index(3) + list_length(3)-1
      list_pair(3) = 03       
c damage damag
      list_name(4) = 'damag'
      list_start_index(4) = 09
      list_length(4) = 1
      list_end_index(4) =  list_start_index(4) + list_length(4)-1
      list_pair(4) = 04 
c local damage
      list_name(5) = 'd_loc'
      list_start_index(5) = 10
      list_length(5) = 1
      list_end_index(5) =  list_start_index(5) + list_length(5)-1
      list_pair(5) = 05 
c @todo Apply the following quicker assignment using "j=6" for all entries
c  and apply everything that solely relies on "j" afterwards in a loop over all "j"
c Deformation gradient F
      j = 6
      list_pair(j) = j
      list_name(j) = 'defoG'
      list_start_index(j) = 50
      list_length(j) = 9
      list_end_index(j) =  list_start_index(j) + list_length(j)-1
c B_1: first back stress
      j = 7
      list_pair(j) = j
      list_name(j) = 'B_1__'
      list_start_index(j) = 11
      list_length(j) = 6
      list_end_index(j) =  list_start_index(j) + list_length(j)-1
c B_2: second back stress
      j = 8
      list_pair(j) = j
      list_name(j) = 'B_2__'
      list_start_index(j) = 11+6
      list_length(j) = 6
      list_end_index(j) =  list_start_index(j) + list_length(j)-1
c B_3: third back stress
      j = 9
      list_pair(j) = j
      list_name(j) = 'B_3__'
      list_start_index(j) = 11+6+6
      list_length(j) = 6
      list_end_index(j) =  list_start_index(j) + list_length(j)-1
c
c
      do i=1,n_entries
          ! Look for the nameX in the list of names to find the index 'i'
          if ( index(nameX,trim(list_name(i))) /= 0 ) then
              if ( index(sth_string, 'start_') /= 0 ) then
                  hsv_get_sth_V3 = list_start_index(i)
                    exit
              else if ( index(sth_string, 'length') /= 0 ) then
                  hsv_get_sth_V3 = list_length(i)
                    exit
              else if ( index(sth_string, 'end__') /= 0 ) then
                  hsv_get_sth_V3 = list_end_index(i)
                    exit
              else if ( index(sth_string, 'ipair_') /= 0 ) then
                  hsv_get_sth_V3 = list_pair(i)
                    exit
              else
                  write(*,*) "hsv_get_sth_V3<<
     &Could not find sth_string"
              endif
          elseif ( i == n_entries ) then
              write(*,*) "hsv_get_sth_V3<< Could not find the nameX"
              write(*,*) "nameX: ",nameX
              write(*,*) "sth_string: ",sth_string
              pause
          endif
      enddo

      end function hsv_get_sth_V3


c##########################################################################
      integer function hsv_get_sth_V4(nameX, sth_string)
c isotropic and anisotropic plasticity
c damage and gradient-enhancement      
c no kinematic hardening    
c storing triax and Lode  
c epsInit and epsFail 
c
      implicit none
c
      character(len=5), intent(in) :: nameX
      character(len=6), intent(in) :: sth_string
      character(len=10), dimension(:), allocatable :: list_name
      integer, dimension(:), allocatable :: list_start_index
      integer, dimension(:), allocatable :: list_length
      integer, dimension(:), allocatable :: list_end_index
      integer, dimension(:), allocatable :: list_pair
      integer :: i, len_X, j, n_entries
      len_X = len(nameX)
c
c USER-INPUT: Update the number of entities (including "es" and "defoGrad")
      n_entries=12
c
      allocate(list_name(n_entries))
      allocate(list_start_index(n_entries))
      allocate(list_length(n_entries))
      allocate(list_end_index(n_entries))
      allocate(list_pair(n_entries))
c
c @note The max size of hsv is 200, however this can be increased in "nhisparm.inc"
c Init the strings with blanks
      !do i=1,5
      !    list_name(i) = '          '
      !enddo
c       CONTENT:
c       1    . 2   ...    17  . 18   ...  38 . 39 40 41  42 43 44  45 46 47
c       hsv1 . hsv-variables  . es1 ... es21 . defoGrad1    ...   defoGrad9
c @note Use nhv=38, where nhv corresponds to the index of the last entry "es", here es21
c
c @WARNING: Most of the numbers are not updated automatically, so check every number
c after you changed something.
c @todo clean this use index "j" to avoid typing the same thing several times
c @todo Check that the "nhv" is large enough for everything      
c
c @note The order in the list_* is irrelevant and only locally used
c Tangent in Voigt 6x6 notation
      list_name(1) = 'esSYM'
      list_start_index(1) = 18
      list_length(1) = 21
      list_end_index(1) =  list_start_index(1) + list_length(1)-1
      list_pair(1) = 01
c damage damag
      list_name(4) = 'damag'
      list_start_index(4) = 09
      list_length(4) = 1
      list_end_index(4) =  list_start_index(4) + list_length(4)-1
      list_pair(4) = 04 
c @todo Apply the following quicker assignment using "j=6" for all entries
c  and apply everything that solely relies on "j" afterwards in a loop over all "j"
c Deformation gradient F
      j = 6
      list_pair(j) = j
      list_name(j) = 'defoG'
      list_start_index(j) = 39
      list_length(j) = 9
      list_end_index(j) =  list_start_index(j) + list_length(j)-1
c
      do i=1,n_entries
          ! Look for the nameX in the list of names to find the index 'i'
          if ( index(nameX,trim(list_name(i))) /= 0 ) then
              if ( index(sth_string, 'start_') /= 0 ) then
                  hsv_get_sth_V4 = list_start_index(i)
                    exit
              else if ( index(sth_string, 'length') /= 0 ) then
                  hsv_get_sth_V4 = list_length(i)
                    exit
              else if ( index(sth_string, 'end__') /= 0 ) then
                  hsv_get_sth_V4 = list_end_index(i)
                    exit
              else if ( index(sth_string, 'ipair_') /= 0 ) then
                  hsv_get_sth_V4 = list_pair(i)
                    exit
              else
                  write(*,*) "hsv_get_sth_V4<<
     &Could not find sth_string"
              endif
          elseif ( i == n_entries ) then
              write(*,*) "hsv_get_sth_V4<< Could not find the nameX"
              write(*,*) "nameX: ",nameX
              write(*,*) "sth_string: ",sth_string
              pause
          endif
      enddo

      end function hsv_get_sth_V4

c##########################################################################
      integer function hsv_get_sth_V5(nameX, sth_string)
c isotropic and anisotropic plasticity
c damage and gradient-enhancement      
c kinematic hardening    
c storing triax and Lode  
c epsInit and epsFail 
c
      implicit none
c
      character(len=5), intent(in) :: nameX
      character(len=6), intent(in) :: sth_string
      character(len=10), dimension(:), allocatable :: list_name
      integer, dimension(:), allocatable :: list_start_index
      integer, dimension(:), allocatable :: list_length
      integer, dimension(:), allocatable :: list_end_index
      integer, dimension(:), allocatable :: list_pair
      integer :: i, len_X, j, n_entries
      len_X = len(nameX)
c
c USER-INPUT: Update the number of entities (including "es" and "defoGrad")
      n_entries=3
c
      allocate(list_name(n_entries))
      allocate(list_start_index(n_entries))
      allocate(list_length(n_entries))
      allocate(list_end_index(n_entries))
      allocate(list_pair(n_entries))
c
c @note The max size of hsv is 200, however this can be increased in "nhisparm.inc"
c Init the strings with blanks
      !do i=1,5
      !    list_name(i) = '          '
      !enddo
c       CONTENT:
c       1    . 2   ...    17  . 18   ...   35 . 36 ... 41 . 42  ...   62 . 63 ...
c       hsv1 . hsv-variables  . back stresses . eps p +   . es1 ... es21 . defoGrad1    ...   defoGrad9
c @note Use nhv=56, where nhv corresponds to the index of the last entry "es", here es21
c
c @WARNING: Most of the numbers are not updated automatically, so check every number
c after you changed something.
c @todo clean this use index "j" to avoid typing the same thing several times
c @todo Check that the "nhv" is large enough for everything      
c
c @note The order in the list_* is irrelevant and only locally used
c Tangent in Voigt 6x6 notation
      list_name(1) = 'esSYM'
      list_start_index(1) = 42
      list_length(1) = 21
      list_end_index(1) =  list_start_index(1) + list_length(1)-1
      list_pair(1) = 01
c damage damag
      list_name(2) = 'damag'
      list_start_index(2) = 09
      list_length(2) = 1
      list_end_index(2) =  list_start_index(2) + list_length(2)-1
      list_pair(2) = 02 
c @todo Apply the following quicker assignment using "j=6" for all entries
c  and apply everything that solely relies on "j" afterwards in a loop over all "j"
c Deformation gradient F
      j = 3
      list_pair(j) = j
      list_name(j) = 'defoG'
      list_start_index(j) = 63
      list_length(j) = 9
      list_end_index(j) =  list_start_index(j) + list_length(j)-1
c
      do i=1,n_entries
          ! Look for the nameX in the list of names to find the index 'i'
          if ( index(nameX,trim(list_name(i))) /= 0 ) then
              if ( index(sth_string, 'start_') /= 0 ) then
                  hsv_get_sth_V5 = list_start_index(i)
                    exit
              else if ( index(sth_string, 'length') /= 0 ) then
                  hsv_get_sth_V5 = list_length(i)
                    exit
              else if ( index(sth_string, 'end__') /= 0 ) then
                  hsv_get_sth_V5 = list_end_index(i)
                    exit
              else if ( index(sth_string, 'ipair_') /= 0 ) then
                  hsv_get_sth_V5 = list_pair(i)
                    exit
              else
                  write(*,*) "hsv_get_sth_V5<<
     &Could not find sth_string"
              endif
          elseif ( i == n_entries ) then
              write(*,*) "hsv_get_sth_V5<< Could not find the nameX"
              write(*,*) "nameX: ",nameX
              write(*,*) "sth_string: ",sth_string
              pause
          endif
      enddo

      end function hsv_get_sth_V5

c##########################################################################
      integer function hsv_get_sth_V6(nameX, sth_string)
c multiplicative plasticity
c isotropic and kinematic hardening
c exponential map (U_p, U_pi)
c uncoupled damage/failure (d, failInd, dmgDriver)
c
      implicit none
c
      character(len=5), intent(in) :: nameX
      character(len=6), intent(in) :: sth_string
      character(len=10), dimension(:), allocatable :: list_name
      integer, dimension(:), allocatable :: list_start_index
      integer, dimension(:), allocatable :: list_length
      integer, dimension(:), allocatable :: list_end_index
      integer, dimension(:), allocatable :: list_pair
      integer :: i, len_X, j, n_entries
      len_X = len(nameX)
c
c USER-INPUT: Update the number of entities (including "es" and "defoGrad")
      n_entries=3
c
      allocate(list_name(n_entries))
      allocate(list_start_index(n_entries))
      allocate(list_length(n_entries))
      allocate(list_end_index(n_entries))
      allocate(list_pair(n_entries))
c
c @note The max size of hsv is 200, however this can be increased in "nhisparm.inc"
c Init the strings with blanks
      !do i=1,5
      !    list_name(i) = '          '
      !enddo
c       CONTENT:
c       1    . 2     . 3 ... 8  . 9   . 10 ... 15 .     16    .   17    . 18...23 . 24  ...  44  . 45 ... 53
c       hsv1 . alpha . Up_inv-I . dmg . Upi_inv-I . dmgDriver . failInd . epsp+   . es1 ... es21 . defoGrad1 ... defoGrad9
c @note Use nhv=44, where nhv corresponds to the index of the last entry "es", here es21
c
c @WARNING: Most of the numbers are not updated automatically, so check every number
c after you changed something.
c @todo clean this use index "j" to avoid typing the same thing several times
c @todo Check that the "nhv" is large enough for everything      
c
c @note The order in the list_* is irrelevant and only locally used
c Tangent in Voigt 6x6 notation
      list_name(1) = 'esSYM'
      list_start_index(1) = 24
      list_length(1) = 21
      list_end_index(1) =  list_start_index(1) + list_length(1)-1
      list_pair(1) = 01
c damage damag
      list_name(2) = 'damag'
      list_start_index(2) = 09
      list_length(2) = 1
      list_end_index(2) =  list_start_index(2) + list_length(2)-1
      list_pair(2) = 02 
c @todo Apply the following quicker assignment using "j=6" for all entries
c  and apply everything that solely relies on "j" afterwards in a loop over all "j"
c Deformation gradient F
      j = 3
      list_pair(j) = j
      list_name(j) = 'defoG'
      list_start_index(j) = 45
      list_length(j) = 9
      list_end_index(j) =  list_start_index(j) + list_length(j)-1
c
      do i=1,n_entries
          ! Look for the nameX in the list of names to find the index 'i'
          if ( index(nameX,trim(list_name(i))) /= 0 ) then
              if ( index(sth_string, 'start_') /= 0 ) then
                  hsv_get_sth_V6 = list_start_index(i)
                    exit
              else if ( index(sth_string, 'length') /= 0 ) then
                  hsv_get_sth_V6 = list_length(i)
                    exit
              else if ( index(sth_string, 'end__') /= 0 ) then
                  hsv_get_sth_V6 = list_end_index(i)
                    exit
              else if ( index(sth_string, 'ipair_') /= 0 ) then
                  hsv_get_sth_V6 = list_pair(i)
                    exit
              else
                  write(*,*) "hsv_get_sth_V6<<
     &Could not find sth_string"
              endif
          elseif ( i == n_entries ) then
              write(*,*) "hsv_get_sth_V6<< Could not find the nameX"
              write(*,*) "nameX: ",nameX
              write(*,*) "sth_string: ",sth_string
              pause
          endif
      enddo

      end function hsv_get_sth_V6

c OoO
c unsymmetric es
c      list_name(1) = 'es___'
c      list_start_index(1) = 11
c      list_length(1) = 36
c      list_end_index(1) =  list_start_index(1) + list_length(1)-1
c      list_pair(1) = 01      