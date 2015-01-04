program euler04
  implicit none
  
  integer :: number
  integer :: i
  integer :: j
  integer :: mult
  integer :: largest
  integer :: i_largest
  integer :: j_largest

  largest = -1

  do i = 1, 999
     do j = 1, 999
        mult = i * j

        if (is_palindrome(mult)) then
           if (mult > largest) then
              i_largest = i
              j_largest = j
              largest = mult
              !write (*,*) i, j, mult
           end if
        endif

     end do
  end do

  !write (*,*)
  write (*,*) i_largest, j_largest, largest

contains
  
  
  logical function is_palindrome(number)
    integer, intent(in) :: number
    integer :: i
    integer :: places2
    
    places2 = places(number)   
    is_palindrome = .true.
    
    do i=1, places2/2
       if (digit(number, i) /= digit(number, places2 + 1 - i)) then
         is_palindrome = .false.
         exit
       end if
    end do    
      
  end function is_palindrome

    
  integer function places(number)
    integer, intent(in) :: number
    
    places = aint(log10(real(number))) + 1
      
  end function places


  integer function digit(number, place)
    integer, intent(in) :: number
    integer, intent(in) :: place
    integer :: temp

    temp = 10 ** (place-1)
    digit = modulo(number, temp * 10) / temp

  end function digit
    

end program



