program euler10

  integer, parameter :: BTS = selected_int_kind(18)
  integer(BTS) :: sum
  integer :: i
  integer :: factor
  integer, parameter :: limit = 2000000

  sum = 2
  outer: do i=3, limit, 2
     do factor=3, int(sqrt(real(i+1))), 2
        if (modulo(i,factor) == 0) then
           cycle outer
        end if
        
        
     end do

     if (sum + i < sum) then
        write (*,*) '!!!!', sum
     end if
     sum = sum + i
     
  end do outer

  write (*,*) sum

end program euler10
