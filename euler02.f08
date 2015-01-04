program euler02

  integer :: previous_fib
  integer :: current_fib
  integer :: temp
  integer :: sum

  previous_fib = 0
  current_fib = 1
  sum = 0

  do while(current_fib < 4000000)
     temp = previous_fib + current_fib
     previous_fib = current_fib
     current_fib = temp
     if (modulo(current_fib, 2) == 0) then
        sum = sum + current_fib
     end if
  end do
  
  write (*,*) sum
  
  

end program euler02
