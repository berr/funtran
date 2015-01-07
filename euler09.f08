program euler09
  integer :: a
  integer :: a2
  integer :: b
  integer :: b2
  integer :: c
  integer :: c2
  
  integer, parameter :: expected_sum = 1000

  outer: do a=1, expected_sum
     do b=1, a
        a2 = a ** 2
        b2 = b ** 2
        c2 = a2 + b2
        c = int(sqrt(real(c2)))

        if (c ** 2 /= c2) then
           cycle
        end if
        
        if (a + b + c == expected_sum) then
           exit outer
        end if
   
     end do
  end do outer

  write (*,*) a, b, c, a*b*c

  

  
end program euler09
