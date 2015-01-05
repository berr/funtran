program euler08

  character(len=1000), parameter :: input = &
       "73167176531330624919225119674426574742355349194934" // & 
       "96983520312774506326239578318016984801869478851843" // &
       "85861560789112949495459501737958331952853208805511" // &
       "12540698747158523863050715693290963295227443043557" // &
       "66896648950445244523161731856403098711121722383113" // &
       "62229893423380308135336276614282806444486645238749" // &
       "30358907296290491560440772390713810515859307960866" // &
       "70172427121883998797908792274921901699720888093776" // &
       "65727333001053367881220235421809751254540594752243" // &
       "52584907711670556013604839586446706324415722155397" // &
       "53697817977846174064955149290862569321978468622482" // &
       "83972241375657056057490261407972968652414535100474" // &
       "82166370484403199890008895243450658541227588666881" // &
       "16427171479924442928230863465674813919123162824586" // &
       "17866458359124566529476545682848912883142607690042" // &
       "24219022671055626321111109370544217506941658960408" // &
       "07198403850962455444362981230987879927244284909188" // &
       "84580156166097919133875499200524063689912560717606" // &
       "05886116467109405077541002256983155200055935729725" // &
       "71636269561882670428252483600823257530420752963450"

  integer, parameter :: sequence_length = 13

  integer, parameter :: BTS=selected_int_kind(13)
  integer(kind=BTS) :: greatest_sequence_mult
  integer :: greatest_sequence_index

  integer :: starting_index

  integer, dimension(sequence_length) :: current_sequence
  integer :: current_digit_index
  integer :: current_digit
  character :: current_char
  logical :: completed_sequence
  integer :: i
  integer :: j
  integer(kind=BTS) :: mult

  greatest_sequence_index = -1
  greatest_sequence_mult = 0
  starting_index = 1
  
  do while (starting_index <= (len(input) + 1 - sequence_length))

     completed_sequence = .true.
     
     do i=1, sequence_length
        current_digit_index = starting_index + i - 1
        current_char = input(current_digit_index:current_digit_index)
        read (current_char, '(i1)') current_digit
        current_sequence(i) = current_digit

        !if (current_digit == 0) then
        !   starting_index = current_digit_index + 1
        !   completed_sequence = .false.
        !   exit
        !end if

     end do

     if (completed_sequence) then
        mult = 1_BTS
        do j=1, sequence_length
           mult = mult * current_sequence(j)
        end do

        if (mult > greatest_sequence_mult) then
           greatest_sequence_mult = mult
           greatest_sequence_index = starting_index
        end if
        
        starting_index = starting_index + 1
     end if

  end do

  write (*,*) 'sequence start:', greatest_sequence_index
  write (*,*) 'sequence:', input(greatest_sequence_index:greatest_sequence_index + sequence_length-1)
  write (*,*) 'mult:', greatest_sequence_mult
   


end program euler08
