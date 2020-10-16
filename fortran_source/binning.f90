module Mbinning

  use, intrinsic :: iso_c_binding, only: dp=>C_DOUBLE

  contains

  integer function test(a,b)
    implicit none
    integer, intent(in) :: a,b
    test = a+b
    return
  end function test

  ! the matrix, vector sizes go to the end of the call list
  ! since we do not have to provide them in python
  ! automatic detection
  subroutine binMatrix_r(inMatrix, vec1, vec2, outMatrix, inX, inY, vec1length, vec2length, outX, outY)
    implicit none
    integer, intent(in) :: inX, inY, vec1length, vec2length, outX, outY

    real(dp), dimension(inX, inY), intent(in) :: inMatrix
    real(dp), dimension(outX, outY), intent(inout) :: outMatrix

    integer, dimension(vec1length), intent(in) :: vec1
    integer, dimension(vec2length), intent(in) :: vec2

    logical :: ordered

    integer :: i,j
    integer :: ii,jj
    real(dp) :: tempsum

    integer :: xbegin, xend
    integer :: ybegin, yend

    ! check vector lengths

    if ((vec1length+1 /= outX) .or. (vec2length+1 /= outY)) then
      write(*,*) 'WARNING: outMatrix size inconsistent with vector'
    endif


    ordered = .true.
    do i=2,vec1length
      if (vec1(i) <= vec1(i-1)) then
        ordered = .false.
      endif
    enddo

    if (.not. ordered) then
      write(*,*) 'WARNING: X-vector not ordered'
    endif

    ordered = .true.
    do j=2,vec2length
      if (vec2(j) <= vec2(j-1)) then
        ordered = .false.
      endif
    enddo

    if (.not. ordered) then
      write(*,*) 'WARNING: Y-vector not ordered'
    endif


    ! we scan through the coarse grid
    do j=1,vec2length+1
      do i=1,vec1length+1

        ! determine index ranges
        ! here we go from positions [one-indexed]
        ! to intervals
        if (i==1) then
          xbegin = 1
        else
          xbegin = vec1(i-1)+1
        endif

        if (i==(vec1length+1)) then
          xend = inX
        else
          xend = vec1(i)
        endif

        if (j==1) then
          ybegin = 1
        else
          ybegin = vec2(j-1)+1
        endif

        if (j==(vec2length+1)) then
          yend = inY
        else
          yend = vec2(j)
        endif


        ! index through the coarse grid and sum all elements
        tempsum = 0
        do jj=ybegin,yend
          do ii=xbegin,xend
            tempsum = tempsum + inMatrix(ii,jj)
          enddo
        enddo

        outMatrix(i,j) = tempsum

      enddo
    enddo

  end subroutine

  subroutine binMatrix_c(inMatrix, vec1, vec2, outMatrix, inX, inY, vec1length, vec2length, outX, outY)
    implicit none
    integer, intent(in) :: inX, inY, vec1length, vec2length, outX, outY

    complex(dp), dimension(inX, inY), intent(in) :: inMatrix
    complex(dp), dimension(outX, outY), intent(inout) :: outMatrix

    integer, dimension(vec1length), intent(in) :: vec1
    integer, dimension(vec2length), intent(in) :: vec2

    logical :: ordered

    integer :: i,j
    integer :: ii,jj
    complex(dp) :: tempsum

    integer :: xbegin, xend
    integer :: ybegin, yend

    ! check vector lengths

    if ((vec1length+1 /= outX) .or. (vec2length+1 /= outY)) then
      write(*,*) 'WARNING: outMatrix size inconsistent with vector'
    endif


    ordered = .true.
    do i=2,vec1length
      if (vec1(i) <= vec1(i-1)) then
        ordered = .false.
      endif
    enddo

    if (.not. ordered) then
      write(*,*) 'WARNING: X-vector not ordered'
    endif

    ordered = .true.
    do j=2,vec2length
      if (vec2(j) <= vec2(j-1)) then
        ordered = .false.
      endif
    enddo

    if (.not. ordered) then
      write(*,*) 'WARNING: Y-vector not ordered'
    endif


    ! we scan through the coarse grid
    do j=1,vec2length+1
      do i=1,vec1length+1

        ! determine index ranges
        ! here we go from positions [one-indexed]
        ! to intervals
        if (i==1) then
          xbegin = 1
        else
          xbegin = vec1(i-1)+1
        endif

        if (i==(vec1length+1)) then
          xend = inX
        else
          xend = vec1(i)
        endif

        if (j==1) then
          ybegin = 1
        else
          ybegin = vec2(j-1)+1
        endif

        if (j==(vec2length+1)) then
          yend = inY
        else
          yend = vec2(j)
        endif


        ! index through the coarse grid and sum all elements
        tempsum = 0
        do jj=ybegin,yend
          do ii=xbegin,xend
            tempsum = tempsum + inMatrix(ii,jj)
          enddo
        enddo

        outMatrix(i,j) = tempsum

      enddo
    enddo

  end subroutine

end module Mbinning
