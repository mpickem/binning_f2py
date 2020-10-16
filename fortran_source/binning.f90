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
  subroutine binMatrix_r(inMatrix, vec1, vec2, outMatrix, inX, inY, vec1length, vec2length)
    implicit none
    integer, intent(in) :: inX, inY, vec1length, vec2length

    real(dp), dimension(inX, inY), intent(in) :: inMatrix
    real(dp), dimension(vec1length, vec2length), intent(inout) :: outMatrix

    integer, dimension(vec1length), intent(in) :: vec1
    integer, dimension(vec2length), intent(in) :: vec2

    integer :: i,j
    integer :: ii,jj
    real(dp) :: tempsum
    integer :: vecsum

    integer :: xbegin, xend
    integer :: ybegin, yend

    ! check vector lengths

    vecsum = 0
    do i=1,vec1length
      vecsum = vecsum + vec1(i)
    enddo

    if (vecsum /= inX) then
      write(*,*) 'WARNING: inMatrix x-size inconsistent with x vector'
    endif


    vecsum = 0
    do i=1,vec2length
      vecsum = vecsum + vec2(i)
    enddo

    if (vecsum /= inY) then
      write(*,*) 'WARNING: inMatrix y-size inconsistent with y vector'
    endif


    ! we scan through the coarse grid
     do j=1,vec2length
       do i=1,vec1length

         ! determine index ranges
         xbegin = 1
         do ii=1,i-1
           xbegin = xbegin + vec1(ii)
         enddo
         xend = xbegin + vec1(i) - 1

         ybegin = 1
         do jj=1,j-1
           ybegin = ybegin + vec2(jj)
         enddo
         yend = ybegin + vec2(j) - 1


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

  subroutine binMatrix_c(inMatrix, vec1, vec2, outMatrix, inX, inY, vec1length, vec2length)
    implicit none
    integer, intent(in) :: inX, inY, vec1length, vec2length

    complex(dp), dimension(inX, inY), intent(in) :: inMatrix
    complex(dp), dimension(vec1length, vec2length), intent(inout) :: outMatrix

    integer, dimension(vec1length), intent(in) :: vec1
    integer, dimension(vec2length), intent(in) :: vec2

    integer :: i,j
    integer :: ii,jj
    complex(dp) :: tempsum
    integer :: vecsum

    integer :: xbegin, xend
    integer :: ybegin, yend

    ! check vector lengths

    vecsum = 0
    do i=1,vec1length
      vecsum = vecsum + vec1(i)
    enddo

    if (vecsum /= inX) then
      write(*,*) 'WARNING: inMatrix x-size inconsistent with x vector'
    endif


    vecsum = 0
    do i=1,vec2length
      vecsum = vecsum + vec2(i)
    enddo

    if (vecsum /= inY) then
      write(*,*) 'WARNING: inMatrix y-size inconsistent with y vector'
    endif


    ! we scan through the coarse grid
     do j=1,vec2length
       do i=1,vec1length

         ! determine index ranges
         xbegin = 1
         do ii=1,i-1
           xbegin = xbegin + vec1(ii)
         enddo
         xend = xbegin + vec1(i) - 1

         ybegin = 1
         do jj=1,j-1
           ybegin = ybegin + vec2(jj)
         enddo
         yend = ybegin + vec2(j) - 1


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
