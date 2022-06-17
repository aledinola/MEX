#include "fintrf.h"
!----------------------------------------------------------------!
! Author: Alessandro Di Nola. 12/2021.
! Gateway routine
! Compile with mex -O -v -R2018a myinterp1q.f90 -output myinterp1q
! or, if you want to modify optimization falgs, with 
! mex -v OPTIMFLAGS="/O3 /DNDEBUG" -R2018a myinterp1q.f90 -output myinterp1q
! or even more aggressive
! mex -v OPTIMFLAGS="/O3 /Qprec-div- /QxHost /DNDEBUG" -R2018a myinterp1q.f90 -output myinterp1q
!! USAGE in Matlab: yi = myinterp1q(x,y,xi)
!----------------------------------------------------------------!
subroutine mexFunction(nlhs,plhs,nrhs,prhs)
    implicit none 
    ! Arguments in mexFunction
    mwPointer :: plhs(*), prhs(*)
    integer   :: nlhs, nrhs
    
    ! Function declarations
    mwPointer :: mxGetPr, mxCreateDoubleMatrix, mxGetM, mxGetN
    integer   :: mxIsNumeric
    
    !  Array information:
    mwPointer :: nx, ny2, nxi
    
    ! Pointers to input arrays
    mwPointer :: x_ptr, y_ptr, xi_ptr
    ! Pointers to output arrays
    mwPointer :: yi_ptr
    
    ! Arguments for computational routine
    ! These are not necessary anymore!
    !real(8), allocatable :: x_input(:), y_output(:)
    
    ! Others
    character (len=1) :: c
    character (len=50) :: txt
!----------------------------------------------------------------!
    
    ! Check for proper number of arguments. 
    if(nrhs /= 3) then
        call mexErrMsgIdAndTxt('MATLAB:myinterp1q:nInput','Three inputs required.')
    elseif(nlhs > 1) then
        call mexErrMsgIdAndTxt('MATLAB:myinterp1q:nOutput','Too many output arguments.')
    endif
    
    ! Check that the input is a number.
    if(mxIsNumeric(prhs(1)) == 0) then
        call mexErrMsgIdAndTxt('MATLAB:myinterp1q:NonNumeric','Input must be a number.')
    endif
    
    !Get dimensions of 1st input X, dim (nx,1)
    nx = mxGetM(prhs(1)) 
    
    !Get dimensions of 2nd input Y, dim (nx,ny2)
    ny2 = mxGetN(prhs(2))
    
    !Get dimensions of 3rd input Xi, dim (nxi,1)
    nxi = mxGetM(prhs(3))
    
    !! DEBUG Display on screen 
    !! Specify alert to be printed
    !write(txt,'(A)') 'Number of columns of input vector (mexPrintf): ' 
    !! Convert nrhs to char
    !write(c,'(i1)') ncols
    !! Call the API
    !call mexPrintf(txt)
    !call mexPrintf(c)
    
    ! Create a matrix for the return argument Yi
    plhs(1) = mxCreateDoubleMatrix(nxi,ny2,int(0,4))
    
    ! Get pointers for input arguments
    ! 1 = x, 2 = y, 3 = xi
    x_ptr  = mxGetPr(prhs(1))
    y_ptr  = mxGetPr(prhs(2))
    xi_ptr = mxGetPr(prhs(3))
    
    ! Get pointer for output argument
    yi_ptr = mxGetPr(plhs(1))
    
    ! Call the computational subroutine (LHS: output, RHS: inputs)
    call myinterp1q(%val(yi_ptr), %val(x_ptr),%val(y_ptr),%val(xi_ptr),nx,ny2,nxi)
    
end subroutine mexFunction
!============================================================================!

module utils
    implicit none
    
contains
    
subroutine bsearch(ilo,xhat,x)
    ! Use binary search to find the lowerbound on a bracketing interval for 
    ! Given sorted ascending array, find lowerbound on bracketing interval. Note that
    ! ilo will always be in 1,...,size(x)-1.
    ! TODO Write bsearch_equi for equi-spaced grid
    implicit none
    real(8), intent(in)  :: x(:) ! Look up table
    real(8), intent(in)  :: xhat ! Value whose nearest neighbors are to be found
    integer, intent(out) :: ilo  ! returns ilo if x(ilo)<xhat<x(ilo+1)
    !local
    integer :: n,ihi,mid
        
    ilo = 1
    ihi = size(x)
    mid = (ihi+ilo)/2 ! mid will have ilo<=mid<ihi as long as ihi>ilo. If ihi=ilo, then mid=ilo, so the 
                      ! while loop is not entered and ilo=1 is returned 
    
    do while (mid>ilo) ! mid=ilo means ihi=ilo+1.

        ! Here, we must have ihi>ilo and ilo<=mid<ihi

        ! Bisect. 
        ! If xhat<x(1), then xhat>=x(mid) will never occur and ilo will never be modified.
        ! If xhat>=x(nx), then xhat>=x(mid) will always occur (note mid<ihi). Consequently, 
        !      ilo = mid occurs until mid==ilo, which only occures if ihi==ilo+1.
        if (xhat>=x(mid)) then
            ilo = mid
        else
            ihi = mid
        end if

        mid = (ihi+ilo)/2 

    end do

    ! Check the result as a precaution.  Make sure x in fact lies between.
    ! If this has a severe negative performance impact, I should get rid of this
    ! if (xhat<x(1) .and. ilo/=1) STOP 'bsearch: wrong case 1'
    ! if (xhat>x(size(x)) .and. ilo/=size(x)-1) STOP 'bsearch: wrong case 2'
    ! if (size(x)==1 .and. ilo/=1) STOP 'bsearch: wrong case 3'
    ! if (size(x)>=2 .and. xhat>=x(1) .and. xhat<=x(size(x)) .and. (xhat<x(ilo) .or. xhat>x(ilo+1))) STOP 'bsearch: wrong case 4'

end subroutine bsearch

end module utils
    
subroutine myinterp1q(yi,x,y,xi,nx,ny2,nxi)
    ! USAGE in Matlab: yi = myinterp1q(x,y,xi)
    ! Subroutine that takes a 1-D VECTOR x and doubles it
    use utils, only: bsearch
    implicit none 
    mwSize, intent(in)  :: nx, ny2, nxi
    real(8), intent(in)  :: x(nx)       ! Grid of values
    real(8), intent(in)  :: y(nx,ny2)   ! Function values
    real(8), intent(in)  :: xi(nxi)     ! Query points
    real(8), intent(out) :: yi(nxi,ny2) ! Output
    ! Local variables
    integer :: i, ilo
    
    ! Loop over xi and interpolate
    do i = 1,nxi
        ! Find the location of xi(i) in the x grid assuming it is sorted
        call bsearch(ilo,xi(i),x)
        ! Do the interpolation
        yi(i,:) = y(ilo,:) + (y(ilo+1,:)-y(ilo,:))*(xi(i)-x(ilo))/(x(ilo+1)-x(ilo))
    enddo
    
end subroutine myinterp1q
    
!============================================================================!
    

    

    
