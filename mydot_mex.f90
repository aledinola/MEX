#include "fintrf.h"
!----------------------------------------------------------------!
! Gateway routine
! Compile with mex -O -v -R2018a mydot_mex.f90 -output mydot_mex
!! USAGE in Matlab: z = mydot_mex(x,y)
! INPUTS: x,y are vectors
! OUTPUT: z is scalar
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
    mwSize :: nx1,nx2,nx,ny1,ny2,ny,nz
    
    ! Pointers to input arrays
    mwPointer :: x_ptr, y_ptr
    ! Pointers to output arrays
    mwPointer :: z_ptr
    
    ! Arguments for computational routine
    ! These are not necessary anymore!
    !real(8), allocatable :: x_input(:), y_output(:)
    
    ! Others
    character (len=1) :: c
    character (len=50) :: txt
!----------------------------------------------------------------!
    
    ! Check for proper number of arguments. 
    if(nrhs /= 2) then
        call mexErrMsgIdAndTxt('MATLAB:mydot_mex:nInput','Two inputs required.')
    elseif(nlhs > 1) then
        call mexErrMsgIdAndTxt('MATLAB:mydot_mex:nOutput','Too many output arguments.')
    endif
    
    ! Check that the input is a number.
    if(mxIsNumeric(prhs(1)) == 0) then
        call mexErrMsgIdAndTxt('MATLAB:mydot_mex:NonNumeric','Input must be a number.')
    elseif(mxIsNumeric(prhs(2)) == 0) then
        call mexErrMsgIdAndTxt('MATLAB:mydot_mex:NonNumeric','Input must be a number.')
    endif
    
    !Get dimensions of 1st input X, row or column vector
    nx1 = mxGetM(prhs(1))
    nx2 = mxGetN(prhs(1))
    nx  = max(nx1,nx2)
    
    !Check that X is column vector
    !if (nx2>1) then
    !    call mexErrMsgIdAndTxt('MATLAB:mydot_mex:','Input must be a column vector.')
    !endif
    
    !Get dimensions of 2nd input Y, row or column vector
    ny1 = mxGetM(prhs(2))
    ny2 = mxGetN(prhs(2))
    ny  = max(ny1,ny2)
    
    !Check that X and Y are compatible
    if (nx /= ny) then
        call mexErrMsgIdAndTxt('MATLAB:mydot_mex:nInput','X and Y must be same size.')
    endif
    
    !! DEBUG Display on screen 
    !! Specify alert to be printed
    !write(txt,'(A)') 'Number of columns of input vector (mexPrintf): ' 
    !! Convert nrhs to char
    !write(c,'(i1)') ncols
    !! Call the API
    !call mexPrintf(txt)
    !call mexPrintf(c)
    
    ! Create a matrix for the return argument Z
    nz = 1 ! dot product always returns a scalar
    plhs(1) = mxCreateDoubleMatrix(nz,nz,int(0,4))
    
    ! Get pointers for input arguments
    ! 1 = x, 2 = y
    x_ptr  = mxGetPr(prhs(1))
    y_ptr  = mxGetPr(prhs(2))
    
    ! Get pointer for output argument
    z_ptr = mxGetPr(plhs(1))
    
    ! Call the computational subroutine (LHS: output, RHS: inputs)
    call mydot_mex(%val(z_ptr), %val(x_ptr),%val(y_ptr),nx)
    
end subroutine mexFunction
!============================================================================!
    
subroutine mydot_mex(z,x,y,n)
    ! USAGE in Matlab: z = mydot_mex(x,y)
    implicit none 
    mwSize, intent(in)   :: n           ! Length of vectors x,y
    real(8), intent(in)  :: x(n),y(n)   ! Input vectors
    real(8), intent(out) :: z           ! Output
    ! Local variables
    integer :: i
    
    ! Compute the dot product
    z = 0d0
    do i = 1,n
        z = z+x(i)*y(i)
    enddo
    
end subroutine mydot_mex
    
!============================================================================!
