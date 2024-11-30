! ###################################################################
! Copyright (c) 2016-2024, Marc De Graef Research Group/Carnegie Mellon University
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without modification, are 
! permitted provided that the following conditions are met:
!
!     - Redistributions of source code must retain the above copyright notice, this list 
!        of conditions and the following disclaimer.
!     - Redistributions in binary form must reproduce the above copyright notice, this 
!        list of conditions and the following disclaimer in the documentation and/or 
!        other materials provided with the distribution.
!     - Neither the names of Marc De Graef, Carnegie Mellon University nor the names 
!        of its contributors may be used to endorse or promote products derived from 
!        this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
! ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE 
! LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR 
! SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER 
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, 
! OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE 
! USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
! ###################################################################

program EMplay
  !! author: MDG
  !! version: 1.0 
  !! date: 02/26/24
  !!
  !! test program to look at triple junctions and fundamental zones

use mod_kinds
use mod_global
use mod_EMsoft
use mod_rotations
use mod_quaternions
use mod_math
use mod_povray
use mod_so3
use mod_io
use mod_symmetry
use mod_crystallography
use HDF5
use mod_HDFsupport
use bspline_module
use bspline_kinds_module, only: wp, ip

IMPLICIT NONE

interface
    function getShapeFunction(h) result(W)

    use bspline_kinds_module, only: wp

    real(wp), INTENT(IN)        :: h(8)
    real(wp)                    :: W(3,3)

    end function getShapeFunction

    function getHomography(W) result(h)

    use bspline_kinds_module, only: wp

    real(wp), INTENT(IN)        :: W(3,3)
    real(wp)                    :: h(8)

    end function getHomography

    function matrixInvert(a) result(b)

    use bspline_kinds_module, only: wp
    use mod_io

    real(wp), INTENT(IN)    :: a(3,3)
    real(wp)                :: b(3,3)

    end function matrixInvert

    function Fe2homography(Fe_input, PC, DD) result(h)

    use bspline_kinds_module, only: wp

    real(wp), INTENT(IN)        :: Fe_input(0:2,0:2)
    real(wp), INTENT(IN)        :: PC(0:1)
    real(wp), INTENT(IN)        :: DD
    real(wp)                    :: h(8)

    end function Fe2homography

    function homography2Fe(p, PC, DD) result(W)

    use bspline_kinds_module, only: wp

    real(wp), INTENT(IN)        :: p(0:7)
    real(wp), INTENT(IN)        :: PC(0:1)
    real(wp), INTENT(IN)        :: DD
    real(wp)                    :: W(3,3)

    end function homography2Fe
end interface 



character(fnlen)             :: progname = 'EMplay.f90'
character(fnlen)             :: progdesc = 'test program for HR-EBSD-DIC'

type(EMsoft_T)               :: EMsoft
type(bspline_2d)             :: sref, sdef
type(q_T)                    :: qu 
type(o_T)                    :: om 
type(e_T)                    :: eu 

real(kind=wp), allocatable   :: refpat(:,:), defpat(:,:), x(:), y(:), gradx(:,:), grady(:,:), &
                                referenceSR(:), targetSR(:), gxSR(:), gySR(:), refzmn(:), tarzmn(:), &
                                GradJac(:,:), xiX(:), xiY(:), XiPrime(:,:), targetdef(:,:), residuals(:), &
                                wtarget(:)
real(kind=wp)                :: DD, PCx, PCy, val, err, errmax, rnxi, rnyi, hg(8), W(3,3), gradCIC(8), &
                                refmean, refstdev, tarmean, tarstdev, CIC, Hessian(8,8), GJ(8), minx, miny, &
                                xi1max, xi2max, normdp, oldnorm, oldW(3,3), horiginal(8)
real(kind=dbl)               :: Wnew(3,3), Winv(3,3), dx, dy, p2(3), Woriginal(3,3), alp, srt(3,3), srtrot(3,3)
integer(kind=irg)            :: nx, ny, nxy, nbx, nby, i, ii, j, NSR, cnt, nxSR, nySR  
integer(ip),parameter        :: kx = 5, ky = 5    ! spline order
integer(ip)                  :: iflag
real(wp)                     :: tol

! LAPACK variables
character                    :: UPLO ='U'
integer                      :: NN = 8
integer                      :: NRHS = 1
real(wp)                     :: Hessiancopy(8,8)
integer                      :: LDA = 8
real(wp)                     :: SOL(8,1)
integer                      :: LDB = 8
integer                      :: INFO 


EMsoft = EMsoft_T( progname, progdesc )

call setRotationPrecision('d')

! let's test the second rank tensor transformation routine
alp = 45.D0 * dtor 
qu = q_T( qdinp = (/ cos(alp*0.5D0), 0.D0, 0.D0, sin(alp*0.5D0) /) )
om = qu%qo()
Wnew = om%o_copyd() 

call om%o_print(str = ' rotation matrix : ')

write(*,*) ' extracted rotation matrix : '
do i=1,3
    write(*,*) Wnew(i,1:3)
end do


! create a symmetric second rank tensor
srt = reshape( (/ 1.D0, 2.D0, 3.D0, 2.D0, 4.D0, 5.D0, 3.D0, 5.D0, 8.D0/), (/ 3,3 /) )

write(*,*) ' second rank tensor : '
do i=1,3
    write(*,*) srt(i,1:3)
end do

srtrot = TransSecondRankTensor(srt, Wnew, Wnew)

write(*,*) ' second rank tensor (rotated) : '
do i=1,3
    write(*,*) srtrot(i,1:3)
end do

alp = 90.D0 * dtor 
qu = q_T( qdinp = (/ cos(alp*0.5D0), 0.D0, 0.D0, sin(alp*0.5D0) /) )
om = qu%qo()
Wnew = om%o_copyd() 
srtrot = TransSecondRankTensor(srt, Wnew)
write(*,*) ' second rank tensor (rotated by single 90°) : '
do i=1,3
    write(*,*) srtrot(i,1:3)
end do



stop


! this program is a first conversion of the IDL HR-EBSD-DIC code into f90; we'll test
! everything here before turning it into a fullblown EMsoftOO program. We will use the IDL 
! convention of starting arrays at coordinate 0 for ease of comparison.  Also, keep in mind
! that fortran arrays are column-based whereas IDL is row-based...

! read the reference pattern from a binary file; the patterns have been 
! pre-processed and scaled between 0.D0 and 1.D0
nx = 640
ny = 480
nxy = nx * ny

allocate(refpat(0:nx-1,0:ny-1), defpat(0:nx-1,0:ny-1))
! open(20,file='/Users/mdg/playarea/DIC/refpat.data',status='old',form='unformatted')
open(20,file='/Users/mdg/Files/EMPlay/playarea/DIC/refpat.data',status='old',form='unformatted')
read(20) refpat
close(20,status='keep')

! define some parameters for now...
tol = 100 * epsilon(1.0_wp)
PCx = real(nx/2-1,wp)
PCy = real(ny/2-1,wp)
DD = 15000.0_wp/50.0_wp 

! generate the reference coordinate grid; these are two separate line arrays at this point
write(*,*) ' creating coordinate arrays x and y'
allocate(x(0:nx-1), y(0:ny-1))
rnxi = 1.0_wp/real(nx-1,wp)
rnyi = 1.0_wp/real(ny-1,wp)
do i=0,nx-1
    x(i) = real(i,wp)
end do
x = x * rnxi
do j=0,ny-1
    y(j) = real(j,wp)
end do
y = y * rnyi

! initialize the spline classes stef and sdef
! all interpolations on the deformed region will be done on the whole
! pattern and then the subregion will be extracted for comparison with 
! the reference subregion
write(*,*) ' initializing spline coefficients of order ', kx, ky
call sref%initialize(x,y,refpat,kx,ky,iflag)

! here we deform the reference pattern to obtain a defpat array with known homography
! define the homography hg
horiginal = (/ 0.0002_wp, 0.0001_wp, -0.0004_wp, -0.0001_wp, -0.0003_wp, 0.0005_wp, 0.0_wp, 0.0_wp /)
! convert to shape function
W = getShapeFunction(horiginal)
! determine the XiPrime coordinates so that we can apply the deformation by
! means of the evaluate method in the bspline class
allocate( XiPrime(0:1, 0:nx*ny-1))
cnt = 0
do j = 0, ny-1
    do i = 0, nx-1
        p2 = matmul( W,  (/ x(i)-0.5_wp, y(j)-0.5_wp, 1.0_wp /) )
        if (p2(3).ne.0.0_wp) then 
            XiPrime(0:1,cnt) = p2(1:2)/p2(3)
        else
            XiPrime(0:1,cnt) = p2(1:2)
        end if
        cnt = cnt + 1
    end do 
end do 
! and interpolate/extrapolate the deformed pattern as needed
call sref%set_extrap_flag(.TRUE.)
cnt = 0
do j=0,ny-1
    do i=0,nx-1
        iflag = 0_wp
        call sref%evaluate(XiPrime(0,cnt)+0.5_wp,XiPrime(1,cnt)+0.5_wp,0,0,defpat(i,j),iflag)
        if (iflag.ne.0) write(*,*) 'iflag : ', iflag, i, j
        cnt = cnt+1
   end do
end do
! write (*,*) ' defpat: ', minval(defpat), maxval(defpat)
defpat = defpat - minval(defpat)
defpat = defpat/maxval(defpat)
! finally get the b-spline coefficients for the deformed pattern
call sdef%initialize(x,y,defpat,kx,ky,iflag)
deallocate( XiPrime )

Woriginal = W

! get the derivatives of the reference pattern to determine the Hessian
write(*,*) ' computing gradient arrays'
allocate( gradx(0:nx-1,0:ny-1), grady(0:nx-1,0:ny-1) )
do i=0,nx-1
   do j=0,ny-1
        ! determine how well the spline coefficients represent the original pattern
        call sref%evaluate(x(i),y(j),0,0,val,iflag)
        err = abs(refpat(i,j)-val)
        errmax = max(err,errmax)
        ! and extract the gradient arrays
        call sref%evaluate(x(i),y(j),1,0,gradx(i,j),iflag)
        call sref%evaluate(x(i),y(j),0,1,grady(i,j),iflag)
   end do
end do

! open(20,file='patterns.data',status='unknown',form='unformatted')
! write(20) real(refpat)
! write(20) real(gradx)
! write(20) real(grady)
! close(20)

! check max error against tolerance
write(*,*) ' max b-spline reconstruction error of reference pattern:', errmax
if (errmax >= tol) then
    write(*,*)  ' ** test failed ** '
else
    write(*,*)  ' ** test passed ** '
end if
write(*,*) ''
write(*,*) ' range(gradx) : ', minval(gradx), maxval(gradx)
write(*,*) ' range(grady) : ', minval(grady), maxval(grady)

! define the border widths nbx and nby for the subregion
nbx = 30
nby = 30
nxSR = nx-2*nbx
nySR = ny-2*nby
NSR = nxSR * nySR

! extract the subregions from the reference and gradient arrays
write(*,*) ' creating subregion arrays '
allocate( referenceSR(0:NSR-1), gxSR(0:NSR-1), gySR(0:NSR-1), refzmn(0:NSR-1), tarzmn(0:NSR-1) )
referenceSR = reshape( refpat(nbx:nx-nbx-1,nby:ny-nby-1), (/ NSR /) )
gxSR = reshape( gradx(nbx:nx-nbx-1,nby:ny-nby-1), (/ NSR /) )
gySR = reshape( grady(nbx:nx-nbx-1,nby:ny-nby-1), (/ NSR /) )

! zero-mean and normalize the referenceSR array
write(*,*) ' zero-mean and normalization of subregion arrays '
refmean = sum(referenceSR)/dble(NSR)
refstdev = sqrt( sum( (referenceSR - refmean)**2 ) )
refzmn = (referenceSR-refmean)/refstdev 
write(*,*) '   reference ', refmean, refstdev, sum(refzmn)/dble(NSR), &
                            sqrt( sum( (refzmn - sum(refzmn)/dble(NSR))**2 ) )

! repeat for the targetSR so that we can compute the initial CIC
tarmean = sum(defpat(nbx:nx-nbx-1,nby:ny-nby-1))/dble(NSR)
tarstdev = sqrt( sum( (defpat(nbx:nx-nbx-1,nby:ny-nby-1) - tarmean)**2 ) )
tarzmn = reshape( (defpat(nbx:nx-nbx-1,nby:ny-nby-1)-tarmean)/tarstdev, (/NSR /) ) 
write(*,*) '   target    ', tarmean, tarstdev, sum(tarzmn)/dble(NSR), &
                            sqrt( sum( (tarzmn - sum(tarzmn)/dble(NSR))**2 ) )

! initial CIC function (to be minimized ...)
CIC = sum( (refzmn-tarzmn)**2 )
write (*,*) ' initial CIC value : ', CIC

! next we compute the GradJac array and the Hessian using the standard grid shifted 
! by the Pattern Center coordinates
write(*,*) ' computing GradJac and Hessian arrays'
allocate( xiX(0:nxSR-1), xiY(0:nySR-1) )
xiX = x(nbx:nx-nbx-1) - 0.5_wp ! PCx
xiY = y(nby:ny-nby-1) - 0.5_wp ! PCy
write(*,*) ' range(xiX) : ', minval(xiX), maxval(xiX), shape(xiX), PCx
write(*,*) ' range(xiY) : ', minval(xiY), maxval(xiY), shape(xiY), PCy

allocate( GradJac(0:7,0:NSR-1) )
cnt = 0
Hessian = 0.0_wp
do j = 0, nySR-1
    do i = 0, nxSR-1
        GJ = (/ gxSR(cnt)*xiX(i), gxSR(cnt)*xiY(j), gxSR(cnt), & 
                gySR(cnt)*xiX(i), gySR(cnt)*xiY(j), gySR(cnt), &
                gxSR(cnt)*xiX(i)**2-gySR(cnt)*xiX(i)*xiY(j), &
                gySR(cnt)*xiY(j)**2-gxSR(cnt)*xiX(i)*xiY(j) /)
        GradJac(0:7,cnt) = GJ
        Hessian = Hessian + matmul( reshape(GJ, (/8,1/)), reshape(GJ, (/1,8/)) ) 
        cnt = cnt + 1
    end do 
end do 
Hessian = Hessian * 2.0_wp / refstdev**2

write(*,*) ' final cnt : ', cnt-1, NSR

write(*,*) ' Hessian :'
write(*,*) Hessian

write(*,*) ' range(GradJac) : ', minval(GradJac), maxval(GradJac)

! initialize the homography coefficients to zero
! in a more general implementation we might initialize them to the 
! values for one of the nearest neighbor patterns
hg = (/ (0.0_wp, i=1,8) /)
W = getShapeFunction(hg)
write (*,*) W 

write (*,*) getHomography(W)

allocate( targetdef(0:nxSR-1,0:nySR-1), residuals(0:NSR-1), wtarget(0:NSR-1) )

! allow for extrapolation in addition to interpolation... 
! this required a change to the original bspline-fortran code; the
! modified code is in a personal fork of the package which is 
! installed during the EMsoftOO super build process.
call sdef%set_extrap_flag(.TRUE.)

dx = abs(xiX(2) - xiX(1))
dy = abs(xiY(2) - xiY(1))

allocate( XiPrime(0:1, 0:NSR-1) )

oldnorm = 100.0_wp

! and here we start the loop 
do ii=1,100 
    write (*,*) ' iteration # ',ii
    ! get XiPrime 
    cnt = 0
    do j = 0, nySR-1
        do i = 0, nxSR-1
            p2 = matmul( W,  (/ xiX(i), xiY(j), 1.0_wp /) )
            XiPrime(0:1,cnt) = p2(1:2)/p2(3)
            cnt = cnt + 1
        end do 
    end do 

! apply this deformation to the target pattern using b-splines
    targetdef = 0.D0 
    cnt = 0
    do j=0,nySR-1
        do i=0,nxSR-1
            iflag = 0_wp
            ! call sdef%evaluate(XiPrime(0,cnt)-minx,XiPrime(1,cnt)-miny,0,0,targetdef(i,j),iflag)
            call sdef%evaluate(XiPrime(0,cnt)+0.5_wp,XiPrime(1,cnt)+0.5_wp,0,0,targetdef(i,j),iflag)
            if (iflag.ne.0) write(*,*) 'iflag : ', iflag, i, j
            cnt = cnt+1
       end do
    end do

    ! extract the subregion
    wtarget = reshape( targetdef, (/ NSR /) )
    ! zero-mean and normalize
    tarmean = sum(wtarget)/dble(NSR)
    tarstdev = sqrt( sum( (wtarget - tarmean)**2 ) )
    tarzmn =  (wtarget-tarmean)/tarstdev
    ! residuals
    residuals = refzmn - tarzmn
    CIC = sum( residuals**2 )
    write (*,*) ' range(residuals) : ', minval(residuals), maxval(residuals)
    write (*,*) ' new CIC value : ', CIC 
    ! gradCIC
    gradCIC = matmul(GradJac, residuals)
    gradCIC = gradCIC * 2.0_wp/refstdev
    write (*,*) ' gradCIC :', shape(GradJac), shape(residuals), shape(gradCIC)
    write (*,*) gradCIC
    ! solve the Hessian equation using Cholesky decomposition (LAPACK)
    Hessiancopy = Hessian
    SOL(1:8,1) = -gradCIC(1:8)
    call DPOSV(UPLO, NN, NRHS, Hessiancopy, LDA, SOL, LDB, INFO)
    ! extract the solution vector from SOL
    write (*,*) ' DPOSV INFO = ', INFO
    write (*,*) ' Solution vector : '
    write (*,*) SOL 
    xi1max = maxval( XiPrime(0,:) )
    xi2max = maxval( XiPrime(1,:) )
    normdp = sqrt(xi1max * (SOL(1,1)**2+SOL(4,1)**2+SOL(7,1)**2) + & 
                  xi2max * (SOL(2,1)**2+SOL(5,1)**2+SOL(8,1)**2) + SOL(3,1)**2 + SOL(6,1)**2)

    ! convert to a shape function and take the inverse
    Wnew = getShapeFunction(reshape(SOL, (/8/)))
    Winv = matrixInvert( Wnew )
    W = matmul( W, Winv )
    W = W / W(3,3)
    write (*,*) getHomography(W)
    write (*,*) ' norm(deltap) = ', normdp
    write (*,*) '------------'
    if (normdp.lt.oldnorm) then
        oldnorm = normdp
        oldW = W 
    else
        W = oldW
        EXIT
    end if
end do 

W = matrixInvert( W )
W = W / W(3,3)
write (*,*) '' 
write (*,*) ' Final homography : '
write (*,*) getHomography(W)
write (*,*) '' 
write (*,*) ' Target homography : '
write (*,*) horiginal


call sdef%destroy()
call sref%destroy()

end program EMplay



! functions and subroutines ... 




function getShapeFunction(h) result(W)

use bspline_kinds_module, only: wp

real(wp), INTENT(IN)        :: h(8)
real(wp)                    :: W(3,3)

W(1,1) = 1.0_wp + h(1)
W(2,2) = 1.0_wp + h(5)
W(3,3) = 1.0_wp

W(1,2:3) = h(2:3)
W(2,1) = h(4)
W(2,3) = h(6)
W(3,1:2) = h(7:8)

end function getShapeFunction


function getHomography(W) result(h)

use bspline_kinds_module, only: wp

real(wp), INTENT(IN)        :: W(3,3)
real(wp)                    :: h(8)

h = (/ W(1,1)-1.0_wp, W(1,2), W(1,3), W(2,1), W(2,2)-1.0_wp, W(2,3), W(3,1), W(3,2) /)

end function getHomography




function matrixInvert(a) result(b)

use bspline_kinds_module, only: wp
use mod_io

real(wp), INTENT(IN)    :: a(3,3)
real(wp)                :: b(3,3)

type(IO_T)              :: Message
real(wp)                :: d 

d = a(1,1)*a(2,2)*a(3,3)+a(1,2)*a(2,3)*a(3,1)+ &
     a(1,3)*a(2,1)*a(3,2)-a(1,3)*a(2,2)*a(3,1)- &
     a(1,2)*a(2,1)*a(3,3)-a(1,1)*a(2,3)*a(3,2)
if (d.ne.0.D0) then
    b(1,1)=a(2,2)*a(3,3)-a(2,3)*a(3,2)
    b(1,2)=a(1,3)*a(3,2)-a(1,2)*a(3,3)
    b(1,3)=a(1,2)*a(2,3)-a(1,3)*a(2,2)
    b(2,1)=a(2,3)*a(3,1)-a(2,1)*a(3,3)
    b(2,2)=a(1,1)*a(3,3)-a(1,3)*a(3,1)
    b(2,3)=a(1,3)*a(2,1)-a(1,1)*a(2,3)
    b(3,1)=a(2,1)*a(3,2)-a(2,2)*a(3,1)
    b(3,2)=a(1,2)*a(3,1)-a(1,1)*a(3,2)
    b(3,3)=a(1,1)*a(2,2)-a(1,2)*a(2,1)
    b = b/d
else
    do i=1,3
      write (*,*) (a(i,j),j=1,3)
    end do
    !  call Message%printError('mInvert','matrix has zero determinant')
    call Message%printMessage('mInvert: matrix has zero determinant')
    b = a
end if

end function matrixInvert


function homography2Fe(p, PC, DD) result(W)

use bspline_kinds_module, only: wp

real(wp), INTENT(IN)        :: p(0:7)
real(wp), INTENT(IN)        :: PC(0:1)
real(wp), INTENT(IN)        :: DD
real(wp)                    :: W(3,3)

real(wp)                    :: beta0, Fe11, Fe12, Fe13, Fe21, Fe22, Fe23, Fe31, Fe32, Fe33

! h11, h12, h13, h21, h22, h23, h31, h32
!  0    1    2    3    4    5    6    7

beta0 = 1.0_wp - p(6) * PC(0) - p(7) * PC(1)
Fe11 = 1.0_wp + p(0) + p(6) * PC(0)
Fe12 = p(1) + p(7) * PC(0)
Fe13 = (p(2) - p(0)*PC(0) - p(1)*PC(1) + PC(0)*(beta0 - 1.0_wp))/DD
Fe21 = p(3) + p(6) * PC(1)
Fe22 = 1.0_wp + p(4) + p(7) * PC(1)
Fe23 = (p(5) - p(3)*PC(0) - p(4)*PC(1) + PC(1)*(beta0 - 1.0_wp))/DD
Fe31 = DD * p(6)
Fe32 = DD * p(7)
Fe33 = beta0

W = reshape( (/ Fe11, Fe12, Fe13, Fe21, Fe22, Fe23, Fe31, Fe32, Fe33 /), (/ 3,3 /)) / beta0

end function homography2Fe


function Fe2homography(Fe_input, PC, DD) result(h)

use bspline_kinds_module, only: wp

real(wp), INTENT(IN)        :: Fe_input(0:2,0:2)
real(wp), INTENT(IN)        :: PC(0:1)
real(wp), INTENT(IN)        :: DD
real(wp)                    :: h(8)

real(wp)                    :: Fe(0:2,0:2), g0, g11, g22, g13, g23, h11, h12, h13, h21, h22, h23, h31, h32

Fe = Fe_input/Fe_input(2,2)

g0 = DD + Fe(2,0) * PC(0) + Fe(2,1) * PC(1)
g11 = DD * Fe(0,0) - Fe(2,0) * PC(0) - g0
g22 = DD * Fe(1,1) - Fe(2,1) * PC(1) - g0
g13 = DD * ((Fe(0,0) - 1.D0) * PC(0) + Fe(0,1) * PC(1) + Fe(0,2) * DD) + PC(0) * (DD - g0)
g23 = DD * (Fe(1,0) * PC(0) + (Fe(1,1) - 1.D0) * PC(1) + Fe(1,2) * DD) + PC(1) * (DD - g0)
h11 = g11
h12 = DD * Fe(0,1) - Fe(2,1) * PC(0)
h13 = g13
h21 = DD * Fe(1,0) - Fe(2,0) * PC(1)
h22 = g22
h23 = g23
h31 = Fe(2,0)
h32 = Fe(2,1)

h = (/ h11, h12, h13, h21, h22, h23, h31, h32 /) / g0

end function Fe2homography



