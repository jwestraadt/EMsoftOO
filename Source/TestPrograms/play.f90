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
use mod_DIC
use HDF5
use mod_HDFsupport
use bspline_module
use bspline_kinds_module, only: wp, ip

IMPLICIT NONE

character(fnlen)             :: progname = 'EMplay.f90'
character(fnlen)             :: progdesc = 'test program for HR-EBSD-DIC'

type(EMsoft_T)               :: EMsoft
type(DIC_T)                  :: DIC
type(q_T)                    :: qu 
type(o_T)                    :: om 
type(e_T)                    :: eu 

real(kind=wp), allocatable   :: refpat(:,:), defpat(:,:)
real(kind=wp)                :: DD, PCx, PCy, val, err, errmax, rnxi, rnyi, hg(8), W(3,3), gradCIC(8), Hessian(8,8), &
                                minx, miny, xi1max, xi2max, normdp, oldnorm, oldW(3,3), horiginal(8), CIC, sol(8)
real(kind=dbl)               :: Wnew(3,3), Winv(3,3), dx, dy, p2(3), Woriginal(3,3), alp, srt(3,3), srtrot(3,3)
integer(kind=irg)            :: nx, ny, nxy, nbx, nby, i, ii, j, NSR, cnt, nxSR, nySR  
real(wp)                     :: tol


EMsoft = EMsoft_T( progname, progdesc )

! this program is a first conversion of the IDL HR-EBSD-DIC code into f90; we'll test
! everything here before turning it into a fullblown EMsoftOO program. We will use the IDL 
! convention of starting arrays at coordinate 0 for ease of comparison.  Also, keep in mind
! that fortran arrays are column-based whereas IDL is row-based...

! read the reference pattern from a binary file; the patterns have been 
! pre-processed and scaled between 0.D0 and 1.D0
nx = 640
ny = 480

! instantiate the DIC class
! this also initializes the x and y coordinate arrays
DIC = DIC_T( nx, ny )

nxy = nx * ny

allocate(refpat(0:nx-1,0:ny-1), defpat(0:nx-1,0:ny-1))
open(20,file='/Users/mdg/playarea/DIC/refpat.data',status='old',form='unformatted')
! open(20,file='/Users/mdg/Files/EMPlay/playarea/DIC/refpat.data',status='old',form='unformatted')
read(20) refpat
close(20,status='keep')

call DIC%setpattern('r', refpat)

! define some parameters for now...
tol = 100 * epsilon(1.0_wp)
PCx = real(nx/2-1,wp)
PCy = real(ny/2-1,wp)
DD = 15000.0_wp/50.0_wp 

! define the border widths nbx and nby for the subregion
nbx = 30
nby = 30
call DIC%defineSR( nbx, nby, 0.5_wp, 0.5_wp)


call DIC%getbsplines(refp=.TRUE., verify=.TRUE.)

! here we deform the reference pattern to obtain a defpat array with known homography
! define the homography hg
horiginal = (/ 0.0002_wp, 0.0001_wp, -0.0004_wp, -0.0001_wp, -0.0003_wp, 0.0005_wp, 0.0_wp, 0.0_wp /)
call DIC%applyHomography(horiginal, 0.5_wp, 0.5_wp)

Woriginal = DIC%getShapeFunction(horiginal)

! get the derivatives of the reference pattern to determine the Hessian
call DIC%getbsplines(defp=.TRUE., grads=.TRUE.)

! zero-mean and normalize the referenceSR and targetSR arrays
call DIC%applyZMN(doreference=.TRUE., dotarget=.TRUE.)

! initial CIC function (to be minimized ...)
call DIC%getresiduals( CIC )
write (*,*) ' initial CIC value : ', CIC

! compute the Hessian matrix
call DIC%getHessian( Hessian )

! initialize the homography coefficients to zero
! in a more general implementation we might initialize them to the 
! values for one of the nearest neighbor patterns
hg = (/ (0.0_wp, i=1,8) /)
W = DIC%getShapeFunction(hg)

oldnorm = 100.0_wp

! and here we start the loop 
do ii=1,100 
    write (*,*) ' iteration # ',ii
    call DIC%applyHomography(hg, 0.5_wp, 0.5_wp, dotarget=.TRUE.)

    ! zero-mean and normalize the deformed sub-region array wtarget
    call DIC%applyZMN( dotarget = .TRUE. )

    ! residuals
    call DIC%getresiduals( CIC )

    ! gradCIC and solve equation
    call DIC%solveHessian(SOL, normdp)

    ! convert to a shape function and take the inverse
    Wnew = DIC%getShapeFunction(reshape(SOL, (/8/)))
    Winv = matrixInvert_wp( Wnew )
    W = matmul( W, Winv )
    W = W / W(3,3)
    hg = DIC%getHomography(W)
    write (*,*) hg
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

W = matrixInvert_wp( W )
W = W / W(3,3)
hg = DIC%getHomography(W)
write (*,*) '' 
write (*,*) ' Final homography : '
write (*,*) DIC%getHomography(W)
write (*,*) '' 
write (*,*) ' Target homography : '
write (*,*) horiginal

write (*,*) ' differences : '
write (*,*) horiginal-hg

end program EMplay
