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
  !! test program 

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
use mod_platformsupport
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
type(IO_T)                   :: Message

real(kind=wp), allocatable   :: refpat(:,:), defpat(:,:), newpat(:,:)
real(kind=sgl),allocatable   :: tmppat(:,:)
real(kind=wp)                :: DD, PCx, PCy, val, err, errmax, rnxi, rnyi, hg(8), W(3,3), gradCIC(8), Hessian(8,8), &
                                minx, miny, xi1max, xi2max, normdp, oldnorm, oldW(3,3), horiginal(8), CIC, sol(8), &
                                homographies(8,1000)
real(kind=dbl)               :: Wnew(3,3), Winv(3,3), dx, dy, p2(3), Woriginal(3,3), alp, srt(3,3), srtrot(3,3)
integer(kind=irg)            :: nx, ny, nxy, nbx, nby, i, ii, j, NSR, cnt, nxSR, nySR, jj, recordsize, ierr  
real(wp)                     :: tol
integer(kind=4)              :: hnstat
character(fnlen)             :: fname, gname, hostname 

hnstat = system_hostnm(hostname)

EMsoft = EMsoft_T( progname, progdesc )

! this program is a first conversion of the IDL HR-EBSD-DIC code into f90; we'll test
! everything here before turning it into a fullblown EMsoftOO program. We will use the IDL 
! convention of starting arrays at coordinate 0 for ease of comparison.  Also, keep in mind
! that fortran arrays are column-based whereas IDL is row-based...

! read the reference pattern from a binary file; the patterns have been 
! pre-processed and scaled between 0.D0 and 1.D0
nx = 640
ny = 480

nxy = nx * ny
recordsize = nxy * 4

! instantiate the DIC class
! this also initializes the x and y coordinate arrays
DIC = DIC_T( nx, ny )
call DIC%setverbose(.FALSE.)
! call DIC%setverbose(.TRUE.)


allocate(tmppat(nx,ny), refpat(0:nx-1,0:ny-1), defpat(0:nx-1,0:ny-1))
! open(20,file='/Users/mdg/playarea/DIC/verification-sq.data',status='old',form='unformatted')
if (trim(hostname).eq.'Mac-Studio.fios-router.home') then 
    fname = EMsoft%generateFilePath('EMdatapathname','DIC/refpat.data')
else
    fname = EMsoft%generateFilePath('EMdatapathname','playarea/DIC/refpat.data')
end if

open(20,file=trim(fname),status='old',form='unformatted')
read(20) tmppat
close(20,status='keep')
refpat = dble(tmppat) 
!
 ! open(20,file='/Users/mdg/Files/EMPlay/playarea/UCSB/GaN/pp27238.data',status='old',form='unformatted')
! recordsize = nxy*4
! open(unit=15,file='/Users/mdg/Files/EMPlay/playarea/UCSB/GaN/pp27238.data', &
     ! status='unknown',form='unformatted',access='direct',action='read',recl=recordsize,iostat=ierr)

! read(15, rec=1, iostat=ierr) tmppat
! refpat = tmppat 
! if (trim(hostname).eq.'Mac-Studio.fios-router.home') then 
!     fname = EMsoft%generateFilePath('EMdatapathname','UCSB/GaN/homographypatterns2.data')
! else
!     fname = EMsoft%generateFilePath('EMdatapathname','playarea/UCSB/GaN/homographypatterns.data')
! end if
! open(unit=dataunit,file=trim(fname),&
!      status='unknown',form='unformatted',access='direct',recl=recordsize,iostat=ierr)

 ! read(20) refpat
! read(dataunit, rec=1) tmppat
! read(20, rec=1) tmppat
! read(20) tmppat
! refpat = dble(tmppat)
! close(dataunit,status='keep')
! close(20,status='keep')

! write (*,*) ' tmppat entries : ', tmppat(1:2,1:2)
! write (*,*) ' reference pattern : ', minval(tmppat), maxval(tmppat)

! refpat(20:40,20:22) = 1.0_wp
! refpat(20:22,20:40) = 1.0_wp

refpat = refpat - minval(refpat)
refpat = refpat/maxval(refpat)
call DIC%setpattern('r', refpat)


! define some parameters for now...
tol = 100 * epsilon(1.0_wp)
PCx = 0.5_wp ! real(nx/2-1,wp)
PCy = 0.5_wp ! real(ny/2-1,wp)
DD = 15000.0_wp/50.0_wp 

! define the border widths nbx and nby for the subregion
nbx = 30
nby = 30
call DIC%defineSR( nbx, nby, PCx, PCy)

call DIC%getbsplines(refp=.TRUE., verify=.TRUE., grads=.TRUE.)

!zero-mean and normalize the referenceSR and targetSR arrays
call DIC%applyZMN(doreference=.TRUE.)

! compute the Hessian matrix
call DIC%getHessian( Hessian )

write (*,*) 'Hessian'
do i=1,8
    write (*,*) Hessian(i,1:8)
end do

! read 2500 random homographies from a file and run the fitting, then 
! write result to another file for comparison with an IDL routine.
if (trim(hostname).eq.'Mac-Studio.fios-router.home') then 
    gname = EMsoft%generateFilePath('EMdatapathname','DIC/test/homographies2.data')
else
    gname = EMsoft%generateFilePath('EMdatapathname','playarea/DIC/test/homographies2.data')
end if
open(unit=dataunit,file=trim(gname),status='unknown',form='unformatted')
read(dataunit) homographies
close(dataunit,status='keep')

if (trim(hostname).eq.'Mac-Studio.fios-router.home') then 
    gname = EMsoft%generateFilePath('EMdatapathname','DIC/test/homographypatterns2.data')
else
    gname = EMsoft%generateFilePath('EMdatapathname','playarea/DIC/test/homographypatterns2.data')
end if
open(unit=dataunit,file=trim(gname),&
     status='unknown',form='unformatted',access='direct',recl=recordsize,iostat=ierr)

! open(20,file='/Users/mdg/playarea/DIC/check.data',status='old',form='unformatted')

! open(unit=28,file='output-verify.txt',status='unknown',form='formatted')

! PCx = real(nx,wp)/2.0_wp
! PCy = real(ny,wp)/2.0_wp

! write (20) real(refpat)

! horiginal = (/ 0.2D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0 /)
! call DIC%applyHomography(horiginal, PCx, PCy)
! defpat = DIC%getpattern('d', nx, ny)
! write (20) real(defpat)
!   call DIC%cleanup()
! horiginal = (/ 0.0D0, 0.5D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0 /)
! call DIC%applyHomography(horiginal, PCx, PCy)
! defpat = DIC%getpattern('d', nx, ny)
! write (20) real(defpat)
!   call DIC%cleanup()
! horiginal = (/ 0.D0, 0.D0, 50.0D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0 /)
! call DIC%applyHomography(horiginal, PCx, PCy)
! defpat = DIC%getpattern('d', nx, ny)
! write (20) real(defpat)
!   call DIC%cleanup()
! horiginal = (/ 0.D0, 0.D0, 0.0D0, 0.5D0, 0.D0, 0.D0, 0.D0, 0.D0 /)
! call DIC%applyHomography(horiginal, PCx, PCy)
! defpat = DIC%getpattern('d', nx, ny)
! write (20) real(defpat)
!   call DIC%cleanup()
! horiginal = (/ 0.D0, 0.D0, 0.D0, 0.D0, 0.2D0, 0.D0, 0.D0, 0.D0 /)
! call DIC%applyHomography(horiginal, PCx, PCy)
! defpat = DIC%getpattern('d', nx, ny)
! write (20) real(defpat)
!   call DIC%cleanup()
! horiginal = (/ 0.D0, 0.D0, 0.0D0, 0.D0, 0.D0, 50.0D0, 0.D0, 0.D0 /)
! call DIC%applyHomography(horiginal, PCx, PCy)
! defpat = DIC%getpattern('d', nx, ny)
! write (20) real(defpat)
!   call DIC%cleanup()
! horiginal = (/ 0.D0, 0.D0, 0.0D0, 0.D0, 0.D0, 0.D0, 0.001D0, 0.D0 /)
! call DIC%applyHomography(horiginal, PCx, PCy)
! defpat = DIC%getpattern('d', nx, ny)
! write (20) real(defpat)
!   call DIC%cleanup()
! horiginal = (/ 0.D0, 0.D0, 0.0D0, 0.D0, 0.D0, 0.D0, 0.0D0, 0.001D0 /)
! call DIC%applyHomography(horiginal, PCx, PCy)
! defpat = DIC%getpattern('d', nx, ny)
! write (20) real(defpat)
!   call DIC%cleanup()
! horiginal = (/ 0.01D0, 0.1D0, 50.0D0, 0.1D0, 0.5D0,-50.D0, 0.002D0, 0.002D0 /)
! call DIC%applyHomography(horiginal, PCx, PCy)
! defpat = DIC%getpattern('d', nx, ny)
! write (20) real(defpat)

! close(20,status='keep')

! ! check

! horiginal = (/ 0.01D0, 0.1D0, 50.0D0, 0.1D0, 0.5D0,-50.D0, 0.002D0, 0.002D0 /)
! write(*,*) horiginal
! W = DIC%getShapeFunction(horiginal)
! hg = DIC%getHomography(W) 
! write(*,*) hg 


! stop

do jj=1, 1000
    ! call Message%printMessage(' ---------------------- ')
    if (mod(jj,100).eq.0) write (*,*) 'starting pattern ', jj
! here we deform the reference pattern to obtain a defpat array with known homography
! define the homography hg
 horiginal = homographies(1:8,jj)
! horiginal = (/ (0.0_wp, i=1,8) /)
 call DIC%applyHomography(horiginal, PCx, PCy)

! read(dataunit,rec=jj, iostat=ierr) tmppat
! read(20,rec=jj, iostat=ierr) tmppat
! read(20) tmppat
! write (*,*) 'read error = ', ierr
! defpat = dble(tmppat)
! call DIC%setpattern('d', defpat)
defpat = DIC%getpattern('d',nx,ny)
defpat = defpat-minval(defpat)
defpat = defpat/maxval(defpat)
write (dataunit,rec=jj) sngl(defpat)

if (1.eq.0) then 

Woriginal = DIC%getShapeFunction(horiginal)

! get the derivatives of the reference pattern to determine the Hessian
call DIC%getbsplines(defp=.TRUE.)

! ! zero-mean and normalize the referenceSR and targetSR arrays
call DIC%applyZMN(dotarget=.TRUE.)

! ! initial CIC function (to be minimized ...)
call DIC%getresiduals( CIC )
! stop

! initialize the homography coefficients to zero
! in a more general implementation we might initialize them to the 
! values for one of the nearest neighbor patterns
hg = (/ (0.0_wp, i=1,8) /)
W = DIC%getShapeFunction(hg)

oldnorm = 100.0_wp

! and here we start the loop 
do ii=1,3 !50 
    ! write (*,*) ' iteration # ',ii
    call DIC%applyHomography(hg, PCx, PCy, dotarget=.TRUE.)

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
    ! write (*,*) hg
    ! write (*,*) ' norm(deltap) = ', normdp
    ! write (*,*) '------------'
    ! if (normdp.lt.oldnorm) then
    if (normdp.lt.0.001D0) then
    !     oldnorm = normdp
    !     oldW = W 
    ! else
        ! W = oldW
        EXIT
    end if
    ! call Message%printMessage(' ---------------------- ')
end do 

W = matrixInvert_wp( W )
W = W / W(3,3)
hg = DIC%getHomography(W)
! write (*,*) '' 
! write (*,*) ' Final homography : '
! write (*,*) DIC%getHomography(W)
! write (*,*) '' 
! write (*,*) ' Target homography : '
! write (*,*) horiginal
! write (*,*) '' 
! write (*,*) ' Differences : '
! write (*,*) horiginal-hg

! write results to data file (single precision because IDL has a bug for double precision)
write (unit=28,FMT='(9(F12.8,","),I4)') real(hg), real(normdp), ii

! if (jj.eq.3) stop
  call DIC%cleanup()

end if 

end do ! loop over jj 

! close(28,status='keep')
! close(20,status='keep')
close(dataunit,status='keep')

end program EMplay
