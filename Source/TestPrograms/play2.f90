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
DIC = DIC_T( nx, ny, normalize = .TRUE. )
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

! determine the b-splines and the gradients for the reference pattern
call DIC%getbsplines(refp=.TRUE., verify=.TRUE., grads=.TRUE.)

!zero-mean and normalize the referenceSR and targetSR arrays
call DIC%applyZMN(doreference=.TRUE.)

! compute the Hessian matrix
call DIC%getHessian( Hessian )

write (*,*) 'Hessian'
do i=1,8
    write (*,*) Hessian(i,1:8)
end do

! read random homographies from a file and run the fitting, then 
! write result to another file for comparison with an IDL routine.
if (trim(hostname).eq.'Mac-Studio.fios-router.home') then 
    gname = EMsoft%generateFilePath('EMdatapathname','DIC/test/homographies2.data')
else
    gname = EMsoft%generateFilePath('EMdatapathname','playarea/DIC/test/homographiespix.data')
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

do jj=1, 1000
    ! call Message%printMessage(' ---------------------- ')
    if (mod(jj,100).eq.0) write (*,*) 'starting pattern ', jj
! here we deform the reference pattern to obtain a defpat array with known homography
! define the homography hg
    horiginal = homographies(1:8,jj)
    call DIC%applyHomography(horiginal, PCx, PCy)

    defpat = DIC%getpattern('d',nx,ny)
    defpat = defpat-minval(defpat)
    defpat = defpat/maxval(defpat)
    write (dataunit,rec=jj) sngl(defpat)
end do ! loop over jj 

! close(28,status='keep')
! close(20,status='keep')
close(dataunit,status='keep')

end program EMplay
