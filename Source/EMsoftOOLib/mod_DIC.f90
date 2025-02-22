! ###################################################################
! Copyright (c) 2014-2024, Marc De Graef Research Group/Carnegie Mellon University
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

module mod_DIC
 !! author: MDG
 !! version: 1.0
 !! date: 12/01/24
 !!
 !! this class implements the DIC algorithm as described in the series of 
 !! papers by Ernould et al.
 !!
 
use mod_kinds
use mod_global
use bspline_module
use bspline_kinds_module, only: wp, ip 

IMPLICIT NONE

type, public :: DIC_T
  private
    real(wp)                        :: W(3,3)     ! shape function
    real(wp)                        :: h(8)       ! homography
    real(wp)                        :: CIC        ! current value of function to be minimized
    integer(ip)                     :: kx = 5     ! spline order
    integer(ip)                     :: ky = 5     ! spline order
    integer(kind=irg)               :: nx         ! pattern x-size
    integer(kind=irg)               :: ny         ! pattern y-size
    integer(kind=irg)               :: nxc        ! actual size of x-coordinate array
    integer(kind=irg)               :: nyc        ! actual size of y-coordinate array
    integer(kind=irg)               :: cross(4)   ! start and end x and y coordinates of dark bands
    real(wp)                        :: rnxi       ! pattern x scale factor
    real(wp)                        :: rnyi       ! pattern y scale factor
    real(wp)                        :: aspectratio! smallest n over largest n
    integer(kind=irg)               :: nxSR       ! sub-region x-size
    integer(kind=irg)               :: nySR       ! sub-region y-size
    integer(kind=irg)               :: nSR        ! sub-region total size
    integer(kind=irg)               :: nbx        ! sub-region border x-width
    integer(kind=irg)               :: nby        ! sub-region border y-width
    type(bspline_2d)                :: sref       ! b-spline class for reference pattern
    type(bspline_2d)                :: sdef       ! b-spline class for deformed pattern
    real(wp)                        :: tol = 100.0_wp * epsilon(1.0_wp) ! tolerance
    logical                         :: verbose = .FALSE.   ! useful for debugging
    logical                         :: normalizedcoordinates = .TRUE.
    logical                         :: applycross = .FALSE.


! the arrays that are tied to a given reference/target pattern are defined here
    real(wp),allocatable            :: x(:)
    real(wp),allocatable            :: y(:)
    real(wp),allocatable            :: xiX(:)
    real(wp),allocatable            :: xiY(:)
    integer(kind=irg),allocatable   :: xiXint(:)
    integer(kind=irg),allocatable   :: xiYint(:)
    real(wp),allocatable            :: XiPrime(:,:)
    real(wp),allocatable            :: refpat(:,:)
    real(wp),allocatable            :: defpat(:,:)
    real(wp),allocatable            :: wtarget(:)
    real(wp),allocatable            :: gradx(:,:)
    real(wp),allocatable            :: grady(:,:)
    real(wp),allocatable            :: gxSR(:)
    real(wp),allocatable            :: gySR(:)
    real(wp),allocatable            :: GradJac(:,:)
    real(wp),allocatable            :: referenceSR(:)
    real(wp),allocatable            :: targetSR(:)
    real(wp),allocatable            :: aux(:,:)
    real(wp),allocatable            :: refzmn(:)
    real(wp),allocatable            :: tarzmn(:)
    real(wp),allocatable            :: residuals(:)
    real(wp)                        :: Hessian(8,8) 
    real(wp)                        :: gradCIC(8)
    real(wp)                        :: refmean
    real(wp)                        :: tarmean
    real(wp)                        :: refstdev
    real(wp)                        :: tarstdev

  contains
  private

    procedure, pass(self) :: getShapeFunction_
    procedure, pass(self) :: getHomography_
    procedure, pass(self) :: homography2Fe_
    procedure, pass(self) :: Fe2homography_
    procedure, pass(self) :: getbsplines_
    procedure, pass(self) :: getHessian_
    procedure, pass(self) :: solveHessian_
    procedure, pass(self) :: defineSR_
    procedure, pass(self) :: applyZMN_
    procedure, pass(self) :: applyHomography_
    procedure, pass(self) :: getresiduals_
    procedure, pass(self) :: setverbose_
    procedure, pass(self) :: setpattern_
    procedure, pass(self) :: getpattern_
    procedure, pass(self) :: DIC_cleanup_

    final :: DIC_destructor

    generic, public :: getShapeFunction => getShapeFunction_
    generic, public :: getHomography => getHomography_
    generic, public :: Fe2homography=> Fe2homography_
    generic, public :: homography2Fe=> homography2Fe_
    generic, public :: getbsplines => getbsplines_
    generic, public :: getHessian => getHessian_
    generic, public :: solveHessian => solveHessian_
    generic, public :: defineSR => defineSR_
    generic, public :: applyZMN => applyZMN_
    generic, public :: applyHomography => applyHomography_
    generic, public :: getresiduals => getresiduals_
    generic, public :: setverbose => setverbose_
    generic, public :: setpattern => setpattern_
    generic, public :: getpattern => getpattern_
    generic, public :: cleanup => DIC_cleanup_

end type DIC_T

! the constructor routine for this class
interface DIC_T
  module procedure DIC_constructor
end interface DIC_T

contains

!--------------------------------------------------------------------------
recursive type(DIC_T) function DIC_constructor( nx, ny, normalize, cross ) result(DIC)
!DEC$ ATTRIBUTES DLLEXPORT :: DIC_constructor
 !! author: MDG
 !! version: 1.0
 !! date: 12/01/24
 !!
 !! constructor for the DIC_T Class;
 !!
 !! only normalized coordinates have been tested so far...

use bspline_kinds_module, only: wp, ip 

IMPLICIT NONE

integer(kind=irg), INTENT(IN)               :: nx
integer(kind=irg), INTENT(IN)               :: ny
logical,INTENT(IN),OPTIONAL                 :: normalize
integer(kind=irg),INTENT(IN),OPTIONAL       :: cross(4)

integer(kind=irg)                           :: i, j
real(wp)                                    :: ratio=1.0_wp 

DIC%normalizedcoordinates = .FALSE.

! do we need to remove a cross-shaped area from the subregion in defineSR ?
if (present(cross)) then 
  if (sum(cross).gt.0) then 
    DIC%cross = cross
    DIC%applycross = .TRUE.
  else 
    DIC%applycross = .FALSE.
  end if 
end if 

! set pattern dimensions 
DIC%nx = nx 
DIC%ny = ny 

! allocate and initialize the normalized coordinate arrays
allocate( DIC%x(0:DIC%nx-1), DIC%y(0:DIC%ny-1) )

DIC%x = (/ (real(i,wp),i=0,nx-1) /)
DIC%y = (/ (real(j,wp),j=0,ny-1) /)

if (present(normalize)) then  
  if (normalize.eqv..TRUE.) then ! we use normalized coordinates
    DIC%rnxi = 1.0_wp/real(DIC%nx-1,wp)
    DIC%rnyi = 1.0_wp/real(DIC%ny-1,wp)
    DIC%x = DIC%x * DIC%rnxi
    DIC%y = DIC%y * DIC%rnyi
    DIC%normalizedcoordinates = .TRUE.
  end if
end if

DIC%aspectratio = ratio

end function DIC_constructor

!--------------------------------------------------------------------------
subroutine DIC_destructor( self )
!DEC$ ATTRIBUTES DLLEXPORT :: DIC_destructor
 !! author: MDG
 !! version: 1.0
 !! date: 12/01/24
 !!
 !! deallocate all arrays

IMPLICIT NONE

type(DIC_T), INTENT(INOUT)      :: self

call self%sref%destroy()
call self%sdef%destroy()

if (allocated(self%x)) deallocate(self%x)
if (allocated(self%y)) deallocate(self%y)
if (allocated(self%xiX)) deallocate(self%xiX)
if (allocated(self%xiY)) deallocate(self%xiY)
if (allocated(self%XiPrime)) deallocate(self%XiPrime)
if (allocated(self%refpat)) deallocate(self%refpat)
if (allocated(self%defpat)) deallocate(self%defpat)
if (allocated(self%wtarget)) deallocate(self%wtarget)
if (allocated(self%gradx)) deallocate(self%gradx)
if (allocated(self%grady)) deallocate(self%grady)
if (allocated(self%gxSR)) deallocate(self%gxSR)
if (allocated(self%gySR)) deallocate(self%gySR)
if (allocated(self%GradJac)) deallocate(self%GradJac)
if (allocated(self%referenceSR)) deallocate(self%referenceSR)
if (allocated(self%targetSR)) deallocate(self%targetSR)
if (allocated(self%refzmn)) deallocate(self%refzmn)
if (allocated(self%tarzmn)) deallocate(self%tarzmn)
if (allocated(self%residuals)) deallocate(self%residuals)

call reportDestructor('DIC_T')

end subroutine DIC_destructor

!--------------------------------------------------------------------------
subroutine DIC_cleanup_( self )
!DEC$ ATTRIBUTES DLLEXPORT :: DIC_cleanup_
 !! author: MDG
 !! version: 1.0
 !! date: 12/11/24
 !!
 !! deallocate some arrays

IMPLICIT NONE

class(DIC_T), INTENT(INOUT)      :: self

call self%sdef%destroy()

if (allocated(self%defpat)) deallocate(self%defpat)
if (allocated(self%wtarget)) deallocate(self%wtarget)
if (allocated(self%tarzmn)) deallocate(self%tarzmn)
if (allocated(self%residuals)) deallocate(self%residuals)

end subroutine DIC_cleanup_

!--------------------------------------------------------------------------
recursive subroutine setverbose_(self, v)
!DEC$ ATTRIBUTES DLLEXPORT :: setverbose_
 !! author: MDG
 !! version: 1.0
 !! date: 12/01/24
 !!
 !! set the verbose parameter

use mod_IO 

IMPLICIT NONE

class(DIC_T),INTENT(INOUT)      :: self
logical, INTENT(IN)             :: v

type(IO_T)                      :: Message 

self%verbose = v 

if (v.eqv..TRUE.) then 
  call Message%printMessage(' DIC_T class verbosity turned on')
end if 

end subroutine setverbose_

!--------------------------------------------------------------------------
recursive subroutine setpattern_(self, rp, pattern)
!DEC$ ATTRIBUTES DLLEXPORT :: setpattern_
 !! author: MDG
 !! version: 1.0
 !! date: 12/01/24
 !!
 !! set the reference or target pattern

use mod_IO 

IMPLICIT NONE

class(DIC_T),INTENT(INOUT)      :: self
character(1),INTENT(IN)         :: rp
real(wp),INTENT(IN)             :: pattern(:,:)

type(IO_T)                      :: Message 
integer(kind=irg)               :: sz(2) 

sz = shape(pattern)

if (rp.eq.'r') then 
  if (.not.allocated(self%defpat)) allocate( self%refpat(0:sz(1)-1,0:sz(2)-1) )
  self%refpat = pattern 
  if (self%verbose) call Message%printMessage(' setpattern_::reference pattern set ')
end if

if (rp.eq.'d') then 
  if (.not.allocated(self%defpat)) allocate( self%defpat(0:sz(1)-1,0:sz(2)-1) )
  self%defpat = pattern 
  if (self%verbose) call Message%printMessage(' setpattern_::target pattern set ')
end if

end subroutine setpattern_

!--------------------------------------------------------------------------
recursive function getpattern_(self, rp, nx, ny) result(pattern)
!DEC$ ATTRIBUTES DLLEXPORT :: getpattern_
 !! author: MDG
 !! version: 1.0
 !! date: 12/01/24
 !!
 !! get the reference or target pattern

use mod_IO 

IMPLICIT NONE

class(DIC_T),INTENT(INOUT)      :: self
character(1),INTENT(IN)         :: rp
integer(kind=irg),INTENT(IN)    :: nx
integer(kind=irg),INTENT(IN)    :: ny
real(kind=sgl)                  :: pattern(nx, ny)

type(IO_T)                      :: Message 

if (rp.eq.'r') then 
  pattern = real(self%refpat)
  if (self%verbose) call Message%printMessage(' getpattern_::reference pattern retrieved ')
end if

if (rp.eq.'d') then 
  pattern = real(self%defpat)
  if (self%verbose) call Message%printMessage(' getpattern_::target pattern retrieved ')
end if

end function getpattern_

!--------------------------------------------------------------------------
recursive subroutine getbsplines_(self, verify, refp, defp, grads)
!DEC$ ATTRIBUTES DLLEXPORT :: getbsplines_
 !! author: MDG
 !! version: 1.0
 !! date: 12/01/24
 !!
 !! compute the bspline coefficients for the reference pattern and, optionally,
 !! also for the deformed pattern; optionally also compute the gradients of the 
 !! reference pattern

use mod_IO 

IMPLICIT NONE

class(DIC_T),INTENT(INOUT)      :: self
logical, INTENT(IN), OPTIONAL   :: verify ! reconstruct the reference pattern from splines ?
logical, INTENT(IN), OPTIONAL   :: refp   ! get bsplines for deformed pattern ?
logical, INTENT(IN), OPTIONAL   :: defp   ! get bsplines for deformed pattern ?
logical, INTENT(IN), OPTIONAL   :: grads  ! compute gradients of the reference pattern ?

type(IO_T)                      :: Message 
integer(ip)                     :: iflag
integer(kind=irg)               :: i, j
real(wp)                        :: val, err, errmax 
real(kind=dbl)                  :: io_real(1)

if (present(refp)) then 
  if (refp.eqv..TRUE.) then 
    ! initialize the bsplines for the reference pattern
    call self%sref%initialize(self%x,self%y,self%refpat,self%kx,self%ky,iflag)
    ! for potential later calls, allow for extrapolation
    call self%sref%set_extrap_flag(.TRUE.)
    if (self%verbose) call Message%printMessage(' getbsplines_::b-splines for reference pattern completed')
  end if 
end if

if (present(defp)) then 
  if (defp.eqv..TRUE.) then 
! initialize the bsplines for the deformed pattern
    call self%sdef%initialize(self%x,self%y,self%defpat,self%kx,self%ky,iflag)
! for potential later calls, allow for extrapolation
    call self%sdef%set_extrap_flag(.TRUE.)
    if (self%verbose) call Message%printMessage(' getbsplines_::b-splines for target pattern completed')
  end if 
end if

if (present(verify)) then 
  if (verify.eqv..TRUE.) then 
    errmax = -1.0_wp
    do i=0,self%nx-1
       do j=0,self%ny-1
            ! determine how well the spline coefficients represent the original reference pattern
            call self%sref%evaluate(self%x(i),self%y(j),0,0,val,iflag)
            err = abs(self%refpat(i,j)-val)
            errmax = max(err,errmax)
       end do
    end do
    ! check max error against tolerance
    if (self%verbose) then 
      io_real(1) = dble(errmax)
      call Message%WriteValue(' getbsplines_::max b-spline reconstruction error of reference pattern:', io_real, 1) 
      if (errmax >= self%tol) then
        call Message%printMessage(' ** reference reconstruction test failed ** ')
      else
        call Message%printMessage(' ** reference reconstruction test passed ** ')
      end if
    end if
  end if 
end if 

if (present(grads)) then 
  if (grads.eqv..TRUE.) then 
    if (.not.allocated(self%gradx)) then 
      allocate( self%gradx(0:self%nx-1,0:self%ny-1), self%grady(0:self%nx-1,0:self%ny-1) )
    end if
    do i=0,self%nx-1
       do j=0,self%ny-1
            ! extract the gradient arrays
            call self%sref%evaluate(self%x(i),self%y(j),1,0,self%gradx(i,j),iflag)
            call self%sref%evaluate(self%x(i),self%y(j),0,1,self%grady(i,j),iflag)
       end do
    end do
    if (self%verbose) then 
      call Message%printMessage(' getbsplines_::gradient of reference pattern completed')
    end if 
    if (self%applycross.eqv..FALSE.) then 
      self%gxSR = reshape( self%gradx(self%nbx:self%nx-self%nbx-1,self%nby:self%ny-self%nby-1), (/ self%NSR /) )
      self%gySR = reshape( self%grady(self%nbx:self%nx-self%nbx-1,self%nby:self%ny-self%nby-1), (/ self%NSR /) )
    else 
      do i = 0, self%nxSR-1
        do j = 0, self%nySR-1
          self%aux(i,j) = self%gradx(self%xiXint(i),self%xiYint(j))
        end do 
      end do 
      self%gxSR = reshape( self%aux, (/ self%NSR /) )
      do i = 0, self%nxSR-1
        do j = 0, self%nySR-1
          self%aux(i,j) = self%grady(self%xiXint(i),self%xiYint(j))
        end do 
      end do 
      self%gySR = reshape( self%aux, (/ self%NSR /) )
    end if 
  end if 
end if 

end subroutine getbsplines_

!--------------------------------------------------------------------------
recursive subroutine defineSR_(self, nbx, nby, PCx, PCy)
!DEC$ ATTRIBUTES DLLEXPORT :: defineSR_
 !! author: MDG
 !! version: 1.0
 !! date: 12/01/24
 !!
 !! set dimensions and allocate all SR-related arrays
 !! this optionally includes exclusion of points inside the dark
 !! horizontal and vertical bands for some cameras (cross) 

use mod_IO 

IMPLICIT NONE

class(DIC_T),INTENT(INOUT)      :: self
integer(kind=irg),INTENT(IN)    :: nbx
integer(kind=irg),INTENT(IN)    :: nby
real(wp),INTENT(IN)             :: PCx
real(wp),INTENT(IN)             :: PCy

type(IO_T)                      :: Message 

integer(kind=irg)               :: i, j

! set the array dimensions (nbx, nby) for the outer dimensions of the sub-region
self%nbx = nbx
self%nby = nby

! if applycross is set, then we need to correct the dimensions of the subregion
if (self%applycross.eqv..FALSE.) then 
  self%nxSR = self%nx-2*nbx
  self%nySR = self%ny-2*nby
  self%NSR = self%nxSR * self%nySR

! allocate and populate the reference subregion array
  allocate( self%referenceSR(0:self%NSR-1) )
  self%referenceSR = reshape( self%refpat(nbx:self%nx-nbx-1,nby:self%ny-nby-1), (/ self%NSR /) )
  if (self%verbose) call Message%printMessage(' defineSR_::referenceSR array allocated')
! define the coordinate arrays for the sub-region
  allocate( self%xiX(0:self%nxSR-1), self%xiY(0:self%nySR-1) )
  if (self%aspectratio.eq.1.0_wp) then 
    self%xiX = self%x(nbx:self%nx-nbx-1) - PCx
    self%xiY = self%y(nby:self%ny-nby-1) - PCy
  else
    if (self%nx.gt.self%ny) then 
      self%xiX = self%x(nbx:self%nx-nbx-1) - PCx
      self%xiY = self%y(nby:self%ny-nby-1) - PCy * self%aspectratio
    else
      self%xiX = self%x(nbx:self%nx-nbx-1) - PCx * self%aspectratio
      self%xiY = self%y(nby:self%ny-nby-1) - PCy
    end if
  end if
else  ! we need to eliminate the pixels inside the vertical and horizontal bands ... 
  self%nxSR = self%nx-2*nbx-(self%cross(2) - self%cross(1)) 
  self%nySR = self%ny-2*nby-(self%cross(4) - self%cross(3))
  self%NSR = self%nxSR * self%nySR
! first get the subregion x and y coordinate arrays with the bands removed
! these will be the normalized coordinates  
  allocate( self%xiX(0:self%nxSR-1), self%xiY(0:self%nySR-1) )
  self%xiX(0:self%cross(1)-nbx-1) = self%x(nbx:self%cross(1)-1)
  self%xiX(self%cross(1)-nbx:self%nxSR-1) = self%x(self%cross(2):self%nx-self%nbx-1)
  self%xiY(0:self%cross(3)-nby-1) = self%y(nby:self%cross(3)-1)
  self%xiY(self%cross(3)-nby:self%nySR-1) = self%y(self%cross(4):self%ny-self%nby-1)
! allocate the 1-D arrays needed for the computations
  allocate( self%referenceSR(0:self%NSR-1), self%aux(0:self%nxSR-1,0:self%nySR-1) )
  allocate( self%xiXint(0:self%nxSR-1), self%xiYint(0:self%nySR-1))
  if (self%normalizedcoordinates.eqv..TRUE.) then 
    self%xiXint = nint( self%xiX * real(self%nx,wp) )
    self%xiYint = nint( self%xiY * real(self%ny,wp) )
  else
    self%xiXint = nint( self%xiX )
    self%xiYint = nint( self%xiY )
  end if
! this is the remapping code from full pattern to subregion with bands removed
  do i = 0, self%nxSR-1
    do j = 0, self%nySR-1
      self%aux(i,j) = self%refpat(self%xiXint(i),self%xiYint(j))
    end do 
  end do 
  self%referenceSR = reshape( self%aux, (/ self%NSR /) )

  ! open(unit=40,file='aux.data',status='unknown',form='unformatted')
  ! write (40) real(aux)
  ! close(unit=40,status='keep')

  self%xiX = self%xiX - PCx
  self%xiY = self%xiY - PCy
end if

if (self%verbose) call Message%printMessage(' defineSR_::xiX, xiY arrays allocated')

! allocate arrays for gradient components and zero mean + normalized arrays
allocate(  self%gxSR(0:self%NSR-1), self%gySR(0:self%NSR-1), &   
            self%refzmn(0:self%NSR-1), self%tarzmn(0:self%NSR-1) )
if (self%verbose) call Message%printMessage(' defineSR_::gradient and zmn arrays allocated')

! allocate array for the product of the gradient and the Jacobian
allocate( self%GradJac(0:7,0:self%NSR-1) )
if (self%verbose) call Message%printMessage(' defineSR_::GradJac array allocated')

! allocate the residuals array 
allocate( self%residuals(0:self%NSR-1) )
if (self%verbose) call Message%printMessage(' defineSR_::residuals array allocated')

! and finally the targetdef array 
allocate( self%targetSR(0:self%NSR-1) )
if (self%verbose) call Message%printMessage(' defineSR_::targetSR array allocated')

end subroutine defineSR_

!--------------------------------------------------------------------------
recursive subroutine applyZMN_(self, doreference, dotarget)
!DEC$ ATTRIBUTES DLLEXPORT :: applyZMN_
 !! author: MDG
 !! version: 1.0
 !! date: 12/01/24
 !!
 !! apply zero-mean and normalization to a pattern

use mod_IO 

IMPLICIT NONE

class(DIC_T),INTENT(INOUT)      :: self
logical,INTENT(IN), OPTIONAL    :: doreference
logical,INTENT(IN), OPTIONAL    :: dotarget
real(wp)                        :: io_real(2)

type(IO_T)                      :: Message 

if (present(doreference)) then 
  if (doreference.eqv..TRUE.) then 
    self%refmean = sum(self%referenceSR)/dble(self%NSR)
    self%refstdev = sqrt( sum( (self%referenceSR - self%refmean)**2 ) )
    self%refzmn = (self%referenceSR-self%refmean)/self%refstdev 
    io_real(1:2) = (/ self%refmean, self%refstdev /)
    if (self%verbose) call Message%WriteValue(' applyZMN_::reference mean/stdev : ', io_real, 2)
    ! write (*,*) ' applyZMN_::refzmn range ', minval(self%refzmn), maxval(self%refzmn)
  end if 
end if

if (present(dotarget)) then 
  if (dotarget.eqv..TRUE.) then 
    self%tarmean = sum(self%targetSR)/dble(self%NSR)
    self%tarstdev = sqrt( sum( (self%targetSR - self%tarmean)**2 ) )
    self%tarzmn = (self%targetSR-self%tarmean)/self%tarstdev 
    io_real(1:2) = (/ self%tarmean, self%tarstdev /)
    if (self%verbose) call Message%WriteValue(' applyZMN_::target mean/stdev : ', io_real, 2)
    ! write (*,*) ' applyZMN_::tarzmn range ', minval(self%tarzmn), maxval(self%tarzmn)
  end if 
end if

end subroutine applyZMN_

!--------------------------------------------------------------------------
recursive subroutine applyHomography_(self, h, PCx, PCy, dotarget)
!DEC$ ATTRIBUTES DLLEXPORT :: applyHomography_
 !! author: MDG
 !! version: 1.0
 !! date: 12/01/24
 !!
 !! apply a homography to the reference or target pattern and store in defpat

use mod_IO 
use mod_math

IMPLICIT NONE

class(DIC_T),INTENT(INOUT)      :: self
real(wp),INTENT(IN)             :: h(8)
real(wp),INTENT(IN)             :: PCx
real(wp),INTENT(IN)             :: PCy
logical,INTENT(IN),OPTIONAL     :: dotarget

type(IO_T)                      :: Message

real(wp)                        :: W(3,3), p(3), lPCx, lPCy
integer(ip)                     :: iflag
integer(kind=irg)               :: cnt, i, j, lnx, lny 
logical                         :: tgt

tgt = .FALSE.
if (present(dotarget)) then 
  if (dotarget.eqv..TRUE.) then 
    tgt = .TRUE.
    if (allocated(self%XiPrime)) deallocate(self%XiPrime)
  end if
end if 

lnx = self%nx
lny = self%ny

if (self%aspectratio.eq.1.0_wp) then 
  lPCx = PCx 
  lPCy = PCy 
else
  if (self%nx.gt.self%ny) then 
    lPCx = PCx 
    lPCy = PCy*self%aspectratio
  else
    lPCx = PCx*self%aspectratio 
    lPCy = PCy
  end if 
end if

! convert to shape function
W =  self%getShapeFunction(h)

! determine the XiPrime coordinates so that we can apply the deformation by
! means of the evaluate method in the bspline class
if (.not.allocated(self%XiPrime)) allocate( self%XiPrime(0:1, 0:lnx*lny-1))
cnt = 0
do j = 0, lny-1
    do i = 0, lnx-1
      p = matmul( W,  (/ self%x(i)-lPCx, self%y(j)-lPCy, 1.0_wp /) )
      if (p(3).ne.0.0_wp) then 
          self%XiPrime(0:1,cnt) = p(1:2)/p(3)
      else
          self%XiPrime(0:1,cnt) = p(1:2)
      end if
      cnt = cnt + 1
    end do 
end do 
if (self%verbose) then 
  write (*,*) ' applyHomography_::XiPrime(0:1,0:1) : ', self%XiPrime(0:1,0:1)
end if 

! and interpolate/extrapolate the deformed pattern as needed
if (.not.allocated(self%defpat)) allocate( self%defpat( 0:lnx-1, 0:lny-1 ))
cnt = 0
do j=0,lny-1
    do i=0,lnx-1
      iflag = 0_wp
      if (tgt) then 
        call self%sdef%evaluate(self%XiPrime(0,cnt)+lPCx,self%XiPrime(1,cnt)+lPCy,0,0,self%defpat(i,j),iflag)
      else
        call self%sref%evaluate(self%XiPrime(0,cnt)+lPCx,self%XiPrime(1,cnt)+lPCy,0,0,self%defpat(i,j),iflag)
      end if
      cnt = cnt+1
   end do
end do
! reduce to interval [0,1]
self%defpat = self%defpat - minval(self%defpat)
self%defpat = self%defpat/maxval(self%defpat)
if (self%verbose) then 
  call Message%printMessage(' applyHomography_::defpat interpolated')
  write (*,*) ' applyHomography_::range(defpat) : ',minval(self%defpat), maxval(self%defpat)
end if 

! do we need to reinit sdef each time ?
call self%sdef%initialize(self%x,self%y,self%defpat,self%kx,self%ky,iflag)

! finally get the b-spline coefficients for the deformed pattern
if (.not.tgt) then 
  if (self%verbose) call Message%printMessage(' applyHomography_::sdef class initialized')
  call self%sdef%set_extrap_flag(.TRUE.)
end if 

! copy the sub-region of the deformed target pattern into targetSR array
if (.not.allocated(self%targetSR)) then 
  allocate( self%targetSR( 0:self%NSR-1 ) )
end if

if (self%applycross.eqv..FALSE.) then 
  self%targetSR = reshape( self%defpat( self%nbx:self%nx-self%nbx-1, self%nby:self%ny-self%nby-1 ), (/ self%NSR /) )
else
! this is the remapping code from full pattern to subregion with bands removed
  do i = 0, self%nxSR-1
    do j = 0, self%nySR-1
      self%aux(i,j) = self%defpat(self%xiXint(i),self%xiYint(j))
    end do 
  end do 
  self%targetSR = reshape( self%aux, (/ self%NSR /) )
end if

end subroutine applyHomography_

!--------------------------------------------------------------------------
recursive subroutine getresiduals_(self, CIC)
!DEC$ ATTRIBUTES DLLEXPORT :: getresiduals_
 !! author: MDG
 !! version: 1.0
 !! date: 12/01/24
 !!
 !! compute the residuals and optionally CIC

use mod_IO 

IMPLICIT NONE

class(DIC_T),INTENT(INOUT)      :: self
real(wp),INTENT(OUT),OPTIONAL   :: CIC 

type(IO_T)                      :: Message 
real(kind=dbl)                  :: io_real(2)

self%residuals = self%refzmn-self%tarzmn

if (present(CIC)) then 
  self%CIC = sum( self%residuals**2 )
  CIC = self%CIC 
  if (self%verbose) then
    io_real(1) = dble(CIC)
    call Message%WriteValue(' getresiduals_::CIC value : ', io_real, 1)
    io_real(1) = minval(self%residuals)
    io_real(2) = maxval(self%residuals)
    call Message%WriteValue(' getresiduals_::residuals range : ', io_real, 2)
  end if 
end if

end subroutine getresiduals_

!--------------------------------------------------------------------------
recursive subroutine getHessian_(self, Hessian)
!DEC$ ATTRIBUTES DLLEXPORT :: getHessian_
 !! author: MDG
 !! version: 1.0
 !! date: 12/01/24
 !!
 !! compute the Hessian matrix for the reference pattern

use mod_IO 

IMPLICIT NONE

class(DIC_T),INTENT(INOUT)      :: self
real(wp),INTENT(OUT),OPTIONAL   :: Hessian(8,8)

type(IO_T)                      :: Message 
integer(kind=irg)               :: cnt, i, j 
real(wp)                        :: GJ(8)

cnt = 0
self%Hessian = 0.0_wp
do j = 0, self%nySR-1
    do i = 0, self%nxSR-1
        GJ = (/ self%gxSR(cnt)*self%xiX(i), self%gxSR(cnt)*self%xiY(j), self%gxSR(cnt), & 
                self%gySR(cnt)*self%xiX(i), self%gySR(cnt)*self%xiY(j), self%gySR(cnt), &
                self%gxSR(cnt)*self%xiX(i)**2-self%gySR(cnt)*self%xiX(i)*self%xiY(j), &
                self%gySR(cnt)*self%xiY(j)**2-self%gxSR(cnt)*self%xiX(i)*self%xiY(j) /)
        self%GradJac(0:7,cnt) = GJ
        self%Hessian = self%Hessian + matmul( reshape(GJ, (/8,1/)), reshape(GJ, (/1,8/)) ) 
        cnt = cnt + 1
    end do 
end do 
self%Hessian = self%Hessian * 2.0_wp / self%refstdev**2

if (present(Hessian)) Hessian = self%Hessian

if (self%verbose) then 
  call Message%printMessage(' getHessian_::reference Hessian computed')
  write (*,*) Hessian 
end if 

end subroutine getHessian_

!--------------------------------------------------------------------------
recursive subroutine solveHessian_(self, SOL, normdp)
!DEC$ ATTRIBUTES DLLEXPORT :: solveHessian_
 !! author: MDG
 !! version: 1.0
 !! date: 12/01/24
 !!
 !! solve the Hessian equation using Cholesky decomposition (LAPACK)

use mod_IO 

IMPLICIT NONE

class(DIC_T),INTENT(INOUT)   :: self
real(wp),INTENT(INOUT)       :: SOL(8)
real(wp),INTENT(INOUT)       :: normdp

type(IO_T)                   :: Message

real(wp)                     :: Hessiancopy(8,8) ! local copy 
real(wp)                     :: xi1max, xi2max
real(kind=dbl)               :: io_real(2)

! LAPACK variables
character                    :: UPLO ='U'
integer                      :: NN = 8
integer                      :: NRHS = 1
integer                      :: LDA = 8
real(wp)                     :: SL(1:8,1)
integer                      :: LDB = 8
integer                      :: INFO 

! compute gradCIC (right-hand side of the equation)
self%gradCIC = matmul(self%GradJac, self%residuals)
self%gradCIC = self%gradCIC * 2.0_wp/self%refstdev
if (self%verbose) then 
  call Message%printMessage(' solveHessian_::gradCIC computed')
  io_real(1) = minval(self%gradCIC)
  io_real(2) = maxval(self%gradCIC)
  call Message%WriteValue(' solveHessian_::gradCIC range : ',io_real,2)
end if 

! solve by Cholesky decomposition 
Hessiancopy = self%Hessian
SL(1:8,1) = -self%gradCIC(1:8)
call DPOSV(UPLO, NN, NRHS, Hessiancopy, LDA, SL, LDB, INFO)
SOL(1:8) = SL(1:8,1)
if (self%verbose) write (*,*) ' solveHessian_::SOL : ', SOL 

! get the weighted norm of the solution vector
xi1max = maxval( self%XiPrime(0,:) )
xi2max = maxval( self%XiPrime(1,:) )
normdp = sqrt(xi1max * (SL(1,1)**2+SL(4,1)**2+SL(7,1)**2) + & 
              xi2max * (SL(2,1)**2+SL(5,1)**2+SL(8,1)**2) + SL(3,1)**2 + SL(6,1)**2)
! normdp = sum(SOL*SOL)
if (self%verbose) write (*,*) ' solveHessian_::normdp : ', normdp 

end subroutine solveHessian_

!--------------------------------------------------------------------------
recursive function getShapeFunction_(self, h, store) result(W)
!DEC$ ATTRIBUTES DLLEXPORT :: getShapeFunction_
 !! author: MDG
 !! version: 1.0
 !! date: 12/01/24
 !!
 !! convert input homography to a 3x3 shape function W and optionally store it
 !! as a class variable
 !!
 !! note that the interpolation routines sample the original image to the deformed
 !! image so we need to invert that behavior ...
 !! 

IMPLICIT NONE

class(DIC_T),INTENT(INOUT)      :: self
real(wp), INTENT(IN)            :: h(8)
logical, INTENT(IN), OPTIONAL   :: store
real(wp)                        :: W(3,3)

W(1,1) = 1.0_wp / (1.0_wp + h(1))
W(2,2) = 1.0_wp / (1.0_wp + h(5))
W(3,3) = 1.0_wp

W(1,2:3) = -h(2:3)
W(2,1) = -h(4)
W(2,3) = -h(6)
W(3,1) = -h(7)
W(3,2) = -h(8)

! W(1,1) = 1.0_wp + h(1)
! W(2,2) = 1.0_wp + h(5)
! W(3,3) = 1.0_wp

! W(1,2:3) = h(2:3)
! W(2,1) = h(4)
! W(2,3) = h(6)
! W(3,1) = -h(7)
! W(3,2) = h(8)

if (present(store)) then 
  if (store.eqv..TRUE.) then 
    self%W  = W 
  end if 
end if

end function getShapeFunction_

!--------------------------------------------------------------------------
recursive function getHomography_(self, W, store) result(h)
!DEC$ ATTRIBUTES DLLEXPORT :: getHomography_
 !! author: MDG
 !! version: 1.0
 !! date: 12/01/24
 !!
 !! convert input 3x3 shape function to homography h and optionally store it 
 !! as a class variable
 !!
 !! note that the interpolation routines sample the original image to the deformed
 !! image so we need to invert that behavior ...


IMPLICIT NONE

class(DIC_T),INTENT(INOUT)      :: self
real(wp), INTENT(IN)            :: W(3,3)
logical, INTENT(IN), OPTIONAL   :: store
real(wp)                        :: h(8)

h = (/ 1.0_wp/W(1,1)-1.0_wp, -W(1,2), -W(1,3), -W(2,1), 1.0_wp/W(2,2)-1.0_wp, -W(2,3),-W(3,1), -W(3,2) /)

! h = (/ W(1,1)-1.0_wp, W(1,2), W(1,3), W(2,1), W(2,2)-1.0_wp, W(2,3), -W(3,1), W(3,2) /)

if (present(store)) then 
  if (store.eqv..TRUE.) then 
    self%h  = h 
  end if 
end if

end function getHomography_

!--------------------------------------------------------------------------
recursive function homography2Fe_(self, h, PC, DD) result(W)
!DEC$ ATTRIBUTES DLLEXPORT :: homography2Fe_
 !! author: MDG
 !! version: 1.0
 !! date: 12/01/24
 !!
 !! convert a homography to a reduced deformation tensor (Fe-hat in Ernould papers)

class(DIC_T),INTENT(INOUT)  :: self
real(wp), INTENT(INOUT)     :: h(0:7)
real(wp), INTENT(IN)        :: PC(0:1)
real(wp), INTENT(IN)        :: DD
real(wp)                    :: W(3,3)

real(wp)                    :: beta0, Fe11, Fe12, Fe13, Fe21, Fe22, Fe23, Fe31, Fe32, Fe33

! h = -h 
! h11, h12, h13, h21, h22, h23, h31, h32
!  0    1    2    3    4    5    6    7

beta0 = 1.0_wp - h(6) * PC(0) - h(7) * PC(1)
Fe11 = 1.0_wp + h(0) + h(6) * PC(0)
Fe12 = h(1) + h(7) * PC(0)
Fe13 = (h(2) - h(0)*PC(0) - h(1)*PC(1) + PC(0)*(beta0 - 1.0_wp))/DD
Fe21 = h(3) + h(6) * PC(1)
Fe22 = 1.0_wp + h(4) + h(7) * PC(1)
Fe23 = (h(5) - h(3)*PC(0) - h(4)*PC(1) + PC(1)*(beta0 - 1.0_wp))/DD
Fe31 = DD * h(6)
Fe32 = DD * h(7)
Fe33 = beta0

W = reshape( (/ Fe11, Fe12, Fe13, Fe21, Fe22, Fe23, Fe31, Fe32, Fe33 /), (/ 3,3 /)) / beta0

end function homography2Fe_

!--------------------------------------------------------------------------
recursive function Fe2homography_(self, Fe_input, PC, DD) result(h)
!DEC$ ATTRIBUTES DLLEXPORT :: Fe2homography_
 !! author: MDG
 !! version: 1.0
 !! date: 12/01/24
 !!
 !! convert a reduced deformation tensor (with (3,3) entry = 1.0_wp) to a homography

class(DIC_T),INTENT(INOUT)  :: self
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

end function Fe2homography_





end module mod_DIC
