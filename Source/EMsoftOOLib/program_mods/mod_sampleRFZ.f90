! ###################################################################
! Copyright (c) 2013-2025, Marc De Graef Research Group/Carnegie Mellon University
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

module mod_sampleRFZ
  !! author: MDG
  !! version: 1.0
  !! date: 01/22/20
  !!
  !! class definition for the EMsampleRFZ program

use mod_kinds
use mod_global

IMPLICIT NONE

! namelist for the EMsampleRFZ program
type, public :: sampleRFZNameListType
    integer(kind=irg) :: pgnum
    integer(kind=irg) :: nsteps
    integer(kind=irg) :: gridtype
    integer(kind=irg) :: norientations
    integer(kind=irg) :: seed
    integer(kind=irg) :: hkl(15)
    integer(kind=irg) :: uvw(15)
    integer(kind=irg) :: norient(5)
    real(kind=dbl)    :: qFZ(4)
    real(kind=dbl)    :: axFZ(4)
    real(kind=dbl)    :: rodrigues(4)
    real(kind=dbl)    :: maxmisor
    real(kind=dbl)    :: conevector(3)
    real(kind=dbl)    :: semiconeangle
    real(kind=dbl)    :: tcos(5)
    character(6)      :: SO3cover
    character(fnlen)  :: xtalname
    character(fnlen)  :: samplemode
    character(fnlen)  :: euoutname
    character(fnlen)  :: cuoutname
    character(fnlen)  :: hooutname
    character(fnlen)  :: rooutname
    character(fnlen)  :: quoutname
    character(fnlen)  :: omoutname
    character(fnlen)  :: axoutname
    character(fnlen)  :: rvoutname
    character(fnlen)  :: stoutname
end type sampleRFZNameListType

type, public :: sampleRFZ_T
private
  character(fnlen)             :: nmldeffile = 'EMsampleRFZ.nml'
  type(sampleRFZNameListType)  :: nml

contains
private
  procedure, pass(self) :: readNameList_
  procedure, pass(self) :: getNameList_
  procedure, pass(self) :: CreateSampling_

  generic, public :: getNameList => getNameList_
  generic, public :: readNameList => readNameList_
  generic, public :: CreateSampling => CreateSampling_

end type sampleRFZ_T

! the constructor routine for this class
interface sampleRFZ_T
  module procedure sampleRFZ_constructor
end interface sampleRFZ_T

contains

!--------------------------------------------------------------------------
type(sampleRFZ_T) function sampleRFZ_constructor( nmlfile ) result(RFZ)
!DEC$ ATTRIBUTES DLLEXPORT :: sampleRFZ_constructor
!! author: MDG
!! version: 1.0
!! date: 01/22/20
!!
!! constructor for the sampleRFZ_T Class; reads the name list file

IMPLICIT NONE

character(fnlen), INTENT(IN)    :: nmlfile

call RFZ%readNameList(nmlfile)

end function sampleRFZ_constructor

!--------------------------------------------------------------------------
subroutine readNameList_(self, nmlfile, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: readNameList_
!! author: MDG
!! version: 1.0
!! date: 01/22/20
!!
!! read the namelist for the sampleRFZ_T Class

use mod_io

IMPLICIT NONE

class(sampleRFZ_T), INTENT(INOUT)  :: self
character(fnlen),INTENT(IN)        :: nmlfile
 !! full path to namelist file
logical,OPTIONAL,INTENT(IN)        :: initonly
 !! fill in the default values only; do not read the file

logical                            :: skipread = .FALSE.

integer(kind=irg)                  :: pgnum, nsteps, gridtype, norientations, seed, hkl(15), uvw(15), norient(5)
real(kind=dbl)                     :: rodrigues(4), qFZ(4), axFZ(4), maxmisor, conevector(3), semiconeangle, tcos(5)
character(6)                       :: SO3cover
character(fnlen)                   :: samplemode
character(fnlen)                   :: xtalname
character(fnlen)                   :: euoutname
character(fnlen)                   :: cuoutname
character(fnlen)                   :: hooutname
character(fnlen)                   :: rooutname
character(fnlen)                   :: quoutname
character(fnlen)                   :: omoutname
character(fnlen)                   :: axoutname
character(fnlen)                   :: rvoutname
character(fnlen)                   :: stoutname

! namelist components
namelist / RFZlist / pgnum, nsteps, gridtype, euoutname, cuoutname, hooutname, rooutname, quoutname, omoutname, axoutname, &
                     samplemode, rodrigues, maxmisor, conevector, semiconeangle, xtalname, qFZ, axFZ, rvoutname, stoutname, &
                     norientations, SO3cover, seed, hkl, uvw, norient, tcos

! initialize to default values
pgnum = 32
nsteps = 50
gridtype = 0
norientations = 100000
norient = (/ 100000, 0, 0, 0, 0 /)
hkl = (/ 0,0,0, 0,0,0, 0,0,0, 0,0,0, 0,0,0 /)
uvw = (/ 0,0,0, 0,0,0, 0,0,0, 0,0,0, 0,0,0 /)
seed = 1234
rodrigues = (/ 0.D0, 0.D0, 0.D0, 0.D0 /)  ! initialize as the identity rotation
qFZ= (/ 1.D0, 0.D0, 0.D0, 0.D0 /)         ! initialize as the identity rotation
axFZ= (/ 0.D0, 0.D0, 1.D0, 0.D0 /)        ! initialize as the identity rotation
maxmisor = 5.D0                           ! in degrees
tcos = (/ 5.D0, 0.D0, 0.D0, 0.D0, 0.D0 /) ! in degrees
samplemode = 'RFZ'                        ! or 'MIS' for sampling inside a ball with constant misorientation w.r.t. rodrigues
! or 'CON' for conical sampling around a unitvector for a cone with semi opening angle semiconangle
conevector = (/ 0.D0, 0.D0, 1.D0 /)       ! default unit vector for cone axis
semiconeangle = 2.0                       ! default opening semi-angle (in degrees)
SO3cover = 'single'
euoutname = 'undefined'
xtalname = 'undefined'
cuoutname = 'undefined'
hooutname = 'undefined'
rooutname = 'undefined'
quoutname = 'undefined'
omoutname = 'undefined'
axoutname = 'undefined'
rvoutname = 'undefined'
stoutname = 'undefined'

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
  open(UNIT=dataunit,FILE=trim(nmlfile),DELIM='apostrophe',STATUS='old')
  read(UNIT=dataunit,NML=RFZlist)
  close(UNIT=dataunit,STATUS='keep')

end if

! and copy the variables to the namelist variable
self%nml%pgnum  = pgnum
self%nml%nsteps = nsteps
self%nml%gridtype = gridtype
self%nml%norientations = norientations
self%nml%norient = norient
self%nml%hkl = hkl 
self%nml%uvw = uvw
self%nml%seed = seed  
self%nml%rodrigues = rodrigues
self%nml%qFZ = qFZ
self%nml%axFZ = axFZ
self%nml%maxmisor = maxmisor
self%nml%tcos = tcos
self%nml%samplemode = samplemode
self%nml%conevector = conevector
self%nml%semiconeangle = semiconeangle
self%nml%SO3cover = SO3cover
self%nml%xtalname = xtalname
self%nml%euoutname = euoutname
self%nml%cuoutname = cuoutname
self%nml%hooutname = hooutname
self%nml%rooutname = rooutname
self%nml%quoutname = quoutname
self%nml%omoutname = omoutname
self%nml%axoutname = axoutname
self%nml%rvoutname = rvoutname
self%nml%stoutname = stoutname

end subroutine readNameList_

!--------------------------------------------------------------------------
function getNameList_(self) result(nml)
!DEC$ ATTRIBUTES DLLEXPORT :: getNameList_
!! author: MDG
!! version: 1.0
!! date: 01/22/20
!!
!! pass the namelist for the sampleRFZ_T Class to the calling program

IMPLICIT NONE

class(sampleRFZ_T), INTENT(INOUT)    :: self
type(sampleRFZNameListType)          :: nml

nml = self%nml

end function getNameList_

!--------------------------------------------------------------------------
!
! SUBROUTINE:CreateSampling
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief Generate a sampling of the Rodrigues Fundamental Zone for a given xtal symmetry
!
!> @todo add an HDF5 output option
!
!> @param nmlfile namelist file name
!
!> @date 05/29/14 MDG 1.0 original
!> @date 12/09/14 MDG 2.0 changed rfznl handling
!> @date 08/19/15 MDG 2.1 added all rotation representations as output options
!> @date 12/22/16 MDG 2.2 added option to generate reduced sampling inside constant misorientation ball
!> @date 02/01/17 MDG 2.3 added option to generate sampling inside a conical volume in Rodrigues space
!> @date 08/16/17 MDG 2.4 added option to generate uniform fiber texture sampling in Rodrigues space
!> @date 12/21/22 MDG 3.0 added Super-Fibonacci sampling 
!> @date 12/23/22 MDG 3.1 added Marsaglia and uniform sampling
!> @date 01/09/23 MDG 3.2 added von Mises-Fisher and Watson sampling
!> @date 01/10/23 MDG 3.3 added texture component sampling
!--------------------------------------------------------------------------
subroutine CreateSampling_(self, EMsoft)
!DEC$ ATTRIBUTES DLLEXPORT :: CreateSampling_
!! author: MDG
!! version: 1.0
!! date: 01/22/20
!!
!! Generate a sampling of the Rodrigues Fundamental Zone for a given xtal symmetry

use mod_kinds
use mod_global
use mod_EMsoft
use mod_crystallography
use mod_quaternions
use mod_dirstats
use mod_io
use mod_symmetry
use mod_rotations
use mod_so3
use mod_CliffordTorus
use HDF5
use mod_HDFsupport

IMPLICIT NONE

class(sampleRFZ_T), INTENT(INOUT)  :: self
type(EMsoft_T), INTENT(INOUT)      :: EMsoft

type(sampleRFZNameListType)        :: rfznl
type(IO_T)                         :: Message
type(SpaceGroup_T)                 :: SG
type(Cell_T)                       :: cell
type(q_T)                          :: qFZ, q
type(a_T)                          :: a
type(r_T)                          :: r
type(o_T)                          :: o
type(so3_T)                        :: SO
type(CliffordTorus_T)              :: CT
type(DirStat_T)                    :: DS
type(Quaternion_T)                 :: mu
type(QuaternionArray_T)            :: qAR

integer(kind=irg)                  :: i, j, num, m, io_int(4), FZcnt, FZtype, FZorder, numTC, seed
real(kind=dbl)                     :: ax(4), calpha, conevector(3), x, kappa, cnvec(3), cbvec(3), t(3), &
                                      h, k, l, ih, ik, il, idiff, eps = 0.0001D0, om(3,3), io_dble(4)
real(kind=dbl),allocatable         :: itmp(:,:)
logical                            :: doeu = .FALSE., docu = .FALSE., doho = .FALSE., doqu = .FALSE., dorv = .FALSE., &
                                      dost = .FALSE., doom = .FALSE., doax = .FALSE., doro = .FALSE., newpoint, &
                                      rotateFZ = .FALSE.
character(fnlen)                   :: filename
real(kind=dbl),allocatable         :: SGdirec(:,:,:)
character(2)                       :: listmode

call openFortranHDFInterface()

! first get the name list
associate( rfznl => self%nml )
! rfznl = self%getNameList()

! determine which files to create
if (trim(rfznl%euoutname).ne.'undefined') doeu = .TRUE.
if (trim(rfznl%cuoutname).ne.'undefined') docu = .TRUE.
if (trim(rfznl%hooutname).ne.'undefined') doho = .TRUE.
if (trim(rfznl%quoutname).ne.'undefined') doqu = .TRUE.
if (trim(rfznl%rooutname).ne.'undefined') doro = .TRUE.
if (trim(rfznl%omoutname).ne.'undefined') doom = .TRUE.
if (trim(rfznl%axoutname).ne.'undefined') doax = .TRUE.
if (trim(rfznl%stoutname).ne.'undefined') dost = .TRUE.
if (trim(rfznl%rvoutname).ne.'undefined') dorv = .TRUE.

! do we need to rotate the Rodrigues FZ before sampling ?
if (sum(rfznl%qFZ - (/ 1.D0, 0.D0, 0.D0, 0.D0 /)) .ne. 0.D0) then
  rotateFZ = .TRUE.
  qFZ = q_T( qdinp=rfznl%qFZ )
end if

! or is there an axis-angle pair for the FZ rotation ?
if (sum(rfznl%axFZ - (/0.D0, 0.D0, 1.D0, 0.D0 /)) .ne. 0.D0) then
  ax = rfznl%axFZ
  x = sqrt(sum(ax(1:3)*ax(1:3)))
  if (x.gt.0.D0) then
    ax(1:3) = ax(1:3) / x
  else
    ax(1:3) = (/ 0.D0, 0.D0, 1.D0 /)
  end if
  ax(4) = ax(4) * cPi / 180.D0
  a = a_T( adinp = ax )
  qFZ = a%aq()
  rotateFZ = .TRUE.
end if

! a bit of output
call Message%printMessage(' Starting computation for point group '//PGTHD(rfznl%pgnum))

! if samplemode is set to FIB or TXC, a fiber texture will be generated, so we
! need to properly initialize the symmetry operations...
if ( (trim(rfznl%samplemode).eq.'FIB').or.(trim(rfznl%samplemode).eq.'TXC') ) then
  if (rfznl%xtalname.eq.'undefined') then
    call Message%printError('CreateSampling','Routine requires an .xtal filename for this sampling mode')
  endif
! initialize crystal
  call SG%setSpaceGroupreduce(.FALSE.)
  call cell%getCrystalData(rfznl%xtalname, SG, EMsoft)
  if (trim(rfznl%samplemode).eq.'FIB') then
    conevector = rfznl%conevector/sqrt(sum(rfznl%conevector**2))

    SGdirec = SG%getSpaceGroupPGdirecMatrices()

  ! first take the identity
    allocate(itmp(SG%getSpaceGroupNUMpt(),3))
    itmp = 0.0D0
    j=1
    h=conevector(1)
    k=conevector(2)
    l=conevector(3)
    itmp(j,1:3)=conevector(1:3)
    write (*,*) 'fiber vector : ',itmp(j,1:3)

  ! multiply with all point group elements
    do i=2,SG%getSpaceGroupNUMpt()
       ih=SGdirec(i,1,1)*h+SGdirec(i,1,2)*k+SGdirec(i,1,3)*l
       ik=SGdirec(i,2,1)*h+SGdirec(i,2,2)*k+SGdirec(i,2,3)*l
       il=SGdirec(i,3,1)*h+SGdirec(i,3,2)*k+SGdirec(i,3,3)*l

  ! is this a new point ?
       newpoint=.TRUE.
       do m=1,j+1
         idiff=(itmp(m,1)-ih)**2+(itmp(m,2)-ik)**2+(itmp(m,3)-il)**2
         if (idiff.lt.eps) newpoint=.FALSE.
       end do

       if (newpoint) then
         j=j+1
         itmp(j,1:3)=(/ ih, ik, il /)
       end if

    end do
    num=j
    write (*,*) 'total number of equivalent fiber axes ',num
  end if 
end if

! determine which function we should call for this point group symmetry
SO = so3_T( rfznl%pgnum )
call SO%setGridType( rfznl%gridtype )

! get the linked list for the FZ for point group symmetry pgnum for nsteps along the cubic semi-edge
if (trim(rfznl%samplemode).eq.'RFZ') then
  if (rotateFZ.eqv..TRUE.) then
    call SO%SampleRFZ(rfznl%nsteps,qFZ)
  else
    call SO%SampleRFZ(rfznl%nsteps)
  end if
  listmode = 'FZ'
end if
if (trim(rfznl%samplemode).eq.'MIS') then
  io_dble = rfznl%rodrigues
  call Message%WriteValue(' Rodrigues vector = ', io_dble, 4)
  call SO%sample_isoCubeFilled(rfznl%maxmisor, rfznl%nsteps)
  r = r_T( rdinp=rfznl%rodrigues )
  call SO%SampleIsoMisorientation(r, rfznl%maxmisor)
  listmode = 'CM'
end if
if (trim(rfznl%samplemode).eq.'CON') then
  conevector = rfznl%conevector/sqrt(sum(rfznl%conevector**2))
  io_dble(1:3) = conevector
  call Message%WriteValue(' cone axis unit vector   = ', io_dble, 3)
  io_dble(1) = rfznl%semiconeangle
  call Message%WriteValue(' cone semi opening angle = ', io_dble, 1)
  calpha = cos(rfznl%semiconeangle/rtod)
  io_dble(1) = calpha
  call Message%WriteValue(' minimum dot product    = ', io_dble, 1)
  call SO%sample_Cone(conevector, calpha, rfznl%nsteps)
  listmode = 'CO'
end if
if (trim(rfznl%samplemode).eq.'FIB') then
  conevector = rfznl%conevector/sqrt(sum(rfznl%conevector**2))
  io_dble(1:3) = conevector
  call Message%WriteValue(' fiber axis unit vector   = ', io_dble, 3)
  io_dble(1) = rfznl%semiconeangle
  call message%writevalue(' fiber cone semi opening angle = ', io_dble, 1)
  calpha = cos(rfznl%semiconeangle*dtor)
  do i=1,num
    io_int(1) = i
    io_dble(1:3) = itmp(i,1:3)
    call Message%WriteValue('', io_int, 1, frm="(I3,' -> ')", advance='n')
    call Message%WriteValue('', io_dble, 3)
  end do
  call SO%sample_Fiber(itmp, num, calpha, rfznl%nsteps)
  listmode = 'FB'
end if
if (trim(rfznl%samplemode).eq.'SHO') then
  call Message%printMessage(' performing Shoemake sampling ')
  call SO%sample_SHO( rfznl%norientations, rfznl%pgnum )
  io_int(1) = rfznl%norientations
  call Message%WriteValue('Number of Shoemake orientations requested = ',io_int,1,"(I10)")
  listmode = 'SF'
end if
if (trim(rfznl%samplemode).eq.'SFS') then
  call Message%printMessage(' performing Super-Fibonacci sampling ')
  call SO%sample_SFS( rfznl%norientations, rfznl%pgnum )
  io_int(1) = rfznl%norientations
  call Message%WriteValue('Number of Super-Fibonacci orientations requested = ',io_int,1,"(I10)")
  listmode = 'SF'
end if
if (trim(rfznl%samplemode).eq.'MAR') then
  call Message%printMessage(' Performing Marsaglia sampling ')
  call SO%sample_MAR( rfznl%norientations )
  io_int(1) = rfznl%norientations
  call Message%WriteValue('Number of Super-Fibonacci orientations requested = ',io_int,1,"(I10)")
  listmode = 'MA'
end if
if (trim(rfznl%samplemode).eq.'UNI') then
  call Message%printMessage(' Performing uniform linear sampling ')
  call SO%sample_UNI( rfznl%norientations )
  io_int(1) = rfznl%norientations
  call Message%WriteValue('Number of uniform linear orientations requested = ',io_int,1,"(I10)")
  listmode = 'UN'
end if
if (trim(rfznl%samplemode).eq.'vMF') then
  call Message%printMessage(' Performing von Mises-Fisher sampling')
  DS = DirStat_T( DStype='VMF', PGnum=rfznl%pgnum )
  r = r_T( rdinp=rfznl%rodrigues )
  q = r%rq()
  mu = Quaternion_T( qd = q%q_copyd() )
  kappa =  1.D0/ ( 1.D0 - cos( rfznl%maxmisor * dtor ) )
  io_dble(1) = kappa
  call Message%WriteValue(' kappa, mu ', io_dble, 1)
  call mu%quat_print()
  qAR = DS%SampleDS( rfznl%norientations, rfznl%seed, mu, kappa )
  call SO%setFZcnt( rfznl%norientations, 'FZ' )
  call SO%QuaternionArraytonewlist( qAR, 'FZ', rfznl%SO3cover )
  io_int(1) = rfznl%norientations
  call Message%WriteValue('Number of von Mises-Fisher orientations requested = ',io_int,1,"(I10)")
  listmode = 'FZ'
end if
if (trim(rfznl%samplemode).eq.'WAT') then
  call Message%printMessage(' Performing Watson sampling')
  DS = DirStat_T( DStype='WAT', PGnum=rfznl%pgnum )
  r = r_T( rdinp=rfznl%rodrigues )
  q = r%rq()
  mu = Quaternion_T( qd = q%q_copyd() )
  kappa =  1.D0/ ( 1.D0 - cos( rfznl%maxmisor * dtor ) )
  io_dble(1) = kappa
  call Message%WriteValue(' kappa, mu ', io_dble, 1)
  call mu%quat_print()
  qAR = DS%SampleDS( rfznl%norientations, rfznl%seed, mu, kappa )
  call SO%QuaternionArraytonewlist( qAR, 'FZ', rfznl%SO3cover )
  io_int(1) = rfznl%norientations
  call Message%WriteValue(' Number of Watson orientations requested = ',io_int,1,"(I10)")
  listmode = 'FZ'
end if
if (trim(rfznl%samplemode).eq.'TXC') then
  call Message%printMessage(' Performing texture component sampling')
  numTC = 0
  do i=1,5 
    if (rfznl%norient(i).ne.0) numTC = numTC+1
  end do 
  io_int(1) = numTC
  call Message%WriteValue(' Number of texture components found = ',io_int,1,"(I2)")
! get the orientation matrices and then the corresponding mean quaternions 
  seed = rfznl%seed
  DS = DirStat_T( DStype='WAT', PGnum=rfznl%pgnum )
  do i=1,numTC 
! transform the two vectors to the Cartesian crystallographic reference frame
    call cell%TransSpace(dble(rfznl%hkl((i-1)*3+1:i*3)), cnvec, 'r', 'c') 
    call cell%TransSpace(dble(rfznl%uvw((i-1)*3+1:i*3)), cbvec, 'd', 'c') 
! normalize them
    call cell%NormVec(cnvec, 'c')
    call cell%NormVec(cbvec, 'c')
! get the cross product n x b and normalize
    call cell%CalcCross(cnvec,cbvec,t,'c','c',0)
    call cell%NormVec(t, 'c')
! get these into a rotation matrix with b,t,n in the columns (in that order)
    om(1:3,1) = cbvec(1:3)
    om(1:3,2) = t(1:3)
    om(1:3,3) = cnvec(1:3)
    o = o_T( odinp = om )
    call o%o_print(' om ')
    q = o%oq()
    mu = Quaternion_T( qd = q%q_copyd() )
    kappa =  1.D0/ ( 1.D0 - cos( rfznl%tcos(i) * dtor ) )
    io_dble(1) = kappa
    call Message%WriteValue(' kappa, mu ', io_dble, 1)
    call mu%quat_print()
    qAR = DS%SampleDS( rfznl%norient(i), seed, mu, kappa )
    if (i.eq.1) then 
      call SO%QuaternionArraytonewlist( qAR, 'FZ' )
    else
      call SO%QuaternionArrayappendtolist( qAR, 'FZ' )
    end if
  end do 
  io_int(1) = sum( rfznl%norient )
  call Message%WriteValue(' Total number of texture component orientations requested = ',io_int,1,"(I10)")
  listmode = 'FZ'
end if 

FZcnt = SO%getListCount(listmode)
io_int(1) = FZcnt
call Message%WriteValue('Total number of unique orientations generated = ',io_int,1,"(I10)")

! generate a list of all orientations in Euler angle format (if requested)
if (doeu) then
  filename = EMsoft%generateFilePath('EMdatapathname',rfznl%euoutname)
  call SO%writeOrientationstoFile(filename, 'eu', listmode)
end if

! generate a list of all orientations in cubochoric format (if requested)
if (docu) then
  filename = EMsoft%generateFilePath('EMdatapathname',rfznl%cuoutname)
  call SO%writeOrientationstoFile(filename, 'cu', listmode)
end if

! generate a list of all orientations in homochoric format (if requested)
if (doho) then
  filename = EMsoft%generateFilePath('EMdatapathname',rfznl%hooutname)
  call SO%writeOrientationstoFile(filename, 'ho', listmode)
end if

! generate a list of all orientations in quternion format (if requested)
if (doqu) then
  filename = EMsoft%generateFilePath('EMdatapathname',rfznl%quoutname)
  call SO%writeOrientationstoFile(filename, 'qu', listmode)
end if

! generate a list of all orientations in Rodrigues format (if requested)
if (doro) then
  filename = EMsoft%generateFilePath('EMdatapathname',rfznl%rooutname)
  call SO%writeOrientationstoFile(filename, 'ro', listmode)
end if

! generate a list of all orientations in orientation matrix format (if requested)
if (doom) then
  filename = EMsoft%generateFilePath('EMdatapathname',rfznl%omoutname)
  call SO%writeOrientationstoFile(filename, 'om', listmode)
end if

! generate a list of all orientations in axis angle pair format (if requested)
if (doax) then
  filename = EMsoft%generateFilePath('EMdatapathname',rfznl%axoutname)
  call SO%writeOrientationstoFile(filename, 'ax', listmode)
end if

! generate a list of all orientations in stereographic format (if requested)
if (dost) then
  filename = EMsoft%generateFilePath('EMdatapathname',rfznl%stoutname)
  call SO%writeOrientationstoFile(filename, 'st', listmode)
end if

! generate a list of all orientations in axis angle pair format (if requested)
if (dorv) then
  filename = EMsoft%generateFilePath('EMdatapathname',rfznl%rvoutname)
  call SO%writeOrientationstoFile(filename, 'rv', listmode)
end if

if (doeu) call Message%printMessage('Euler angles stored in file '//rfznl%euoutname)
if (docu) call Message%printMessage('Cubochoric representation stored in file '//rfznl%cuoutname)
if (doho) call Message%printMessage('Homochoric representation stored in file '//rfznl%hooutname)
if (doqu) call Message%printMessage('Quaternion representation stored in file '//rfznl%quoutname)
if (doro) call Message%printMessage('Rodrigues vector representation stored in file '//rfznl%rooutname)
if (doom) call Message%printMessage('Orientation matrix representation stored in file '//rfznl%omoutname)
if (doax) call Message%printMessage('Axis-angle pair representation stored in file '//rfznl%axoutname)
if (dost) call Message%printMessage('Stereographic representation stored in file '//rfznl%stoutname)
if (dorv) call Message%printMessage('Rotation vector representation stored in file '//rfznl%rvoutname)

end associate 

end subroutine CreateSampling_


end module mod_sampleRFZ
