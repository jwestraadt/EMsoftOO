! ###################################################################
! Copyright (c) 2013-2023, Marc De Graef Research Group/Carnegie Mellon University
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

module mod_HREBSDDIC
  !! author: MDG (OO version)
  !! version: 1.0 
  !! date: 11/07/24
  !!
  !! class definition for the EMHREBSDDIC program
  !! uses the inverse compositional Gauss–Newton algorithm from ATEX

use mod_kinds
use mod_global
use mod_ppEBSD, only : ppEBSDNameListType 
use mod_DIfiles, only : DictionaryIndexingNameListType

IMPLICIT NONE 

! namelist for the EMHREBSDDIC program
type, public :: HREBSDDICNameListType
   integer(kind=irg)    :: patx
   integer(kind=irg)    :: paty
   integer(kind=irg)    :: nbx
   integer(kind=irg)    :: nby
   integer(kind=irg)    :: maxnumit
   integer(kind=irg)    :: nthreads
   real(kind=sgl)       :: C11
   real(kind=sgl)       :: C12
   real(kind=sgl)       :: C44
   real(kind=sgl)       :: C13
   real(kind=sgl)       :: C33
   real(kind=dbl)       :: mindeltap
   character(fnlen)     :: patternfile
   character(fnlen)     :: DIfile
   character(fnlen)     :: datafile
   character(fnlen)     :: ppEBSDnml
   character(fnlen)     :: DInml
   character(3)         :: crystal
   logical              :: verbose
end type HREBSDDICNameListType

! class definition
type, public :: HREBSD_DIC_T
private 
  character(fnlen)                        :: nmldeffile = 'EMHREBSDDIC.nml'
  type(HREBSDDICNameListType)             :: nml 
  type(ppEBSDNameListType)                :: ppEBSDnml 
  type(DictionaryIndexingNameListType)    :: DInml

contains
private 
  procedure, pass(self) :: readNameList_
  procedure, pass(self) :: writeHDFNameList_
  procedure, pass(self) :: getNameList_
  procedure, pass(self) :: HREBSD_DIC_

  generic, public :: getNameList => getNameList_
  generic, public :: writeHDFNameList => writeHDFNameList_
  generic, public :: readNameList => readNameList_
  generic, public :: HREBSD_DIC => HREBSD_DIC_

end type HREBSD_DIC_T

! the constructor routine for this class 
interface HREBSD_DIC_T
  module procedure HREBSDDIC_constructor
end interface HREBSD_DIC_T

contains

!--------------------------------------------------------------------------
type(HREBSD_DIC_T) function HREBSDDIC_constructor( nmlfile ) result(HREBSDDIC)
!DEC$ ATTRIBUTES DLLEXPORT :: HREBSDDIC_constructor
!! author: MDG 
!! version: 1.0 
!! date: 11/07/24
!!
!! constructor for the HREBSD_DIC_T Class; 

use mod_ppEBSD, only : ppEBSD_T
use mod_DIfiles, only : DIfile_T 

IMPLICIT NONE

character(fnlen), INTENT(IN)  :: nmlfile 
type(ppEBSD_T)                :: ppEBSD
type(DIfile_T)                :: DIFT

call HREBSDDIC%readNameList(nmlfile)

! we also need to read in the namelist fileis from the EMppEBSD and EMDI programs (for now)
! this file defines where the original patterns are located as well 
! as information about their size etc.
ppEBSD = ppEBSD_T( HREBSDDIC%nml%ppEBSDnml )
HREBSDDIC%ppEBSDnml = ppEBSD%get_nml()

DIFT = DIfile_T()
call DIFT%readNameList(HREBSDDIC%nml%DInml)
HREBSDDIC%DInml = DIFT%getNameList()

end function HREBSDDIC_constructor

!--------------------------------------------------------------------------
subroutine HREBSDDIC_destructor(self) 
!DEC$ ATTRIBUTES DLLEXPORT :: HREBSDDIC_destructor
!! author: MDG 
!! version: 1.0 
!! date: 11/07/24
!!
!! destructor for the HREBSD_DIC_T Class
 
IMPLICIT NONE

type(HREBSD_DIC_T), INTENT(INOUT)  :: self 

call reportDestructor('HREBSD_DIC_T')

end subroutine HREBSDDIC_destructor

!--------------------------------------------------------------------------
subroutine readNameList_(self, nmlfile, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: readNameList_
!! author: MDG 
!! version: 1.0 
!! date: 11/07/24
!!
!! read the namelist from an nml file for the HREBSD_DIC_T Class 

use mod_io 
use mod_EMsoft

IMPLICIT NONE 

class(HREBSD_DIC_T), INTENT(INOUT)   :: self
character(fnlen),INTENT(IN)          :: nmlfile
 !! full path to namelist file 
logical,OPTIONAL,INTENT(IN)          :: initonly
 !! fill in the default values only; do not read the file

type(EMsoft_T)                       :: EMsoft 
type(IO_T)                           :: Message       
logical                              :: skipread = .FALSE.

integer(kind=irg)                    :: patx
integer(kind=irg)                    :: paty
integer(kind=irg)                    :: nbx
integer(kind=irg)                    :: nby
integer(kind=irg)                    :: maxnumit
integer(kind=irg)                    :: nthreads
real(kind=sgl)                       :: C11
real(kind=sgl)                       :: C12
real(kind=sgl)                       :: C44
real(kind=sgl)                       :: C13
real(kind=sgl)                       :: C33
real(kind=dbl)                       :: mindeltap
character(fnlen)                     :: patternfile
character(fnlen)                     :: DIfile
character(fnlen)                     :: datafile
character(fnlen)                     :: ppEBSDnml
character(fnlen)                     :: DInml
character(3)                         :: crystal
logical                              :: verbose

namelist / HREBSDDICdata / patx, paty, nthreads, C11, C12, C44, C13, C33, DIfile, DInml, mindeltap, verbose, & 
                           datafile, patternfile, DIfile, ppEBSDnml, crystal, nbx, nby, maxnumit

patx = 0
paty = 0
nbx = 0
nby = 0
maxnumit = 50
nthreads = 1
C11 = 276.0
C12 = 159.0
C44 = 132.0
C13 = 0.0
C33 = 0.0
mindeltap = 0.001D0
patternfile = 'undefined'
datafile = 'undefined'
DIfile = 'undefined'
ppEBSDnml = 'undefined'
DInml = 'undefined'
crystal = 'cub' 
verbose = .FALSE. 

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
    open(UNIT=dataunit,FILE=trim(nmlfile),DELIM='apostrophe',STATUS='old')
    read(UNIT=dataunit,NML=HREBSDDICdata)
    close(UNIT=dataunit,STATUS='keep')

! check for required entries
    if (trim(patternfile).eq.'undefined') then
      call Message%printError('readNameList:',' patternfile name is undefined in '//nmlfile)
    end if

    ! if (trim(DIfile).eq.'undefined') then
    !   call Message%printError('readNameList:',' DIfile file name is undefined in '//nmlfile)
    ! end if

    if (trim(ppEBSDnml).eq.'undefined') then
      call Message%printError('readNameList:',' ppEBSDnml file name is undefined in '//nmlfile)
    end if

    if (trim(DInml).eq.'undefined') then
      call Message%printError('readNameList:',' DInml file name is undefined in '//nmlfile)
    end if

    if (trim(datafile).eq.'undefined') then
      call Message%printError('readNameList:',' datafile file name is undefined in '//nmlfile)
    end if

end if

self%nml%patx = patx
self%nml%paty = paty
self%nml%nbx = nbx
self%nml%nby = nby
self%nml%maxnumit = maxnumit
self%nml%nthreads = nthreads
self%nml%C11 = C11
self%nml%C12 = C12
self%nml%C44 = C44
self%nml%C13 = C13
self%nml%C33 = C33
self%nml%mindeltap = mindeltap
self%nml%patternfile = patternfile
self%nml%DIfile = DIfile
self%nml%datafile = datafile
self%nml%ppEBSDnml = ppEBSDnml
self%nml%DInml = DInml
self%nml%crystal = crystal
self%nml%verbose = verbose

end subroutine readNameList_

!--------------------------------------------------------------------------
function getNameList_(self) result(nml)
!DEC$ ATTRIBUTES DLLEXPORT :: getNameList_
!! author: MDG 
!! version: 1.0 
!! date: 11/07/24
!!
!! pass the namelist for the HREBSD_DIC_T Class to the calling program

IMPLICIT NONE 

class(HREBSD_DIC_T), INTENT(INOUT)           :: self
type(HREBSDDICNameListType)                  :: nml

nml = self%nml

end function getNameList_

!--------------------------------------------------------------------------
recursive subroutine writeHDFNameList_(self, HDF, HDFnames)
!DEC$ ATTRIBUTES DLLEXPORT :: writeHDFNameList_
!! author: MDG 
!! version: 1.0 
!! date: 11/07/24
!!
!! write namelist to HDF file

use mod_HDFsupport
use mod_HDFnames
use stringconstants 

use ISO_C_BINDING

IMPLICIT NONE

class(HREBSD_DIC_T), INTENT(INOUT)      :: self 
type(HDF_T), INTENT(INOUT)              :: HDF
type(HDFnames_T), INTENT(INOUT)         :: HDFnames

integer(kind=irg),parameter             :: n_int = 7, n_real = 6
integer(kind=irg)                       :: hdferr,  io_int(n_int), vb
real(kind=sgl)                          :: io_real(n_real)
character(20)                           :: intlist(n_int), reallist(n_real)
character(fnlen)                        :: dataset, sval(1),groupname
character(fnlen,kind=c_char)            :: line2(1),line10(10)

associate( enl => self%nml )

vb = 0
if (self%nml%verbose.eqv..TRUE.) vb = 1 

! create the group for this namelist
hdferr = HDF%createGroup(HDFnames%get_NMLlist())

! write all the single integers
io_int = (/ enl%nthreads, enl%patx, enl%paty, enl%nbx, enl%nby, enl%maxnumit, vb /)
intlist(1) = 'nthreads'
intlist(2) = 'patx'
intlist(3) = 'paty'
intlist(4) = 'nbx'
intlist(5) = 'nby'
intlist(6) = 'maxnumit'
intlist(7) = 'verbose'
call HDF%writeNMLintegers(io_int, intlist, n_int)

! write all the single reals
io_real = (/ enl%C11, enl%C12, enl%C44, enl%C13, enl%C33, real(enl%mindeltap) /)
reallist(1) = 'C11'
reallist(2) = 'C12'
reallist(3) = 'C44'
reallist(4) = 'C13'
reallist(5) = 'C33'
reallist(6) = 'mindeltap'
call HDF%writeNMLreals(io_real, reallist, n_real)

! write all the strings
dataset = 'DIfile' ! SC_DIfile
line2(1) = trim(enl%DIfile)
hdferr = HDF%writeDatasetStringArray(dataset, line2, 1)
if (hdferr.ne.0) call HDF%error_check('writeHDFNameList: unable to create DIfile dataset', hdferr)

dataset = SC_datafile
line2(1) = trim(enl%datafile)
hdferr = HDF%writeDatasetStringArray(dataset, line2, 1)
if (hdferr.ne.0) call HDF%error_check('writeHDFNameList: unable to create datafile dataset', hdferr)

dataset = 'patternfile'  ! SC_patternfile
line2(1) = trim(enl%patternfile)
hdferr = HDF%writeDatasetStringArray(dataset, line2, 1)
if (hdferr.ne.0) call HDF%error_check('writeHDFNameList: unable to create patternfile dataset', hdferr)

dataset = 'ppEBSDnml'
line2(1) = trim(enl%ppEBSDnml)
hdferr = HDF%writeDatasetStringArray(dataset, line2, 1)
if (hdferr.ne.0) call HDF%error_check('writeHDFNameList: unable to create ppEBSDnml dataset', hdferr)

dataset = 'DInml'
line2(1) = trim(enl%DInml)
hdferr = HDF%writeDatasetStringArray(dataset, line2, 1)
if (hdferr.ne.0) call HDF%error_check('writeHDFNameList: unable to create DInml dataset', hdferr)

dataset = 'crystal'
line2(1) = trim(enl%crystal)
hdferr = HDF%writeDatasetStringArray(dataset, line2, 1)
if (hdferr.ne.0) call HDF%error_check('writeHDFNameList: unable to create crystal dataset', hdferr)

! pop this group off the stack
call HDF%pop()
call HDF%pop()

end associate

end subroutine writeHDFNameList_

!--------------------------------------------------------------------------
subroutine HREBSD_DIC_(self, EMsoft, progname, HDFnames)
!DEC$ ATTRIBUTES DLLEXPORT :: HREBSD_DIC_
!! author: MDG 
!! version: 1.0 
!! date: 11/21/24
!!
!! perform the computations

use mod_EMsoft
use HDF5
use mod_HDFsupport
use mod_HDFnames
use mod_OMPsupport
use mod_quaternions
use mod_rotations 
use mod_io
use mod_HDFsupport
use mod_math
use HDF5
use mod_ppEBSD
use mod_DIC
use mod_DIfiles
use mod_timing
use mod_memory
use mod_fftw3
use stringconstants
use mod_platformsupport
use bspline_module
use bspline_kinds_module, only: wp, ip
use ISO_C_BINDING

IMPLICIT NONE 

class(HREBSD_DIC_T), INTENT(INOUT)      :: self
type(EMsoft_T), INTENT(INOUT)           :: EMsoft
character(fnlen), INTENT(INOUT)         :: progname 
type(HDFnames_T), INTENT(INOUT)         :: HDFnames

type(DIfile_T)                          :: DIFT
type(QuaternionArray_T)                 :: qAR
type(IO_T)                              :: Message
type(Timing_T)                          :: timer
type(memory_T)                          :: mem, memth
type(HDF_T)                             :: HDF
type(e_T)                               :: eu
type(o_T)                               :: om
type(q_T)                               :: qu
type(a_T)                               :: ax
type(Quaternion_T)                      :: quat
type(DIC_T)                             :: DIC
type(HDFnames_T)                        :: HDFnames2

character(fnlen)                        :: inpfile, HDFstring
real(kind=dbl)                          :: stepsizes(3), fpar(3), sig, totaltilt, ave, cosang, sinang
real(kind=sgl)                          :: io_real(6), ss, tstop
real(kind=dbl),allocatable              :: PC(:,:), homographies(:,:), normdp(:), residuals(:)
real(kind=sgl),allocatable              :: exptpattern(:,:), targetpattern(:,:)
integer(kind=irg),allocatable           :: nit(:)
integer(kind=irg)                       :: hdferr, binx, biny, L, recordsize, patsz, ROI_size, sz(2), i, j, kk, numangles, &
                                           istat, interp_grid, interp_size, info, offset, nbx, nby, ii, jj, ierr, io_int(2), &
                                           numpats, TID
real(kind=dbl)                          :: interp_step, std
real(kind=dbl)                          :: Wnew(3,3), Winv(3,3), oldW(3,3)
real(wp)                                :: hg(8), W(3,3), ndp, oldnorm, SOL(8), Hessian(8,8), CIC, PCx, PCy
character(11)                           :: dstr
character(15)                           :: tstrb
character(15)                           :: tstre
! integer(HSIZE_T)                        :: dims2(2), dims3(3), offset3(3)
real(kind=dbl)                          :: C(6,6)
character(fnlen)                        :: DIfile, groupname, dataset, datagroupname, outname, fname, hostname
logical                                 :: f_exists, g_exists, overwrite = .TRUE.
integer(kind=4)                         :: hnstat

! is this program executed from home or work ?  [code will be removed in Release version]
hnstat = system_hostnm(hostname) 
write (*,*) trim(hostname)

! this program reads a dot product EMDI file, including all the experimental parameters
! as well as a pre-processed pattern file generated by EMppEBSD.

associate( enl => self%nml, dinl=>self%DInml )

call setRotationPrecision('d')
call openFortranHDFInterface()

! read all parameters from the EMDI output file
! for now, we only read the EMDI.nml file, so we do not really need the full dot product file
! in a later version we will likely need the complete file
! DIfile = EMsoft%generateFilePath('EMdatapathname',trim(enl%DIfile))

! HDFnames2 = HDFnames_T()
! call HDFnames2%set_NMLfiles(SC_NMLfiles)
! call HDFnames2%set_NMLfilename(SC_DictionaryIndexingNML)
! call HDFnames2%set_NMLparameters(SC_NMLparameters)
! call HDFnames2%set_NMLlist(SC_DictionaryIndexingNameListType)

! call DIFT%readDotProductFile(EMsoft, HDF, HDFnames2, DIfile, hdferr, &
!                              getPhi1=.TRUE., &
!                              getPhi=.TRUE., &
!                              getPhi2=.TRUE.)

fpar = (/ real(dinl%exptnumsx), real(dinl%exptnumsy), real(dinl%delta) /)

! use the memory class for most array allocations
mem = Memory_T()

call Message%printMessage(' Completed reading DI data from file '//trim(DIfile))

! make sure that the patternfile actually exists 
f_exists = .FALSE.
fname = trim(EMsoft%generateFilePath('EMdatapathname'))//trim(enl%patternfile)
inquire(file=trim(fname), exist=f_exists)

if (.not.f_exists) then 
  call Message%printError('HREBSD_DIC_',' pattern file not found ')
end if

! define some pattern dimension parameters
binx = dinl%exptnumsx
biny = dinl%exptnumsy
L = binx * biny
recordsize = 4 * L
patsz = L
PCx = real(binx,wp)/2.0_wp
PCy = real(biny,wp)/2.0_wp

write (*,*) ' pattern dimensions : ', binx, biny, L, self%ppEBSDnml%ROI 

! initialize the timing routines
timer = Timing_T()
tstrb = timer%getTimeString()

! file exists to the first thing we do is extract the reference pattern; this is a simple
! binary file generated by the EMppEBSD program
call mem%alloc(exptpattern, (/ binx,biny /), 'exptpattern', 0.0 )
open(unit=dataunit,file=trim(fname),&
     status='unknown',form='unformatted',access='direct',recl=recordsize,iostat=ierr)
if (sum(self%ppEBSDnml%ROI).ne.0) then 
  offset = enl%paty * self%ppEBSDnml%ROI(3) + enl%patx + 1
  numpats = (self%ppEBSDnml%ROI(3)-self%ppEBSDnml%ROI(1)+1) * (self%ppEBSDnml%ROI(4)-self%ppEBSDnml%ROI(2)+1)
else
  offset = enl%paty * dinl%ipf_wd + enl%patx + 1
  numpats = dinl%ipf_wd * dinl%ipf_ht
end if
write (*,*) ' reference pattern offset = ', offset, recordsize
! offset = 1
! numpats = 1000
read(dataunit,rec=offset) exptpattern
close(dataunit,status='keep')

write (*,*) ' reference entries : ', exptpattern(1:2,1:2)
write (*,*) 'reference pattern : ', minval(exptpattern), maxval(exptpattern)

! allocate global arrays to store the output of the DIC routine.
call mem%alloc(normdp, (/ numpats /), 'normdp', initval=0.D0 ) 
call mem%alloc(homographies, (/ 8, numpats /), 'homographies', initval=0.D0 ) 
call mem%alloc(residuals, (/ numpats /), 'residuals', initval=0.D0 ) 
call mem%alloc(nit, (/ numpats /), 'nit', initval=0 ) 

! open the pattern file so that each thread can read from it
open(unit=dataunit,file=trim(fname),&
     status='unknown',form='unformatted',access='direct',recl=recordsize,iostat=ierr)

call OMP_setNThreads(enl%nthreads)

memth = memory_T( nt = enl%nthreads )

! here we go parallel with OpenMP
!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(TID, DIC, nbx, nby, targetpattern, ii, jj, hg, W)&
!$OMP& PRIVATE(oldnorm, oldW, SOL, ndp, Wnew, Winv, io_int, i) 

! set the thread ID
TID = OMP_GET_THREAD_NUM()

!copy the exptpattern into the thread's DIC class
DIC = DIC_T( binx, biny )
call DIC%setverbose( enl%verbose )
call DIC%setpattern( 'r', dble(exptpattern) )

! define the border widths nbx and nby for the subregion
nbx = enl%nbx
nby = enl%nby
call DIC%defineSR( nbx, nby, PCx, PCy) ! 0.5_wp, 0.5_wp)

! generate the b-splines for the reference pattern and verify the accuracy
! also get the derivatives of the reference pattern to determine the Hessian
call DIC%getbsplines(refp=.TRUE., verify=.TRUE., grads=.TRUE.)

! zero-mean and normalize the referenceSR array
call DIC%applyZMN(doreference=.TRUE.)

! compute the Hessian matrix
call DIC%getHessian(Hessian)

! allocate the targetpattern array
call memth%alloc(targetpattern, (/ binx,biny /), 'targetpattern', TID=TID )

! in the main loop we iterate over all the experimental patterns in the input file
!$OMP DO SCHEDULE(DYNAMIC)
do ii=1, numpats
  ! write (*,*) '-----------------'
  ! write (*,*) 'starting pattern ', ii

!$OMP CRITICAL
  read(dataunit,rec=ii) targetpattern
!$OMP END CRITICAL

  call DIC%setpattern( 'd', dble(targetpattern) )
  call DIC%getbsplines(defp=.TRUE.)
! ! zero-mean and normalize the referenceSR and targetSR arrays
  call DIC%applyZMN(dotarget=.TRUE.)

! initialize the homography to zero
  hg = (/ (0.0_wp, i=1,8) /)
  W = DIC%getShapeFunction(hg)
  oldnorm = 100.0_wp
  oldW = W

! loop until max iterations or convergence
  do jj = 1, enl%maxnumit
    ! write (*,*) '-----------------'
    ! write (*,*) ' iteration # ',jj
    ! call DIC%applyHomography(hg, 0.5_wp, 0.5_wp, dotarget=.TRUE.)
    call DIC%applyHomography(hg, PCx, PCy, dotarget=.TRUE.)
    call DIC%applyZMN(dotarget=.TRUE.)
    call DIC%getresiduals(CIC)
    call DIC%solveHessian(SOL, ndp)

! convert to a shape function and take the inverse
    Wnew = DIC%getShapeFunction(reshape(SOL, (/8/)))
    Winv = matrixInvert_wp( Wnew )
    W = matmul( W, Winv )
    W = W / W(3,3)
    hg = DIC%getHomography(W)
    if (ndp.lt.enl%mindeltap) then
        EXIT
    end if
  end do

  if (mod(ii,25).eq.0) then 
    io_int(1) = ii
    io_int(2) = numpats
    call Message%WriteValue(' completed # patterns/total ',io_int,2)
  end if  
  W = matrixInvert_wp( W )
  hg = DIC%getHomography(W)
  homographies(1:8,ii) = dble(hg)
  normdp(ii) = dble(ndp)
  residuals(ii) = CIC
  nit(ii) = jj
  call DIC%cleanup()
  ! call Message%printMessage(' -------------------------- ')
end do
!$OMP END DO
call memth%dealloc(targetpattern, 'targetpattern', TID=TID)
!$OMP BARRIER
!$OMP END PARALLEL

close(dataunit,status='keep')



! ! stiffness matrix in the crystal frame
! C = 0.D0  
! C(1,1)=dble(enl%C11)
! C(2,2)=dble(enl%C11)
! C(1,2)=dble(enl%C12)
! C(2,1)=dble(enl%C12)
! C(4,4)=dble(enl%C44)
! C(5,5)=dble(enl%C44)

! ! add equivalent entries for cubic crystal
! if (enl%crystal.eq.'cub') then
!   C(3,3)=dble(enl%C11)    
!   C(1,3)=dble(enl%C12) 
!   C(2,3)=dble(enl%C12)
!   C(3,1)=dble(enl%C12)
!   C(3,2)=dble(enl%C12) 
!   C(6,6)=dble(enl%C44)
! ! or for hexagonal close packed crystal
! else if (enl%crystal.eq.'hex') then
!   C(3,3)=dble(enl%C33)    
!   C(1,3)=dble(enl%C13) 
!   C(2,3)=dble(enl%C13)
!   C(3,1)=dble(enl%C13)
!   C(3,2)=dble(enl%C13)
!   C(6,6)=dble(enl%C11-enl%C12)/2.D0  
! else
!   call Message%printError('HREBSD_','Undefined crystal type '//trim(enl%crystal))
! end if

! call Message%printMessage(' Stiffness tensor (unit:GPa) = ')

! do i = 1, 6
!   io_real(1:6) = sngl(C(i,1:6))
!   call Message%WriteValue('',io_real,6)
! end do







! prepare the HDF5 output file
call timer%makeTimeStamp()
dstr = timer%getDateString()
tstre = timer%getTimeString()

! Create a new file using the default properties.
fname = trim(EMsoft%generateFilePath('EMdatapathname'))//trim(enl%datafile)

hdferr =  HDF%createFile(fname)

! write the EMheader to the file
  datagroupname = trim(HDFnames%get_ProgramData())
  call HDF%writeEMheader(EMsoft,dstr, tstrb, tstre, progname, datagroupname)

! add the Duration field to the EMheader group
  hdferr = HDF%openGroup(HDFnames%get_EMheader())
  hdferr = HDF%openGroup(HDFnames%get_ProgramData())

dataset = SC_Duration
  call H5Lexists_f(HDF%getobjectID(),trim(dataset),g_exists, hdferr)
  tstop = 0
  if (g_exists) then
    hdferr = HDF%writeDatasetFloat(dataset, tstop, overwrite)
  else
    hdferr = HDF%writeDatasetFloat(dataset, tstop)
  end if
  call HDF%pop()
  call HDF%pop()

  ! open or create a namelist group to write all the namelist files into
groupname = SC_NMLfiles
  hdferr = HDF%createGroup(HDFnames%get_NMLfiles())

! read the text file and write the array to the file
dataset = trim(HDFnames%get_NMLfilename())
  hdferr = HDF%writeDatasetTextFile(dataset, EMsoft%nmldeffile)

! leave this group
  call HDF%pop()

! create a namelist group to write all the namelist files into
  hdferr = HDF%createGroup(HDFnames%get_NMLparameters())

  call self%writeHDFNameList(HDF, HDFnames)

! then the remainder of the data in a EMData group
  hdferr = HDF%createGroup(HDFnames%get_EMData())
  hdferr = HDF%createGroup(HDFnames%get_ProgramData())

dataset = 'homographies'
  call H5Lexists_f(HDF%getobjectID(),trim(dataset),g_exists, hdferr)
  if (g_exists) then
    hdferr = HDF%writeDatasetDoubleArray(dataset, homographies, 8, numpats, overwrite)
  else
    hdferr = HDF%writeDatasetDoubleArray(dataset, homographies, 8, numpats)
  end if

dataset = 'residuals'
  call H5Lexists_f(HDF%getobjectID(),trim(dataset),g_exists, hdferr)
  if (g_exists) then
    hdferr = HDF%writeDatasetDoubleArray(dataset, residuals, numpats, overwrite)
  else
    hdferr = HDF%writeDatasetDoubleArray(dataset, residuals, numpats)
  end if

dataset = 'ndp'
  call H5Lexists_f(HDF%getobjectID(),trim(dataset),g_exists, hdferr)
  if (g_exists) then
    hdferr = HDF%writeDatasetDoubleArray(dataset, normdp, numpats, overwrite)
  else
    hdferr = HDF%writeDatasetDoubleArray(dataset, normdp, numpats)
  end if

dataset = 'nit'
  call H5Lexists_f(HDF%getobjectID(),trim(dataset),g_exists, hdferr)
  if (g_exists) then
    hdferr = HDF%writeDatasetIntegerArray(dataset, nit, numpats, overwrite)
  else
    hdferr = HDF%writeDatasetIntegerArray(dataset, nit, numpats)
  end if

call HDF%popall()

call closeFortranHDFInterface()

end associate 

end subroutine HREBSD_DIC_


end module mod_HREBSDDIC
