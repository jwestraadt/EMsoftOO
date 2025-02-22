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

module mod_HREBSD_ICGN
  !! author: MDG (OO version)
  !! version: 1.0 
  !! date: 11/07/24
  !!
  !! class definition for the EMHREBSD_ICGN program
  !! uses the inverse compositional Gauss–Newton algorithm from ATEX

use mod_kinds
use mod_global

IMPLICIT NONE 

! namelist for the EMHREBSD_ICGN program
type, public :: HREBSDICGNNameListType
   integer(kind=irg)    :: numsx
   integer(kind=irg)    :: numsy
   integer(kind=irg)    :: ipf_wd
   integer(kind=irg)    :: ipf_ht
   integer(kind=irg)    :: patx
   integer(kind=irg)    :: paty
   integer(kind=irg)    :: N_ROI
   integer(kind=irg)    :: size_ROI
   integer(kind=irg)    :: roi_distance
   integer(kind=irg)    :: nregionsmin
   integer(kind=irg)    :: nregionsmax
   integer(kind=irg)    :: nsteps
   integer(kind=irg)    :: nthreads
   real(kind=sgl)       :: step_size
   real(kind=sgl)       :: delta
   real(kind=sgl)       :: thetac
   real(kind=sgl)       :: highpass
   real(kind=sgl)       :: lowpass
   real(kind=sgl)       :: highpasswmax
   real(kind=sgl)       :: lowpasswmax
   real(kind=sgl)       :: C11
   real(kind=sgl)       :: C12
   real(kind=sgl)       :: C44
   real(kind=sgl)       :: C13
   real(kind=sgl)       :: C33
   character(fnlen)     :: tifffile
   character(fnlen)     :: xcffile
   character(fnlen)     :: patternfile
   character(fnlen)     :: datafile
   character(fnlen)     :: masterfile
   character(fnlen)     :: exptfile
   character(fnlen)     :: inputtype
   character(fnlen)     :: HDFstrings(10)
   character(1)         :: Remap
   character(1)         :: PCrefine
   character(3)         :: crystal
end type HREBSDICGNNameListType

! other type declarations
type EBSDSEMArray
        real(kind=sgl)                  :: Step_X
        real(kind=sgl)                  :: Step_Y
        integer(kind=irg),allocatable   :: SEM_X(:)
        integer(kind=irg),allocatable   :: SEM_Y(:)
        integer(kind=irg),allocatable   :: GrainID(:)
        real(kind=sgl),allocatable      :: GrainID_X(:)
        real(kind=sgl),allocatable      :: GrainID_Y(:)
end type

! class definition
type, public :: HREBSD_ICGN_T
private 
  character(fnlen)                        :: nmldeffile = 'EMHREBSD_ICGN.nml'
  type(HREBSDICGNNameListType)            :: nml 
  type(EBSDSEMArray)                      :: SEM
  real(kind=sgl)                          :: PC(3)
  logical                                 :: preview
  type(C_PTR)                             :: planf, planb
  real(kind=dbl),allocatable              :: hpmask_shifted(:,:), lpmask_shifted(:,:)
  complex(C_DOUBLE_COMPLEX),pointer       :: inp(:,:), outp(:,:)
  type(c_ptr), allocatable                :: ip, op
  type(C_PTR)                             :: planCC, cplanCC
  complex(C_DOUBLE_COMPLEX),pointer       :: inpCC(:,:), outpCC(:,:)
  type(c_ptr), allocatable                :: ipCC, opCC

contains
private 
  procedure, pass(self) :: readNameList_
  procedure, pass(self) :: writeHDFNameList_
  procedure, pass(self) :: getNameList_
  procedure, pass(self) :: HREBSD_ICGN_
  procedure, pass(self) :: setnumsx_
  procedure, pass(self) :: getnumsx_
  procedure, pass(self) :: setnumsy_
  procedure, pass(self) :: getnumsy_
  procedure, pass(self) :: setipf_wd_
  procedure, pass(self) :: getipf_wd_
  procedure, pass(self) :: setipf_ht_
  procedure, pass(self) :: getipf_ht_
  procedure, pass(self) :: setpatx_
  procedure, pass(self) :: getpatx_
  procedure, pass(self) :: setpaty_
  procedure, pass(self) :: getpaty_
  procedure, pass(self) :: setN_ROI_
  procedure, pass(self) :: getN_ROI_
  procedure, pass(self) :: setsize_ROI_
  procedure, pass(self) :: getsize_ROI_
  procedure, pass(self) :: setroi_distance_
  procedure, pass(self) :: getroi_distance_
  procedure, pass(self) :: setnthreads_
  procedure, pass(self) :: getnthreads_
  procedure, pass(self) :: setstep_size_
  procedure, pass(self) :: getstep_size_
  procedure, pass(self) :: setdelta_
  procedure, pass(self) :: getdelta_
  procedure, pass(self) :: setthetac_
  procedure, pass(self) :: getthetac_
  procedure, pass(self) :: sethighpass_
  procedure, pass(self) :: gethighpass_
  procedure, pass(self) :: setlowpass_
  procedure, pass(self) :: getlowpass_
  procedure, pass(self) :: setC11_
  procedure, pass(self) :: getC11_
  procedure, pass(self) :: setC12_
  procedure, pass(self) :: getC12_
  procedure, pass(self) :: setC44_
  procedure, pass(self) :: getC44_
  procedure, pass(self) :: setC13_
  procedure, pass(self) :: getC13_
  procedure, pass(self) :: setC33_
  procedure, pass(self) :: getC33_
  procedure, pass(self) :: setpreview_
  procedure, pass(self) :: getpreview_
  procedure, pass(self) :: sethighpasswmax_
  procedure, pass(self) :: gethighpasswmax_
  procedure, pass(self) :: setlowpasswmax_
  procedure, pass(self) :: getlowpasswmax_
  procedure, pass(self) :: settifffile_
  procedure, pass(self) :: gettifffile_
  procedure, pass(self) :: setpatternfile_
  procedure, pass(self) :: getpatternfile_
  procedure, pass(self) :: setxcffile_
  procedure, pass(self) :: getxcffile_
  procedure, pass(self) :: setdatafile_
  procedure, pass(self) :: getdatafile_
  procedure, pass(self) :: setmasterfile_
  procedure, pass(self) :: getmasterfile_
  procedure, pass(self) :: setexptfile_
  procedure, pass(self) :: getexptfile_
  procedure, pass(self) :: setinputtype_
  procedure, pass(self) :: getinputtype_
  procedure, pass(self) :: setHDFstrings_
  procedure, pass(self) :: getHDFstrings_
  procedure, pass(self) :: setRemap_
  procedure, pass(self) :: getRemap_
  procedure, pass(self) :: setPCrefine_
  procedure, pass(self) :: getPCrefine_
  procedure, pass(self) :: setcrystal_
  procedure, pass(self) :: getcrystal_
  procedure, pass(self) :: initBandPassFilter_
  procedure, pass(self) :: applyBandPassFilter_
  procedure, pass(self) :: init_cross_correlation_function_
  procedure, pass(self) :: cross_correlation_function_
  procedure, pass(self) :: peak_interpolation_

  generic, public :: getNameList => getNameList_
  generic, public :: writeHDFNameList => writeHDFNameList_
  generic, public :: readNameList => readNameList_
  generic, public :: HREBSD_ICGN => HREBSD_ICGN_
  generic, public :: setnumsx => setnumsx_
  generic, public :: getnumsx => getnumsx_
  generic, public :: setnumsy => setnumsy_
  generic, public :: getnumsy => getnumsy_
  generic, public :: setipf_wd => setipf_wd_
  generic, public :: getipf_wd => getipf_wd_
  generic, public :: setipf_ht => setipf_ht_
  generic, public :: getipf_ht => getipf_ht_
  generic, public :: setpatx => setpatx_
  generic, public :: getpatx => getpatx_
  generic, public :: setpaty => setpaty_
  generic, public :: getpaty => getpaty_
  generic, public :: setN_ROI => setN_ROI_
  generic, public :: getN_ROI => getN_ROI_
  generic, public :: setsize_ROI => setsize_ROI_
  generic, public :: getsize_ROI => getsize_ROI_
  generic, public :: setroi_distance => setroi_distance_
  generic, public :: getroi_distance => getroi_distance_
  generic, public :: setnthreads => setnthreads_
  generic, public :: getnthreads => getnthreads_
  generic, public :: setstep_size => setstep_size_
  generic, public :: getstep_size => getstep_size_
  generic, public :: setdelta => setdelta_
  generic, public :: getdelta => getdelta_
  generic, public :: setthetac => setthetac_
  generic, public :: getthetac => getthetac_
  generic, public :: sethighpass => sethighpass_
  generic, public :: gethighpass => gethighpass_
  generic, public :: setlowpass => setlowpass_
  generic, public :: getlowpass => getlowpass_
  generic, public :: setC11 => setC11_
  generic, public :: getC11 => getC11_
  generic, public :: setC12 => setC12_
  generic, public :: getC12 => getC12_
  generic, public :: setC44 => setC44_
  generic, public :: getC44 => getC44_
  generic, public :: setC13 => setC13_
  generic, public :: getC13 => getC13_
  generic, public :: setC33 => setC33_
  generic, public :: getC33 => getC33_
  generic, public :: sethighpasswmax => sethighpasswmax_
  generic, public :: gethighpasswmax => gethighpasswmax_
  generic, public :: setlowpasswmax => setlowpasswmax_
  generic, public :: getlowpasswmax => getlowpasswmax_
  generic, public :: settifffile => settifffile_
  generic, public :: gettifffile => gettifffile_
  generic, public :: setpatternfile => setpatternfile_
  generic, public :: getpatternfile => getpatternfile_
  generic, public :: setxcffile => setxcffile_
  generic, public :: getxcffile => getxcffile_
  generic, public :: setpreview => setpreview_
  generic, public :: getpreview => getpreview_
  generic, public :: setdatafile => setdatafile_
  generic, public :: getdatafile => getdatafile_
  generic, public :: setmasterfile => setmasterfile_
  generic, public :: getmasterfile => getmasterfile_
  generic, public :: setexptfile => setexptfile_
  generic, public :: getexptfile => getexptfile_
  generic, public :: setinputtype => setinputtype_
  generic, public :: getinputtype => getinputtype_
  generic, public :: setHDFstrings => setHDFstrings_
  generic, public :: getHDFstrings => getHDFstrings_
  generic, public :: setRemap => setRemap_
  generic, public :: getRemap => getRemap_
  generic, public :: setPCrefine => setPCrefine_
  generic, public :: getPCrefine => getPCrefine_
  generic, public :: setcrystal => setcrystal_
  generic, public :: getcrystal => getcrystal_
  generic, public :: initBandPassFilter => initBandPassFilter_
  generic, public :: applyBandPassFilter => applyBandPassFilter_
  generic, public :: init_cross_correlation_function => init_cross_correlation_function_
  generic, public :: cross_correlation_function => cross_correlation_function_
  generic, public :: peak_interpolation => peak_interpolation_
end type HREBSD_T

! the constructor routine for this class 
interface HREBSD_T
  module procedure HREBSD_constructor
end interface HREBSD_T

contains

!--------------------------------------------------------------------------
type(HREBSD_T) function HREBSD_constructor( nmlfile, pv ) result(HREBSD)
!DEC$ ATTRIBUTES DLLEXPORT :: HREBSD_constructor
!! author: MDG 
!! version: 1.0 
!! date: 11/07/24
!!
!! constructor for the HREBSD_T Class; reads the name list for both the 
!! EMHREBSD and EMHREBSDpreview programs
 
IMPLICIT NONE

character(fnlen), INTENT(IN), OPTIONAL   :: nmlfile 
logical, INTENT(IN), OPTIONAL            :: pv

HREBSD%preview = .FALSE.

if (present(pv)) then 
  if (pv.eqv..TRUE.) then 
    HREBSD%preview = .TRUE.
  end if
end if 

call HREBSD%readNameList(nmlfile)

end function HREBSD_constructor

!--------------------------------------------------------------------------
subroutine HREBSD_destructor(self) 
!DEC$ ATTRIBUTES DLLEXPORT :: HREBSD_destructor
!! author: MDG 
!! version: 1.0 
!! date: 11/07/24
!!
!! destructor for the HREBSD_T Class
 
IMPLICIT NONE

type(HREBSD_T), INTENT(INOUT)  :: self 

call reportDestructor('HREBSD_T')

end subroutine HREBSD_destructor

!--------------------------------------------------------------------------
subroutine readNameList_(self, nmlfile, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: readNameList_
!! author: MDG 
!! version: 1.0 
!! date: 11/07/24
!!
!! read the namelist from an nml file for the HREBSD_T Class 

use mod_io 
use mod_EMsoft

IMPLICIT NONE 

class(HREBSD_T), INTENT(INOUT)       :: self
character(fnlen),INTENT(IN)          :: nmlfile
 !! full path to namelist file 
logical,OPTIONAL,INTENT(IN)          :: initonly
 !! fill in the default values only; do not read the file

type(EMsoft_T)                       :: EMsoft 
type(IO_T)                           :: Message       
logical                              :: skipread = .FALSE.

integer(kind=irg)                    :: numsx
integer(kind=irg)                    :: numsy
integer(kind=irg)                    :: ipf_wd
integer(kind=irg)                    :: ipf_ht
integer(kind=irg)                    :: patx
integer(kind=irg)                    :: paty
integer(kind=irg)                    :: N_ROI
integer(kind=irg)                    :: size_ROI
integer(kind=irg)                    :: roi_distance
integer(kind=irg)                    :: nthreads
integer(kind=irg)                    :: nsteps
integer(kind=irg)                    :: nregionsmin
integer(kind=irg)                    :: nregionsmax
real(kind=sgl)                       :: step_size
real(kind=sgl)                       :: delta
real(kind=sgl)                       :: thetac
real(kind=sgl)                       :: highpass
real(kind=sgl)                       :: lowpass
real(kind=sgl)                       :: highpasswmax
real(kind=sgl)                       :: lowpasswmax
real(kind=sgl)                       :: C11
real(kind=sgl)                       :: C12
real(kind=sgl)                       :: C44
real(kind=sgl)                       :: C13
real(kind=sgl)                       :: C33
character(fnlen)                     :: tifffile
character(fnlen)                     :: xcffile
character(fnlen)                     :: patternfile
character(fnlen)                     :: datafile
character(fnlen)                     :: masterfile
character(fnlen)                     :: exptfile
character(fnlen)                     :: inputtype
character(fnlen)                     :: HDFstrings(10)
character(1)                         :: Remap
character(1)                         :: PCrefine
character(3)                         :: crystal

namelist / HREBSDdata / numsx, numsy, ipf_wd, ipf_ht, patx, paty, N_ROI, size_ROI, roi_distance, &
                        nthreads, step_size, delta, thetac, highpass, lowpass, C11, C12, C44, C13, &
                        C33, datafile, masterfile, exptfile, inputtype, HDFstrings, Remap, &
                        PCrefine, crystal

namelist / HREBSDpreviewdata / numsx, numsy, ipf_wd, ipf_ht, patx, paty, size_ROI, highpasswmax, lowpasswmax, &
                        exptfile, tifffile, xcffile, patternfile, inputtype, HDFstrings

numsx = 1244
numsy = 1024
ipf_wd = 1
ipf_ht = 1
patx = 0
paty = 0
N_ROI = 21
size_ROI = 8 
roi_distance = 250
nthreads = 1
nsteps = 10
nregionsmin = 1
nregionsmax = 10
step_size = 1.0
delta = 50.0
thetac = 10.0
highpass = 0.05
lowpass = 0.3
highpasswmax = 0.05
lowpasswmax = 0.3
C11 = 276.0
C12 = 159.0
C44 = 132.0
C13 = 0.0
C33 = 0.0
tifffile = 'undefined'
xcffile = 'undefined'
patternfile = 'undefined'
masterfile = 'undefined'
exptfile = 'undefined'
inputtype = 'Binary'
HDFstrings = (/ '', '', '', '', '', '', '', '', '', '' /)
Remap = 'n'
PCrefine = 'y'
crystal = 'cub'

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
  if (self%preview.eqv..FALSE.) then 
    open(UNIT=dataunit,FILE=trim(nmlfile),DELIM='apostrophe',STATUS='old')
    read(UNIT=dataunit,NML=HREBSDdata)
    close(UNIT=dataunit,STATUS='keep')

! check for required entries
    if (trim(masterfile).eq.'undefined') then
      call Message%printError('readNameList:',' master pattern file name is undefined in '//nmlfile)
    end if

    if (trim(datafile).eq.'undefined') then
      call Message%printError('readNameList:',' output file name is undefined in '//nmlfile)
    end if
 else 
    open(UNIT=dataunit,FILE=trim(nmlfile),DELIM='apostrophe',STATUS='old')
    read(UNIT=dataunit,NML=HREBSDpreviewdata)
    close(UNIT=dataunit,STATUS='keep')

    if (trim(tifffile).eq.'undefined') then
      call Message%printError('readNameList:',' tifffile file name is undefined in '//nmlfile)
    end if

    if (trim(xcffile).eq.'undefined') then
      call Message%printError('readNameList:',' xcffile file name is undefined in '//nmlfile)
    end if
  end if 

 if (trim(exptfile).eq.'undefined') then
  call Message%printError('readNameList:',' exptfile file name is undefined in '//nmlfile)
 end if

end if

self%nml%numsx = numsx
self%nml%numsy = numsy
self%nml%ipf_wd = ipf_wd
self%nml%ipf_ht = ipf_ht
self%nml%patx = patx
self%nml%paty = paty
self%nml%N_ROI = N_ROI
self%nml%size_ROI = size_ROI
self%nml%roi_distance = roi_distance
self%nml%nthreads = nthreads
self%nml%nregionsmin = nregionsmin
self%nml%nregionsmax = nregionsmax
self%nml%nsteps = nsteps
self%nml%step_size = step_size
self%nml%delta = delta
self%nml%thetac = thetac
self%nml%highpass = highpass
self%nml%lowpass = lowpass
self%nml%highpasswmax = highpasswmax
self%nml%lowpasswmax = lowpasswmax
self%nml%C11 = C11
self%nml%C12 = C12
self%nml%C44 = C44
self%nml%C13 = C13
self%nml%C33 = C33
self%nml%tifffile = tifffile
self%nml%patternfile = patternfile
self%nml%xcffile = xcffile
self%nml%datafile = datafile
self%nml%masterfile = masterfile
self%nml%exptfile = exptfile
self%nml%inputtype = inputtype
self%nml%HDFstrings = HDFstrings
self%nml%Remap = Remap
self%nml%PCrefine = PCrefine
self%nml%crystal = crystal

end subroutine readNameList_

!--------------------------------------------------------------------------
function getNameList_(self) result(nml)
!DEC$ ATTRIBUTES DLLEXPORT :: getNameList_
!! author: MDG 
!! version: 1.0 
!! date: 11/07/24
!!
!! pass the namelist for the HREBSD_T Class to the calling program

IMPLICIT NONE 

class(HREBSD_T), INTENT(INOUT)          :: self
type(HREBSDNameListType)                :: nml

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

class(HREBSD_T), INTENT(INOUT)          :: self 
type(HDF_T), INTENT(INOUT)              :: HDF
type(HDFnames_T), INTENT(INOUT)         :: HDFnames

integer(kind=irg),parameter             :: n_int = 10, n_real = 10
integer(kind=irg)                       :: hdferr,  io_int(n_int)
real(kind=sgl)                          :: io_real(n_real)
character(20)                           :: intlist(n_int), reallist(n_real)
character(fnlen)                        :: dataset, sval(1),groupname
character(fnlen,kind=c_char)            :: line2(1),line10(10)

associate( enl => self%nml )

! create the group for this namelist
hdferr = HDF%createGroup(HDFnames%get_NMLlist())

! write all the single integers
io_int = (/ enl%numsx, enl%numsy, enl%ipf_wd, enl%ipf_ht, enl%nthreads, enl%patx, enl%paty, &
            enl%N_ROI, enl%size_ROI, enl%roi_distance /)
intlist(1) = 'numsx'
intlist(2) = 'numsy'
intlist(3) = 'ipf_wd'
intlist(4) = 'ipf_ht'
intlist(5) = 'nthreads'
intlist(6) = 'patx'
intlist(7) = 'paty'
intlist(8) = 'N_ROI'
intlist(9) = 'size_ROI'
intlist(10) = 'roi_distance'
call HDF%writeNMLintegers(io_int, intlist, n_int)

! write all the single reals
io_real = (/ enl%step_size, enl%delta, enl%thetac, enl%highpass, enl%lowpass, &
             enl%C11, enl%C12, enl%C44, enl%C13, enl%C33 /)
reallist(1) = 'step_size'
reallist(2) = 'delta'
reallist(3) = 'thetac'
reallist(4) = 'highpass'
reallist(5) = 'lowpass'
reallist(6) = 'C11'
reallist(7) = 'C12'
reallist(8) = 'C44'
reallist(9) = 'C13'
reallist(10)= 'C33'
call HDF%writeNMLreals(io_real, reallist, n_real)

! write all the strings
dataset = SC_datafile
line2(1) = trim(enl%datafile)
hdferr = HDF%writeDatasetStringArray(dataset, line2, 1)
if (hdferr.ne.0) call HDF%error_check('writeHDFNameList: unable to create datafile dataset', hdferr)

dataset = SC_masterfile
line2(1) = trim(enl%masterfile)
hdferr = HDF%writeDatasetStringArray(dataset, line2, 1)
if (hdferr.ne.0) call HDF%error_check('writeHDFNameList: unable to create masterfile dataset', hdferr)

dataset = SC_exptfile
line2(1) = trim(enl%exptfile)
hdferr = HDF%writeDatasetStringArray(dataset, line2, 1)
if (hdferr.ne.0) call HDF%error_check('writeHDFNameList: unable to create exptfile dataset', hdferr)

dataset = SC_inputtype
line2(1) = trim(enl%inputtype)
hdferr = HDF%writeDatasetStringArray(dataset, line2, 1)
if (hdferr.ne.0) call HDF%error_check('writeHDFNameList: unable to create inputtype dataset', hdferr)

dataset = 'Remap'
line2(1) = trim(enl%Remap)
hdferr = HDF%writeDatasetStringArray(dataset, line2, 1)
if (hdferr.ne.0) call HDF%error_check('writeHDFNameList: unable to create Remap dataset', hdferr)

dataset = 'PCrefine'
line2(1) = trim(enl%PCrefine)
hdferr = HDF%writeDatasetStringArray(dataset, line2, 1)
if (hdferr.ne.0) call HDF%error_check('writeHDFNameList: unable to create PCrefine dataset', hdferr)

dataset = 'crystal'
line2(1) = trim(enl%crystal)
hdferr = HDF%writeDatasetStringArray(dataset, line2, 1)
if (hdferr.ne.0) call HDF%error_check('writeHDFNameList: unable to create PCrefine crystal', hdferr)

dataset = SC_HDFstrings
line10 = enl%HDFstrings
hdferr = HDF%writeDatasetStringArray(dataset, line10, 10)
if (hdferr.ne.0) call HDF%error_check('writeHDFNameList: unable to create HDFstrings dataset', hdferr)

! pop this group off the stack
call HDF%pop()
call HDF%pop()

end associate

end subroutine writeHDFNameList_

!--------------------------------------------------------------------------
subroutine setnumsx_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setnumsx_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! set numsx in the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
integer(kind=irg), INTENT(IN)       :: inp

self%nml%numsx = inp

end subroutine setnumsx_

!--------------------------------------------------------------------------
function getnumsx_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getnumsx_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! get numsx from the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
integer(kind=irg)                   :: out

out = self%nml%numsx

end function getnumsx_

!--------------------------------------------------------------------------
subroutine setnumsy_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setnumsy_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! set numsy in the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
integer(kind=irg), INTENT(IN)       :: inp

self%nml%numsy = inp

end subroutine setnumsy_

!--------------------------------------------------------------------------
function getnumsy_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getnumsy_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! get numsy from the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
integer(kind=irg)                   :: out

out = self%nml%numsy

end function getnumsy_

!--------------------------------------------------------------------------
subroutine setipf_wd_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setipf_wd_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! set ipf_wd in the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
integer(kind=irg), INTENT(IN)       :: inp

self%nml%ipf_wd = inp

end subroutine setipf_wd_

!--------------------------------------------------------------------------
function getipf_wd_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getipf_wd_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! get ipf_wd from the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
integer(kind=irg)                   :: out

out = self%nml%ipf_wd

end function getipf_wd_

!--------------------------------------------------------------------------
subroutine setipf_ht_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setipf_ht_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! set ipf_ht in the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
integer(kind=irg), INTENT(IN)       :: inp

self%nml%ipf_ht = inp

end subroutine setipf_ht_

!--------------------------------------------------------------------------
function getipf_ht_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getipf_ht_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! get ipf_ht from the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
integer(kind=irg)                   :: out

out = self%nml%ipf_ht

end function getipf_ht_

!--------------------------------------------------------------------------
subroutine setpatx_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setpatx_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! set patx in the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
integer(kind=irg), INTENT(IN)       :: inp

self%nml%patx = inp

end subroutine setpatx_

!--------------------------------------------------------------------------
function getpatx_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getpatx_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! get patx from the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
integer(kind=irg)                   :: out

out = self%nml%patx

end function getpatx_

!--------------------------------------------------------------------------
subroutine setpaty_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setpaty_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! set paty in the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
integer(kind=irg), INTENT(IN)       :: inp

self%nml%paty = inp

end subroutine setpaty_

!--------------------------------------------------------------------------
function getpaty_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getpaty_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! get paty from the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
integer(kind=irg)                   :: out

out = self%nml%paty

end function getpaty_

!--------------------------------------------------------------------------
subroutine setN_ROI_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setN_ROI_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! set N_ROI in the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
integer(kind=irg), INTENT(IN)       :: inp

self%nml%N_ROI = inp

end subroutine setN_ROI_

!--------------------------------------------------------------------------
function getN_ROI_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getN_ROI_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! get N_ROI from the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
integer(kind=irg)                   :: out

out = self%nml%N_ROI

end function getN_ROI_

!--------------------------------------------------------------------------
subroutine setsize_ROI_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setsize_ROI_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! set size_ROI in the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
integer(kind=irg), INTENT(IN)       :: inp

self%nml%size_ROI = inp

end subroutine setsize_ROI_

!--------------------------------------------------------------------------
function getsize_ROI_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getsize_ROI_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! get size_ROI from the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
integer(kind=irg)                   :: out

out = self%nml%size_ROI

end function getsize_ROI_

!--------------------------------------------------------------------------
subroutine setroi_distance_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setroi_distance_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! set roi_distance in the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
integer(kind=irg), INTENT(IN)       :: inp

self%nml%roi_distance = inp

end subroutine setroi_distance_

!--------------------------------------------------------------------------
function getroi_distance_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getroi_distance_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! get roi_distance from the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
integer(kind=irg)                   :: out

out = self%nml%roi_distance

end function getroi_distance_

!--------------------------------------------------------------------------
subroutine setnthreads_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setnthreads_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! set nthreads in the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
integer(kind=irg), INTENT(IN)       :: inp

self%nml%nthreads = inp

end subroutine setnthreads_

!--------------------------------------------------------------------------
function getnthreads_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getnthreads_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! get nthreads from the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
integer(kind=irg)                   :: out

out = self%nml%nthreads

end function getnthreads_

!--------------------------------------------------------------------------
subroutine setstep_size_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setstep_size_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! set step_size in the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
real(kind=sgl), INTENT(IN)       :: inp

self%nml%step_size = inp

end subroutine setstep_size_

!--------------------------------------------------------------------------
function getstep_size_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getstep_size_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! get step_size from the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
real(kind=sgl)                   :: out

out = self%nml%step_size

end function getstep_size_

!--------------------------------------------------------------------------
subroutine setdelta_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setdelta_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! set delta in the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
real(kind=sgl), INTENT(IN)       :: inp

self%nml%delta = inp

end subroutine setdelta_

!--------------------------------------------------------------------------
function getdelta_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getdelta_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! get delta from the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
real(kind=sgl)                   :: out

out = self%nml%delta

end function getdelta_

!--------------------------------------------------------------------------
subroutine setthetac_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setthetac_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! set thetac in the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
real(kind=sgl), INTENT(IN)       :: inp

self%nml%thetac = inp

end subroutine setthetac_

!--------------------------------------------------------------------------
function getthetac_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getthetac_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! get thetac from the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
real(kind=sgl)                   :: out

out = self%nml%thetac

end function getthetac_

!--------------------------------------------------------------------------
subroutine sethighpass_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: sethighpass_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! set highpass in the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
real(kind=sgl), INTENT(IN)       :: inp

self%nml%highpass = inp

end subroutine sethighpass_

!--------------------------------------------------------------------------
function gethighpass_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: gethighpass_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! get highpass from the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
real(kind=sgl)                   :: out

out = self%nml%highpass

end function gethighpass_

!--------------------------------------------------------------------------
subroutine setlowpass_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setlowpass_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! set lowpass in the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
real(kind=sgl), INTENT(IN)       :: inp

self%nml%lowpass = inp

end subroutine setlowpass_

!--------------------------------------------------------------------------
function getlowpass_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getlowpass_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! get lowpass from the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
real(kind=sgl)                   :: out

out = self%nml%lowpass

end function getlowpass_

!--------------------------------------------------------------------------
subroutine setC11_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setC11_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! set C11 in the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
real(kind=sgl), INTENT(IN)       :: inp

self%nml%C11 = inp

end subroutine setC11_

!--------------------------------------------------------------------------
function getC11_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getC11_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! get C11 from the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
real(kind=sgl)                   :: out

out = self%nml%C11

end function getC11_

!--------------------------------------------------------------------------
subroutine setC12_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setC12_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! set C12 in the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
real(kind=sgl), INTENT(IN)       :: inp

self%nml%C12 = inp

end subroutine setC12_

!--------------------------------------------------------------------------
function getC12_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getC12_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! get C12 from the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
real(kind=sgl)                   :: out

out = self%nml%C12

end function getC12_

!--------------------------------------------------------------------------
subroutine setC44_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setC44_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! set C44 in the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
real(kind=sgl), INTENT(IN)       :: inp

self%nml%C44 = inp

end subroutine setC44_

!--------------------------------------------------------------------------
function getC44_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getC44_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! get C44 from the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
real(kind=sgl)                   :: out

out = self%nml%C44

end function getC44_

!--------------------------------------------------------------------------
subroutine setC13_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setC13_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! set C13 in the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
real(kind=sgl), INTENT(IN)       :: inp

self%nml%C13 = inp

end subroutine setC13_

!--------------------------------------------------------------------------
function getC13_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getC13_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! get C13 from the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
real(kind=sgl)                   :: out

out = self%nml%C13

end function getC13_

!--------------------------------------------------------------------------
subroutine setC33_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setC33_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! set C33 in the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
real(kind=sgl), INTENT(IN)       :: inp

self%nml%C33 = inp

end subroutine setC33_

!--------------------------------------------------------------------------
function getC33_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getC33_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! get C33 from the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
real(kind=sgl)                   :: out

out = self%nml%C33

end function getC33_

!--------------------------------------------------------------------------
subroutine sethighpasswmax_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: sethighpasswmax_
!! author: MDG
!! version: 1.0
!! date: 07/04/23
!!
!! set highpasswmax in the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
real(kind=sgl), INTENT(IN)       :: inp

self%nml%highpasswmax = inp

end subroutine sethighpasswmax_

!--------------------------------------------------------------------------
function gethighpasswmax_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: gethighpasswmax_
!! author: MDG
!! version: 1.0
!! date: 07/04/23
!!
!! get highpasswmax from the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
real(kind=sgl)                   :: out

out = self%nml%highpasswmax

end function gethighpasswmax_

!--------------------------------------------------------------------------
subroutine setlowpasswmax_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setlowpasswmax_
!! author: MDG
!! version: 1.0
!! date: 07/04/23
!!
!! set lowpasswmax in the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
real(kind=sgl), INTENT(IN)       :: inp

self%nml%lowpasswmax = inp

end subroutine setlowpasswmax_

!--------------------------------------------------------------------------
function getlowpasswmax_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getlowpasswmax_
!! author: MDG
!! version: 1.0
!! date: 07/04/23
!!
!! get lowpasswmax from the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
real(kind=sgl)                   :: out

out = self%nml%lowpasswmax

end function getlowpasswmax_

!--------------------------------------------------------------------------
subroutine settifffile_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: settifffile_
!! author: MDG
!! version: 1.0
!! date: 07/04/23
!!
!! set tifffile in the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
character(fnlen), INTENT(IN)       :: inp

self%nml%tifffile = trim(inp)

end subroutine settifffile_

!--------------------------------------------------------------------------
function gettifffile_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: gettifffile_
!! author: MDG
!! version: 1.0
!! date: 07/04/23
!!
!! get tifffile from the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
character(fnlen)                   :: out

out = trim(self%nml%tifffile)

end function gettifffile_

!--------------------------------------------------------------------------
subroutine setpatternfile_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setpatternfile_
!! author: MDG
!! version: 1.0
!! date: 07/04/23
!!
!! set patternfile in the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
character(fnlen), INTENT(IN)       :: inp

self%nml%patternfile = trim(inp)

end subroutine setpatternfile_

!--------------------------------------------------------------------------
function getpatternfile_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getpatternfile_
!! author: MDG
!! version: 1.0
!! date: 07/04/23
!!
!! get patternfile from the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
character(fnlen)                   :: out

out = trim(self%nml%patternfile)

end function getpatternfile_

!--------------------------------------------------------------------------
subroutine setxcffile_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setxcffile_
!! author: MDG
!! version: 1.0
!! date: 07/04/23
!!
!! set xcffile in the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
character(fnlen), INTENT(IN)       :: inp

self%nml%xcffile = trim(inp)

end subroutine setxcffile_

!--------------------------------------------------------------------------
function getxcffile_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getxcffile_
!! author: MDG
!! version: 1.0
!! date: 07/04/23
!!
!! get xcffile from the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
character(fnlen)                   :: out

out = trim(self%nml%xcffile)

end function getxcffile_

!--------------------------------------------------------------------------
subroutine setpreview_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setpreview_
!! author: MDG
!! version: 1.0
!! date: 07/04/23
!!
!! set preview in the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
logical, INTENT(IN)                :: inp

self%preview = inp

end subroutine setpreview_

!--------------------------------------------------------------------------
function getpreview_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getpreview_
!! author: MDG
!! version: 1.0
!! date: 07/04/23
!!
!! get preview from the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
logical                            :: out

out = self%preview

end function getpreview_

!--------------------------------------------------------------------------
subroutine setdatafile_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setdatafile_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! set datafile in the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
character(fnlen), INTENT(IN)       :: inp

self%nml%datafile = trim(inp)

end subroutine setdatafile_

!--------------------------------------------------------------------------
function getdatafile_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getdatafile_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! get datafile from the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
character(fnlen)                   :: out

out = trim(self%nml%datafile)

end function getdatafile_

!--------------------------------------------------------------------------
subroutine setmasterfile_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setmasterfile_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! set masterfile in the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
character(fnlen), INTENT(IN)       :: inp

self%nml%masterfile = trim(inp)

end subroutine setmasterfile_

!--------------------------------------------------------------------------
function getmasterfile_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getmasterfile_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! get masterfile from the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
character(fnlen)                   :: out

out = trim(self%nml%masterfile)

end function getmasterfile_

!--------------------------------------------------------------------------
subroutine setexptfile_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setexptfile_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! set exptfile in the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
character(fnlen), INTENT(IN)       :: inp

self%nml%exptfile = trim(inp)

end subroutine setexptfile_

!--------------------------------------------------------------------------
function getexptfile_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getexptfile_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! get exptfile from the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
character(fnlen)                   :: out

out = trim(self%nml%exptfile)

end function getexptfile_

!--------------------------------------------------------------------------
subroutine setinputtype_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setinputtype_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! set inputtype in the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
character(fnlen), INTENT(IN)       :: inp

self%nml%inputtype = trim(inp)

end subroutine setinputtype_

!--------------------------------------------------------------------------
function getinputtype_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getinputtype_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! get inputtype from the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
character(fnlen)                   :: out

out = trim(self%nml%inputtype)

end function getinputtype_

!--------------------------------------------------------------------------
subroutine setHDFstrings_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setHDFstrings_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! set HDFstrings in the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
character(fnlen), INTENT(IN)       :: inp(10)

integer(kind=irg)                  :: i

do i=1,10
  self%nml%HDFstrings(i) = trim(inp(i))
end do 

end subroutine setHDFstrings_

!--------------------------------------------------------------------------
function getHDFstrings_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getHDFstrings_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! get HDFstrings from the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
character(fnlen)                   :: out(10)

integer(kind=irg)                  :: i

do i=1,10
  out(i) = trim(self%nml%HDFstrings(i))
end do 

end function getHDFstrings_

!--------------------------------------------------------------------------
subroutine setRemap_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setRemap_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! set Remap in the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
character(1), INTENT(IN)       :: inp

self%nml%Remap = trim(inp)

end subroutine setRemap_

!--------------------------------------------------------------------------
function getRemap_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getRemap_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! get Remap from the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
character(1)                   :: out

out = trim(self%nml%Remap)

end function getRemap_

!--------------------------------------------------------------------------
subroutine setPCrefine_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setPCrefine_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! set PCrefine in the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
character(1), INTENT(IN)       :: inp

self%nml%PCrefine = trim(inp)

end subroutine setPCrefine_

!--------------------------------------------------------------------------
function getPCrefine_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getPCrefine_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! get PCrefine from the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
character(1)                   :: out

out = trim(self%nml%PCrefine)

end function getPCrefine_

!--------------------------------------------------------------------------
subroutine setcrystal_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setcrystal_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! set crystal in the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
character(3), INTENT(IN)       :: inp

self%nml%crystal = trim(inp)

end subroutine setcrystal_

!--------------------------------------------------------------------------
function getcrystal_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getcrystal_
!! author: MDG
!! version: 1.0
!! date: 11/07/24
!!
!! get crystal from the HREBSD_T class

IMPLICIT NONE

class(HREBSD_T), INTENT(INOUT)     :: self
character(3)                   :: out

out = trim(self%nml%crystal)

end function getcrystal_

!--------------------------------------------------------------------------
subroutine HREBSD_ICGN_(self, EMsoft, progname, HDFnames)
!DEC$ ATTRIBUTES DLLEXPORT :: HREBSD_ICGN_
!! author: MDG 
!! version: 1.0 
!! date: 11/07/24
!!
!! perform the computations

use mod_EMsoft
use HDF5
use mod_HDFsupport
use mod_HDFnames
use mod_quaternions
use mod_rotations 
use mod_io
! use mod_filters
use mod_vendors
use mod_HDFsupport
use HDF5
use mod_timing
use mod_memory
use mod_fftw3
use stringconstants
use ISO_C_BINDING

IMPLICIT NONE 

class(HREBSD_T), INTENT(INOUT)          :: self
type(EMsoft_T), INTENT(INOUT)           :: EMsoft
character(fnlen), INTENT(INOUT)         :: progname 
type(HDFnames_T), INTENT(INOUT)         :: HDFnames

type(Vendor_T)                          :: VT
type(QuaternionArray_T)                 :: qAR
type(IO_T)                              :: Message
type(Timing_T)                          :: timer
type(memory_T)                          :: mem
type(HDF_T)                             :: HDF
type(e_T)                               :: eu
type(o_T)                               :: om
type(q_T)                               :: qu
type(a_T)                               :: ax
type(Quaternion_T)                      :: quat

character(fnlen)                        :: inpfile, HDFstring
real(kind=dbl)                          :: stepsizes(3), fpar(3), sig, totaltilt, ave, cosang, sinang
real(kind=sgl)                          :: io_real(6), ss, tstop
real(kind=dbl),allocatable              :: PC(:,:)
integer(kind=irg)                       :: hdferr, binx, biny, L, recordsize, patsz, ROI_size, sz(2), i, j, kk, numangles, &
                                           istat, interp_grid, interp_size, info
real(kind=dbl)                          :: interp_step, std
character(11)                           :: dstr
character(15)                           :: tstrb
character(15)                           :: tstre
integer(HSIZE_T)                        :: dims2(2), dims3(3), offset3(3)
integer(kind=irg)                       :: d2(2)
real(kind=dbl)                          :: x, y, val, Ftensor(9), R_sample(3,3), Smatrix(3,3), w(3,3), mea, sdev, fROI,&
                                           R_tilt(3,3),F_sample(3,3),R_detector(3,3),strain_sample(3,3), xx(2), Vpeak,sig1,sig2, &
                                           beta_sample(3,3), C(6,6), R_x, R_y, q(2), Distance(2), RRR(9), axa(4), fit(5), mp1, mp2
real(kind=dbl),allocatable              :: window(:,:), expt(:), expt_ref(:), pattern(:,:), pattern_test(:,:), &
                                           hpvals(:), lpvals(:), sumexpt(:), pcopy_ROI(:,:), q_shift(:,:), &
                                           ref_p(:,:), pcopy_ROI_test(:,:), interp_ngrid(:), ngrid(:), &
                                           z_peak(:,:), test_p(:,:), r(:,:), strain(:,:,:), rotation(:,:,:), minf(:), &
                                           shift_data(:,:,:), Euler_Angle(:,:)
real(kind=sgl),allocatable              :: expts(:), expt_refs(:)
real(kind=dbl),allocatable              :: XCF(:,:), ref_rotated(:,:)
integer(kind=irg),allocatable           :: nrvals(:), pint(:,:), ppp(:,:), pint_test(:,:), XCFint(:,:), &
                                           roi_centre(:,:)
! the following are turned into global private parameters to facilitate fft's
! type(C_PTR)                             :: planf, planb
! real(kind=dbl),allocatable              :: hpmask_shifted(:,:), lpmask_shifted(:,:)
! complex(C_DOUBLE_COMPLEX),pointer       :: inp(:,:), outp(:,:)
! type(c_ptr), allocatable                :: ip, op
real(kind=dbl),allocatable              :: rrdata(:,:), ffdata(:,:)
real(kind=dbl),allocatable              :: rrdata_test(:,:), ffdata_test(:,:)
integer(kind=irg)                       :: max_pos(2), size_interp
character(fnlen)                        :: datafile, groupname, dataset, datagroupname, outname
logical                                 :: g_exists, overwrite = .TRUE.

associate( enl => self%nml )

! this program reads a pattern file, including all the experimental parameters' so 
! this is essentially limited to TSLHDF, EDAXH5, OxfordHDF, and BrukerHDF at the moment.  We can also 
! read EMEBSD output, which is useful for debugging and testing purposes. 

call setRotationPrecision('d')

inpfile = EMsoft%generateFilePath('EMdatapathname',trim(enl%exptfile))
fpar = (/ real(enl%numsx), real(enl%numsy), real(enl%delta) /)
HDFstring = trim(enl%HDFstrings(1))

call openFortranHDFInterface()

VT = Vendor_T( enl%inputtype ) 

mem = Memory_T()

select case (trim(enl%inputtype)) 
  case ('TSLHDF')
    call VT%getTSLmetadata(inpfile, HDFstring, stepsizes, qAR, PC, fpar, sig)

  case ('EDAXH5')
    call VT%getEDAXH5metadata(inpfile, enl%HDFstrings, stepsizes, qAR, PC, fpar, sig)
! in this file format, the actual patterns are stored in .up2 files, so we need to 
! generate the filename and change the inputtype so that the pattern reader routine 
! will use the correct format
    i=1
    do while (trim(enl%HDFstrings(i)).ne.'')
      i = i+1
    end do
    inpfile = trim(enl%HDFstrings(i))//'.up2'
    inpfile = EMsoft%generateFilePath('EMdatapathname',trim(inpfile))
    call VT%set_inputtype('TSLup2')
    call VT%set_itype(3)

  case ('OxfordHDF')
    call VT%getOxfordmetadata(inpfile, HDFstring, stepsizes, qAR, PC, fpar, sig)

  case ('BrukerHDF')
    call VT%getBrukermetadata(inpfile, HDFstring, stepsizes, qAR, PC, fpar, self%SEM%SEM_X, self%SEM%SEM_Y, sig)

  case ('EMEBSD')
    call VT%getEMsoftmetadata(inpfile, HDFstring, stepsizes, qAR, PC, fpar)
    ! for now, we set the sample tilt angle to 70°; this will need to be read from the data file
    sig = 70.D0

  case default 
    call Message%printError('HREBSD_','input format currently not supported in EMHREBSD')
end select

call Message%printMessage(' Completed reading orientation data')

sz = shape( PC )
numangles = sz(2) 

! initialize the timing routines
timer = Timing_T()
tstrb = timer%getTimeString()

binx = enl%numsx
biny = enl%numsy
L = binx * biny
recordsize = 4 * L
patsz = L

! size of region of interest
ROI_size = 2**enl%size_ROI
fROI = 1.D0/dble(ROI_size)**2
dims3 = (/ binx, biny, 1 /)

call mem%alloc(expt, (/ patsz /), 'expt')
call mem%alloc(expt_ref, (/ patsz /), 'expt_ref')
call mem%alloc(expts, (/ patsz /), 'expts')
call mem%alloc(expt_refs, (/ patsz /), 'expt_refs')

! open the file with *reference* experimental pattern
HDF = HDF_T()

call VT%set_filename(inpfile)
istat = VT%openExpPatternFile(EMsoft, enl%ipf_wd, L, recordsize, enl%HDFstrings, HDF)
if (istat.ne.0) then
    call Message%printError("HREBSD_:", "Fatal error handling experimental pattern file")
end if

! and read the pattern
offset3 = (/ 0, 0, enl%paty * enl%ipf_wd + enl%patx /)
call VT%getSingleExpPattern(enl%paty, enl%ipf_wd, patsz, L, dims3, offset3, expt_refs, enl%HDFstrings, HDF)
expt_ref = dble(expt_refs)

! use the center of the diffraction pattern to get ROI and turn it into a 2D pattern
d2 = (/ROI_size, ROI_size/)
call mem%alloc(pattern, d2, 'pattern')
call mem%alloc(pattern_test, d2, 'pattern_test')
call mem%alloc(XCF, (/ 2*d2(1)-1,2*d2(1)-1 /), 'XCF')
call mem%alloc(test_p, (/ biny, binx /), 'test_p')
call mem%alloc(ref_p, (/ biny, binx /), 'ref_p')
call mem%alloc(ref_rotated, (/ biny, binx /), 'ref_rotated')
call mem%alloc(pint, d2, 'pint')
call mem%alloc(pint_test, d2, 'pint_test')
call mem%alloc(ppp, d2, 'ppp')
call mem%alloc(pcopy_ROI, d2, 'pcopy_ROI')
call mem%alloc(XCFint, (/ 2*d2(1)+1, 2*d2(1)+1 /), 'XCFint')

call mem%alloc(strain, (/ 3,3,numangles /), 'strain')
call mem%alloc(rotation, (/ 3,3,numangles /), 'rotation') 
call mem%alloc(minf, (/ numangles /), 'minf')
call mem%alloc(shift_data, (/ 3,enl%N_ROI,numangles /), 'shift_data')

! turn the expt 1D array to 2D patterns
do kk=1,biny
  ref_p(kk, 1:binx) = expt_ref((kk-1)*binx+1:kk*binx)
end do

! image intensity normalization with standard deviation and zero mean
! reference pattern
ave=sum(ref_p)/size(ref_p)
std=sqrt(sum((ref_p-ave)**2)/size(ref_p))
ref_p=(ref_p-ave)/std

! define the interpolation grid and parameters  
interp_grid =10 

call mem%alloc(ngrid, (/ interp_grid+1 /), 'ngrid')
call mem%alloc(z_peak, (/ interp_grid+1,interp_grid+1 /), 'z_peak')
! call mem%alloc(z_peak, (/ 3, 3 /), 'z_peak', startdims = (/ -1, -1 /))
call mem%alloc(q_shift, (/ 3, enl%N_ROI /), 'q_shift') 
call mem%alloc(r, (/ 3,enl%N_ROI /), 'r')
call mem%alloc(roi_centre, (/ enl%N_ROI, 2 /), 'roi_centre') 

q_shift = 0.D0 

ngrid =  (/ ((i-interp_grid/2-1.0), i=1,(interp_grid+1))/)
interp_step= 0.01D0 
interp_size= interp_grid/interp_step+1;
interp_ngrid =  (/ (-interp_grid/2+(i-1)*interp_step, i=1,interp_size)/)

! the following arrays are declared as global to this module; this would ideally
! be changed to using the mod_fft_wrap and FFTWisdom class...

! next we need to set up the high-pass filter fftw plans
call mem%alloc(self%hpmask_shifted, (/ ROI_size, ROI_size /), 'hpmask_shifted')
call mem%alloc(self%lpmask_shifted, (/ ROI_size, ROI_size /), 'lpmask_shifted')  
call mem%alloc(rrdata, (/ ROI_size, ROI_size /), 'rrdata')
call mem%alloc(ffdata, (/ ROI_size, ROI_size /), 'ffdata')
call mem%alloc(rrdata_test, (/ ROI_size, ROI_size /), 'rrdata_test')
call mem%alloc(ffdata_test, (/ ROI_size, ROI_size /), 'ffdata_test')

! use the fftw_alloc routine to create and initialize the inp and outp arrays
self%ip = fftw_alloc_complex(int(ROI_size**2,C_SIZE_T))
call c_f_pointer(self%ip, self%inp, [ROI_size, ROI_size])

self%op = fftw_alloc_complex(int(ROI_size**2,C_SIZE_T))
call c_f_pointer(self%op, self%outp, [ROI_size, ROI_size])

self%inp = cmplx(0.D0,0D0)
self%outp = cmplx(0.D0,0.D0)
  
! initialize band pass filters (low pass and high pass)
call self%initBandPassFilter_((/ROI_size, ROI_size/), dble(enl%highpass), dble(enl%lowpass)) 

! initialize the cross correlation computation
call self%init_cross_correlation_function_((/ ROI_size, ROI_size/)) 

! allocate the Hann windowing function array
call mem%alloc(window, (/ ROI_size,ROI_size /), 'window', initval=0.D0)
! determine the values of the windowing function
call HannWindow(ROI_size, window)

! stiffness matrix in the crystal frame
C = 0.D0  
C(1,1)=dble(enl%C11)
C(2,2)=dble(enl%C11)
C(1,2)=dble(enl%C12)
C(2,1)=dble(enl%C12)
C(4,4)=dble(enl%C44)
C(5,5)=dble(enl%C44)

! add equivalent entries for cubic crystal
if (enl%crystal.eq.'cub') then
  C(3,3)=dble(enl%C11)    
  C(1,3)=dble(enl%C12) 
  C(2,3)=dble(enl%C12)
  C(3,1)=dble(enl%C12)
  C(3,2)=dble(enl%C12) 
  C(6,6)=dble(enl%C44)
! or for hexagonal close packed crystal
else if (enl%crystal.eq.'hex') then
  C(3,3)=dble(enl%C33)    
  C(1,3)=dble(enl%C13) 
  C(2,3)=dble(enl%C13)
  C(3,1)=dble(enl%C13)
  C(3,2)=dble(enl%C13)
  C(6,6)=dble(enl%C11-enl%C12)/2.D0  
else
  call Message%printError('HREBSD_','Undefined crystal structure for HREBSD')
end if

call Message%printMessage(' Stiffness tensor (unit:GPa) = ')

do i = 1, 6
  io_real(1:6) = sngl(C(i,1:6))
  call Message%WriteValue('',io_real,6)
end do

! DD should be specified in pixel units ...
PC(3,:) = PC(3,:) / 50.D0  ! this needs to become a parameter read from file !!!

! test the peak finding algorithm ... 
! use two Gaussians, one at the center of ffdata, the other shifted by (8.2500000,-5.7654321)

do i= 1, ROI_size
  x = dble(i-ROI_size/2)
  do j= 1, ROI_size
    y = dble(j-ROI_size/2)
    ffdata(i,j) = exp(-(x*x+y*y)*0.02D0)
  end do 
end do 

mp1 = 4.3243243D0 
mp2 = 2.343435D0
do i= 1, ROI_size
  x = dble(i-ROI_size/2) - mp1
  do j= 1, ROI_size
    y = dble(j-ROI_size/2) - mp2
    ffdata_test(i,j) = exp(-(x*x+y*y)*0.02D0)
  end do 
end do 

write (*,*) 'actual shifts : ' , mp1, mp2

    pattern = window*ffdata
    rrdata = dble(pattern)
    ! apply the band pass filters on the reference ROI
    ffdata = self%applyBandPassFilter_(rrdata, (/ ROI_size, ROI_size/))
    mea = sum(ffdata)*fROI
    sdev = sqrt( sum( (ffdata-mea)**2)*fROI )
    ffdata = (ffdata-mea) / sdev

    ! apply the windowing function on the test ROI
    pattern_test = window*ffdata_test
    rrdata_test = dble(pattern_test)
    ! apply the band pass filters on the test ROI
    ffdata_test = self%applyBandPassFilter_(rrdata_test, (/ ROI_size, ROI_size/))
    mea = sum(ffdata_test)*fROI
    sdev = sqrt( sum( (ffdata_test-mea)**2)*fROI )
    ffdata_test = (ffdata_test-mea) / sdev


call self%cross_correlation_function_((/ ROI_size, ROI_size/), ffdata, ffdata_test, XCF, max_pos) 
   
! now crop out a small region around the peak of xcf
z_peak=XCF(max_pos(1)-interp_grid/2:max_pos(1)+interp_grid/2,max_pos(2)-interp_grid/2:max_pos(2)+interp_grid/2)

! z_peak=XCF(max_pos(1)-1:max_pos(1)+1,max_pos(2)-1:max_pos(2)+1)

! xx(1) = max_pos(1) + (0.5D0 * (z_peak(1, 0) - z_peak(-1, 0))) / (z_peak(1, 0) + z_peak(-1, 0) - 2.D0 * z_peak(0, 0))
! xx(2) = max_pos(2) + (0.5D0 * (z_peak(0, 1) - z_peak(0, -1))) / (z_peak(0, 1) + z_peak(0, -1) - 2.D0 * z_peak(0, 0))


! do i=1,9 
!   write (*,*) z_peak(i,:)
! end do 

    ! q_shift(:,1) = - (/  max_pos(1)/2+xx(1)-ROI_size/2 , max_pos(2)/2+xx(2)-ROI_size/2, 0.D0/)

! we do interpolation on this small region
! call self%peak_interpolation_(max_pos, z_peak, 2*ROI_size-1, interp_step, interp_grid+1, ngrid, interp_size, interp_ngrid, q)

write (*,*) ' max_pos = ', max_pos 

! initalize the fitting parameter 
mp1 = dble(interp_grid+1)/2.D0
mp2 = dble(interp_grid+1)/2.D0
sig1 = 1.0D-1
sig2 = 1.0D-1 
fit = (/ XCF(max_pos(1),max_pos(2)), mp1, mp2, sig1, sig2 /)
call GaussianFit( interp_grid+1, z_peak, fit, info)

write (*,*) fit, info 
write (*,*) 'fit : ', (/256.5D0,256.5D0/) - max_pos - fit(2:3) +(/ mp1, mp2 /)

! write (*,*) 'peak_interpolation : ', (/ q(2), -q(1) /)

! write (*,*) max_pos, 256.D0 - xx 

! xx=0.D0
! do i=1,15
!   xx(1) = xx(1) +sum(dble(i)*z_peak(:,i))
!   xx(2) = xx(2) +sum(dble(i)*z_peak(i,:))
! end do
! write (*,*) ' x-mean : ', ROI_size - max_pos(1)-8.D0+xx(1)/dble(15*15)
! write (*,*) ' y-mean : ', ROI_size - max_pos(2)-8.D0+xx(2)/dble(15*15)

! loop through patterns, eventually one row at a time using the getExpPatternRow function
! do j = 1, numangles  
do j = 2, 3
! determine the coordinates of the ROIs used for cross-correlation
  call setROI(enl%numsx, enl%numsy, PC(1,j), PC(2,j), PC(3,j), enl%roi_distance, enl%N_ROI, roi_centre, r)

! read the pattern
  offset3 = (/ 0, 0, j-1 /)
  call VT%getSingleExpPattern(enl%paty, enl%ipf_wd, patsz, L, dims3, offset3, expts, enl%HDFstrings, HDF)
  expt = dble(expts)

  ! test pattern (intensity normalization)
  do kk=1,biny
    test_p(kk, 1:binx) = expt((kk-1)*binx+1:kk*binx)
  end do
  ave=sum(test_p)/size(test_p)
  std=sqrt(sum((test_p-ave)**2)/size(test_p))
  test_p=(test_p-ave)/std

  do i = 1, enl%N_ROI  ! loop through all the ROIs

    ! region of interest of reference pattern
    pcopy_ROI=ref_p(roi_centre(i,2)-roi_size/2:roi_centre(i,2)+roi_size/2-1,&
                    roi_centre(i,1)-roi_size/2:roi_centre(i,1)+roi_size/2-1)
    
    ! region of interest of test pattern
    pcopy_ROI_test=test_p(roi_centre(i,2)-ROI_size/2:roi_centre(i,2)+ROI_size/2-1,&
                          roi_centre(i,1)-ROI_size/2:roi_centre(i,1)+ROI_size/2-1)
    
    ! apply the windowing function on the reference ROI
    pattern = window*pcopy_ROI
    rrdata = dble(pattern)
    ! apply the band pass filters on the reference ROI
    ffdata = self%applyBandPassFilter_(rrdata, (/ ROI_size, ROI_size/))
    mea = sum(ffdata)*fROI
    sdev = sqrt( sum( (ffdata-mea)**2)*fROI )
    ffdata = (ffdata-mea) / sdev

    ! apply the windowing function on the test ROI
    pattern_test = window*pcopy_ROI_test
    rrdata_test = dble(pattern_test)
    ! apply the band pass filters on the test ROI
    ffdata_test = self%applyBandPassFilter_(rrdata_test, (/ ROI_size, ROI_size/))
    mea = sum(ffdata_test)*fROI
    sdev = sqrt( sum( (ffdata_test-mea)**2)*fROI )
    ffdata_test = (ffdata_test-mea) / sdev

    ! convert to single precision 
    ! pattern = (sngl(ffdata))
    ! pattern_test = (sngl(ffdata_test))
    ! compute the cross correlation function in the Fourier space
    call self%cross_correlation_function_((/ ROI_size, ROI_size/), ffdata, ffdata_test, XCF, max_pos) 
   
    ! ! now crop out a small region around the peak of xcf
    z_peak=XCF(max_pos(1)-interp_grid/2:max_pos(1)+interp_grid/2,max_pos(2)-interp_grid/2:max_pos(2)+interp_grid/2)

    ! ! we do interpolation on this small region
    call self%peak_interpolation_(max_pos, z_peak, 2*ROI_size-1, interp_step, interp_grid+1, ngrid, interp_size, interp_ngrid, q)
    
    ! z_peak(-1:1,-1:1)=XCF(max_pos(1)-1:max_pos(1)+1,max_pos(2)-1:max_pos(2)+1)
    ! Vpeak = interpolate2DMaxima(z_peak, xx)
! xx(1) = max_pos(1) + (0.5D0 * (z_peak(1, 0) - z_peak(-1, 0))) / (z_peak(1, 0) + z_peak(-1, 0) - 2.D0 * z_peak(0, 0))
! xx(2) = max_pos(2) + (0.5D0 * (z_peak(0, 1) - z_peak(0, -1))) / (z_peak(0, 1) + z_peak(0, -1) - 2.D0 * z_peak(0, 0))

! initalize the fitting parameter 
mp1 = dble(interp_grid+1)/2.D0
mp2 = dble(interp_grid+1)/2.D0
sig1 = 0.5D+1
sig2 = 0.5D+1 
fit = (/ XCF(max_pos(1),max_pos(2)), mp1, mp2, sig1, sig2 /)
call GaussianFit( interp_grid+1, z_peak, fit, info)

xx = (/256.5D0,256.5D0/) - max_pos - fit(2:3) +(/ mp1, mp2 /)
q_shift(1:2,i) =  (/ -xx(2), -xx(1) /)

    ! we can then find the shift vectors associated with the ROI with subpixel accuracy
    ! q_shift(:,i) = (/-q(1), -q(2), 0.D0/)

    ! q_shift(:,i) = (/256.D0-xx(1), 256.D0-xx(2), 0.D0/)
    ! q_shift(:,i) = - (/  max_pos(1)/2+xx(1)-ROI_size/2 , max_pos(2)/2+xx(2)-ROI_size/2, 0.D0/)

write (*,*) i, max_pos(1), max_pos(2), q_shift(1,i), q_shift(2,i), info

    ! pattern center refinement (geometrically corrected)
    ! reference: Britton et al, 2011, Ultramicroscopy
    if (enl%PCrefine.eq.'y') then
      ! shift of the beam position on the sample
      Distance(1) = (0.000001D0*enl%step_size)*(mod(j,enl%ipf_wd)-(enl%patx+1)) ! step size in unit of micron
      Distance(2) = (0.000001D0*enl%step_size)*((1+j/enl%ipf_wd)-(enl%paty+1))  ! step size in unit of micron
      ! ROI positions
      R_x = roi_centre(i,1)
      R_y = enl%numsy-roi_centre(i,2)
      ! diffraction pattern shift
      cosang = cos(sig*dtor) 
      sinang = sin(sig*dtor) 
      q_shift(2,i) = q_shift(2,i)-Distance(2)/(0.000001D0*enl%delta)*(sinang-(enl%numsy/2-R_y)*cosang/PC(3,j))
      q_shift(1,i) = q_shift(1,i)-1/(0.000001D0*enl%delta)*(-Distance(1)+(enl%numsx/2-R_x)*Distance(2)*cosang/PC(3,j))    
    end if
  end do

  ! rotation matrix to sample frame
  totaltilt = 90.D0 - sig + dble(enl%thetac) 
  eu = e_T( edinp = (/0.D0, -totaltilt*dtor, 0.D0/) )
  om = eu%eo()
  R_tilt = om%o_copyd()

  write (*,*) 'R_tilt : ', R_tilt

  ! optimization routine
  quat = qAR%getQuatfromArray(j)
  qu = q_T( qdinp = quat%get_quatd() )
  RRR = reshape(R_tilt,(/9/))
  ! call main_minf(enl%N_ROI, r, q_shift, qu, C, Ftensor, minf(j), RRR)

! try this with a bit of linear algebra (QR decomposition) instead
  Ftensor = QRattempt(enl%N_ROI, r, q_shift, qu, C, RRR)

  ! Ftensor = Villert(enl%N_ROI, r, q_shift, qu, C, RRR)

  ! polar decomposition of the deformation tensor
  call getPolarDecomposition(reshape(Ftensor,(/3,3/)), R_detector, Smatrix)

write (*,*) 'Detector reference frame:'
write (*,*) 'R_detector:'
do i=1,3
  write (*,*) R_detector(i,:)
end do 
write (*,*) 'U_sample:'
do i=1,3
  write (*,*) Smatrix(i,:)
end do 
strain_sample = 0.5*(transpose(reshape(Ftensor,(/3,3/)))*reshape(Ftensor,(/3,3/))-&
                    reshape((/1.D0,0.D0,0.D0,0.D0,1.D0,0.D0,0.D0,0.D0,1.D0/),(/3,3/)))
write (*,*) 'Finite strain:'
do i=1,3
  write (*,*) strain_sample(i,:)
end do 

! write (*,*) 'R_detector:', R_detector 
  om = o_T( odinp = reshape(R_detector, (/ 3, 3 /)))
  eu = om%oe()
  ! write (*,*) ' Euler : ', eu%e_copyd()/dtor 
  ax = om%oa()
  axa = ax%a_copyd()
  write (*,*) ' Axis-Angle : ', axa(1:3), axa(4)/dtor 

  ! deformation tensor in to sample frame
  F_sample= matmul(matmul(R_tilt, reshape(Ftensor,(/3,3/))), transpose(R_tilt))

  ! polar decomposition of the deformation tensor
  call getPolarDecomposition(F_sample, R_sample, Smatrix)

write (*,*) 'Sample reference frame:'
write (*,*) 'R_sample:'
do i=1,3
  write (*,*) R_sample(i,:)
end do 
write (*,*) 'U_sample:'
do i=1,3
  write (*,*) Smatrix(i,:)
end do 
strain_sample = 0.5*(transpose(F_sample)*F_sample-reshape((/1.D0,0.D0,0.D0,0.D0,1.D0,0.D0,0.D0,0.D0,1.D0/),(/3,3/)))
write (*,*) 'Finite strain:'
do i=1,3
  write (*,*) strain_sample(i,:)
end do 



  ! lattice rotation matrix in the sample frame
  w = 0.D0
  call Rot2LatRot(R_sample, w)
  om = o_T( odinp = reshape(R_sample, (/ 3, 3 /)))
  eu = om%oe()
  ! write (*,*) ' Euler : ', eu%e_copyd()/dtor 
  ax = om%oa()
  axa = ax%a_copyd()
  write (*,*) ' Axis-Angle : ', axa(1:3), axa(4)/dtor 

  write (*,*) 'Rot2LatRot : ', w

  ! distortion tensor
  beta_sample = F_sample-reshape((/1.D0,0.D0,0.D0,0.D0,1.D0,0.D0,0.D0,0.D0,1.D0/),(/3,3/))
  strain_sample = 0.5*(transpose(beta_sample)+beta_sample)
  ! strain_sample = 0.5*(F_sample*transpose(F_sample)-reshape((/1.D0,0.D0,0.D0,0.D0,1.D0,0.D0,0.D0,0.D0,1.D0/),(/3,3/)))

  ! populate the data matrix
  strain(:,:,j) = strain_sample
  rotation(:,:,j) = w
  shift_data(:,:,j) = q_shift
  
  ! print rotation and strain tensor 
  write(*,*) Ftensor
  write(*,*) 'Lattice Rotation Matrix (w) = '
  do i = 1, ubound(w, 1)
    write(*,*) w(i, :)
  end do
  write(*,*)
  write(*,*) 'Strain Tensor (e) = '
  do i = 1, ubound(strain_sample, 1)
    write(*,*) strain_sample(i, :)
  end do
  
  ! if (enl%Remap.eq.'y') then
  !   call fRemapbicubic(binx, biny, R_detector, real(enl%PC,8), real(ref_p,8), ref_rotated)
  ! end if
 
end do

! and close the pattern file
call VT%closeExpPatternFile(HDF)

call timer%makeTimeStamp()
dstr = timer%getDateString()
tstre = timer%getTimeString()

! Create a new file using the default properties.
datafile = trim(EMsoft%generateFilePath('EMdatapathname'))//trim(enl%datafile)

hdferr =  HDF%createFile(datafile)

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

dataset = 'EulerAngles'
  call H5Lexists_f(HDF%getobjectID(),trim(dataset),g_exists, hdferr)
  call mem%alloc(Euler_Angle, (/ 3, numangles /), 'Euler_Angle')
  do i=1,numangles
    quat = qAR%getQuatfromArray(i)
    qu = q_T( qdinp = quat%get_quatd() )
    eu = qu%qe()
    Euler_Angle(:,i) = eu%e_copyd()/dtor
  end do
  if (g_exists) then
    hdferr = HDF%writeDatasetDoubleArray(dataset, Euler_Angle, 3, numangles, overwrite)
  else
    hdferr = HDF%writeDatasetDoubleArray(dataset, Euler_Angle, 3, numangles)
  end if
  call mem%dealloc(Euler_Angle, 'Euler_Angle')

dataset = 'Shift'
  call H5Lexists_f(HDF%getobjectID(),trim(dataset),g_exists, hdferr)
  if (g_exists) then
    hdferr = HDF%writeDatasetDoubleArray(dataset, shift_data, 3, enl%N_ROI, numangles, overwrite)
  else
    hdferr = HDF%writeDatasetDoubleArray(dataset, shift_data, 3, enl%N_ROI, numangles)
  end if

dataset = 'Strain'
  call H5Lexists_f(HDF%getobjectID(),trim(dataset),g_exists, hdferr)
  if (g_exists) then
    hdferr = HDF%writeDatasetDoubleArray(dataset, strain, 3, 3, numangles, overwrite)
  else
    hdferr = HDF%writeDatasetDoubleArray(dataset, strain, 3, 3, numangles)
  end if

dataset = 'Rotation'
  call H5Lexists_f(HDF%getobjectID(),trim(dataset),g_exists, hdferr)
  if (g_exists) then
    hdferr = HDF%writeDatasetDoubleArray(dataset, rotation, 3, 3, numangles, overwrite)
  else
    hdferr = HDF%writeDatasetDoubleArray(dataset, rotation, 3, 3, numangles)
  end if

dataset = SC_numangles
  call H5Lexists_f(HDF%getobjectID(),trim(dataset),g_exists, hdferr)
  if (g_exists) then
    hdferr = HDF%writeDatasetInteger(dataset, numangles, overwrite)
  else
    hdferr = HDF%writeDatasetInteger(dataset, numangles)
  end if

dataset = 'PC'
  call H5Lexists_f(HDF%getobjectID(),trim(dataset),g_exists, hdferr)
  if (g_exists) then
    hdferr = HDF%writeDatasetDoubleArray(dataset, PC, 3, numangles, overwrite)
  else
    hdferr = HDF%writeDatasetDoubleArray(dataset, PC, 3, numangles)
  end if

dataset = 'minf'
  call H5Lexists_f(HDF%getobjectID(),trim(dataset),g_exists, hdferr)
  if (g_exists) then
    hdferr = HDF%writeDatasetDoubleArray(dataset, minf, numangles, overwrite)
  else
    hdferr = HDF%writeDatasetDoubleArray(dataset, minf, numangles)
  end if

dataset = 'PixelSize'
  call H5Lexists_f(HDF%getobjectID(),trim(dataset),g_exists, hdferr)
  if (g_exists) then
    hdferr = HDF%writeDatasetFloat(dataset, enl%delta, overwrite)
  else
    hdferr = HDF%writeDatasetFloat(dataset, enl%delta)
  end if

dataset = 'RefPosition_X'
  call H5Lexists_f(HDF%getobjectID(),trim(dataset),g_exists, hdferr)
  if (g_exists) then
    hdferr = HDF%writeDatasetInteger(dataset, enl%patx, overwrite)
  else
    hdferr = HDF%writeDatasetInteger(dataset, enl%patx)
  end if

dataset = 'RefPosition_Y'
  call H5Lexists_f(HDF%getobjectID(),trim(dataset),g_exists, hdferr)
  if (g_exists) then
    hdferr = HDF%writeDatasetInteger(dataset, enl%paty, overwrite)
  else
    hdferr = HDF%writeDatasetInteger(dataset, enl%paty)
  end if

call HDF%popall()

call closeFortranHDFInterface()

end associate 

! call mem%allocated_memory_use(' before dealloc')
call mem%dealloc(expt, 'expt')
call mem%dealloc(expt_ref,'expt_ref')
call mem%dealloc(expts, 'expts')
call mem%dealloc(expt_refs, 'expt_refs')
call mem%dealloc(pattern, 'pattern')
call mem%dealloc(pattern_test, 'pattern_test')
call mem%dealloc(XCF, 'XCF')
call mem%dealloc(test_p, 'test_p')
call mem%dealloc(ref_p, 'ref_p')
call mem%dealloc(ref_rotated, 'ref_rotated')
call mem%dealloc(pint, 'pint')
call mem%dealloc(pint_test, 'pint_test')
call mem%dealloc(ppp, 'ppp')
call mem%dealloc(pcopy_ROI, 'pcopy_ROI')
call mem%dealloc(XCFint, 'XCFint')

call mem%dealloc(strain, 'strain')
call mem%dealloc(rotation, 'rotation') 
call mem%dealloc(minf, 'minf')
call mem%dealloc(shift_data, 'shift_data')
call mem%dealloc(ngrid, 'ngrid')
call mem%dealloc(z_peak, 'z_peak')
call mem%dealloc(q_shift, 'q_shift') 
call mem%dealloc(r, 'r')
call mem%dealloc(roi_centre, 'roi_centre') 

call mem%dealloc(self%hpmask_shifted, 'hpmask_shifted')
call mem%dealloc(self%lpmask_shifted, 'lpmask_shifted')  
call mem%dealloc(rrdata, 'rrdata')
call mem%dealloc(ffdata, 'ffdata')
call mem%dealloc(rrdata_test, 'rrdata_test')
call mem%dealloc(ffdata_test, 'ffdata_test')
call mem%dealloc(window, 'window')
! call mem%allocated_memory_use(' after dealloc')

call fftw_free(self%ip)
call fftw_free(self%op)
call fftw_free(self%ipCC)
call fftw_free(self%opCC)
! call fftw_cleanup()

end subroutine HREBSD_ICGN_

!--------------------------------------------------------------------------
recursive subroutine init_cross_correlation_function_(self,dims) 
!DEC$ ATTRIBUTES DLLEXPORT :: init_cross_correlation_function_
!! author: MDG (OO) / Chaoyi Zhu (5.0)
!! version: 1.0
!! date: 07/02/23
!!
! this routine initializes all the necessary FFTW arrays and plans for the 
! cross correlation step

use mod_FFTW3

IMPLICIT NONE

class(HREBSD_T),INTENT(INOUT)               :: self
integer(kind=irg),intent(in)                :: dims(2)

integer(kind=irg)                           :: cdims(2), i, j

! matrix dimensions
cdims=2*dims-1

! set up the fftw
self%ipCC = fftw_alloc_complex(int(cdims(1)*cdims(2),C_SIZE_T))
call c_f_pointer(self%ipCC, self%inpCC, [cdims(1),cdims(2)])

self%opCC = fftw_alloc_complex(int(cdims(1)*cdims(2),C_SIZE_T))
call c_f_pointer(self%opCC, self%outpCC, [cdims(1),cdims(2)])

self%inpCC = cmplx(0.D0,0D0)
self%outpCC = cmplx(0.D0,0.D0)

! create plan for forward Fourier transform
self%planCC = fftw_plan_dft_2d(cdims(1),cdims(2), self%inpCC, self%outpCC, FFTW_FORWARD, FFTW_ESTIMATE)
self%cplanCC = fftw_plan_dft_2d(cdims(1),cdims(2), self%inpCC, self%outpCC, FFTW_BACKWARD, FFTW_ESTIMATE)

end subroutine init_cross_correlation_function_

!--------------------------------------------------------------------------
recursive subroutine cross_correlation_function_(self,dims,a,b,c,max_pos) 
!DEC$ ATTRIBUTES DLLEXPORT :: cross_correlation_function_
!! author: MDG (OO) / Chaoyi Zhu (5.0)
!! version: 1.0
!! date: 07/02/23
!!
! this routine assumes that a and b have the same input dimension
! INPUT
! dims: dimension of the array to be cross-correlated 
! a(dims): reference image array
! b(dims): test image  array  
! OUTPUT
! c(2*dims-1): cross-correlation function of a and b
! max_pos(2): location of the maximum value in the cross-correlation function

use mod_FFTW3

IMPLICIT NONE

class(HREBSD_T),INTENT(INOUT)               :: self
integer(kind=irg),intent(in)                :: dims(2)
real(kind=dbl),intent(in)                   :: a(dims(1),dims(2)), b(dims(1),dims(2))
real(kind=dbl),intent(out)                  :: c(2*dims(1)-1,2*dims(1)-1)
integer(kind=irg),intent(out)               :: max_pos(2)

integer(kind=irg)                           :: cdims(2), i, j
complex(C_DOUBLE_COMPLEX), allocatable      :: ffta(:,:), fftb(:,:), fftc(:,:)
real(kind=dbl), allocatable                 :: apad(:,:), bpad(:,:) 

! matrix dimensions
cdims=2*dims-1

! allocate arrays for cross-correlation in Fourier space
allocate(apad(cdims(1),cdims(2)),bpad(cdims(1),cdims(2)))
apad = 0.D0
bpad = 0.D0
apad(1:dims(1),1:dims(2)) = a
bpad(1:dims(1),1:dims(2)) = b(dims(1):1:-1,dims(2):1:-1)

do j=1,cdims(1)
    do i=1, cdims(2)
     self%inpCC(j,i) = cmplx(apad(j,i),0.D0)    
    end do
end do

allocate(ffta(cdims(1),cdims(2)),fftb(cdims(1),cdims(2)),fftc(cdims(1),cdims(2)))

! compute the Forward Fourier transform of a
call fftw_execute_dft(self%planCC, self%inpCC, self%outpCC)
ffta=self%outpCC

! compute the Forward Fourier transform of b
self%inpCC = cmplx(0.D0,0D0)
self%outpCC = cmplx(0.D0,0.D0)
do j=1,cdims(1)
    do i=1, cdims(2)
     self%inpCC(j,i) = cmplx(bpad(j,i),0.D0)    
    end do
end do
call fftw_execute_dft(self%planCC, self%inpCC, self%outpCC)
fftb=self%outpCC

! compute the inverse Fourier transform of invF(F(a)*F(b))
fftc = ffta*fftb
self%outpCC = cmplx(0.D0,0.D0)

call fftw_execute_dft(self%cplanCC, fftc, self%outpCC)
c=real(self%outpCC)/real(product(cdims))/real(product(dims))
max_pos=maxloc(c)

deallocate(ffta, fftb, fftc, apad, bpad)

end subroutine cross_correlation_function_

!--------------------------------------------------------------------------
recursive subroutine peak_interpolation_(self, max_pos, z, z_size, interp_step, interp_size, ngrid, size_interp, &
                                         interp_ngrid, q)
!DEC$ ATTRIBUTES DLLEXPORT :: peak_interpolation_
!! author: MDG (OO) / Chaoyi Zhu (5.0)
!! version: 1.0
!! date: 07/02/23

use mod_Grid_Interpolation

IMPLICIT NONE

class(HREBSD_T),INTENT(INOUT):: self
integer(kind=irg),intent(in) :: interp_size, size_interp, z_size, max_pos(2)
real(kind=dbl),intent(in)    :: ngrid(interp_size), interp_step
real(kind=dbl),intent(inout) :: interp_ngrid(size_interp), z(interp_size,interp_size)                       
real(kind=dbl),intent(out)   :: q(2)
integer(kind=irg)            :: md, ixi, iyi, ier, max_pos_interp(2), interp_half
real(kind=dbl)               :: zi(size_interp,size_interp)

do  iyi = 1, size_interp
  do  ixi = 1, size_interp
    if (ixi == 1.AND.iyi == 1) then
      md = 1
    else
      md = 2
    end if
    ! Rectangular-grid bivariate interpolation
    call rgbi3p(md, interp_size, interp_size, ngrid, ngrid, z, 1, interp_ngrid(ixi), interp_ngrid(iyi), zi(ixi,iyi), ier)
    if (ier > 0) stop
  end do
end do
! location of the maximum value on the interpolated surface
max_pos_interp = maxloc(zi)

interp_half=(size_interp+1)/2
! shift vector
q(1)=(max_pos(2)-(z_size+1)/2)+((max_pos_interp(2)-interp_half)*interp_step);
q(2)=((z_size+1)/2-max_pos(1))+((interp_half-max_pos_interp(1))*interp_step);
  
end subroutine peak_interpolation_





  recursive function interpolate2DMaxima(p, x) result(vPeak)
  !DEC$ ATTRIBUTES DLLEXPORT :: interpolate2DMaxima

! this should really be done with a 2D Gaussian fit ... 

  implicit none

    real   (kind=dbl),INTENT(IN   ) :: p(-1:1,-1:1)
    real   (kind=dbl),INTENT(INOUT) :: x(0:1)
    real   (kind=dbl)               :: vPeak

    integer(kind=irg)               :: m(1) 
    real   (kind=dbl)               :: a, b, c, y, fm1, xm1, f0, x0, fp1, xp1, pp(3), ff(3)

! first, determine the maximum locations for each of the three horizontal rows
! we use a simple parabolic fit to the three points in each row
    a = 0.5D0*(p(-1,-1)+p(1,-1)) - p(0,-1)
    b = 0.5D0*(p(1,-1)-p(-1,-1))
    c = p(0,-1)
    xm1 = -b/(2.D0*a)
    fm1 = a*xm1**2+b*xm1+c 

    a = 0.5D0*(p(-1, 0)+p(1, 0)) - p(0, 0)
    b = 0.5D0*(p(1, 0)-p(-1, 0))
    c = p(0, 0)
    x0  = -b/(2.D0*a)
    f0  = a*x0**2+b*x0+c 

    a = 0.5D0*(p(-1, 1)+p(1, 1)) - p(0, 1)
    b = 0.5D0*(p(1, 1)-p(-1, 1))
    c = p(0, 1)
    xp1 = -b/(2.D0*a)
    fp1 = a*xp1**2+b*xp1+c 

    pp = (/ xm1, x0, xp1 /)
    ff = (/ fm1, f0, fp1 /)
    m = maxloc(ff)

! then fit another parabola to these three vertical points
    a = 0.5D0*(fm1+fp1) - f0
    b = 0.5D0*(fp1-fm1)
    c = f0
    y = -b/(2.D0*a)
    Vpeak = a*y**2+b*y+c

! the max position:
    x = (/ pp(m(1)), y /)

  end function interpolate2DMaxima



!--------------------------------------------------------------------------
recursive subroutine setROI(Lx, Ly, PC_x, PC_y, DD, roi_distance, N_ROI, roi_centre, r)
!DEC$ ATTRIBUTES DLLEXPORT :: setROI
!
!> @brief set the number and location of region of interests;
!> the center of the ROI ring is the pattern center of the reference pattern
!
!> @date 07/02/23 MDG 1.0 original
!--------------------------------------------------------------------------

implicit NONE

integer(kind=irg), INTENT(IN)                   :: Lx, Ly 
real(kind=dbl), INTENT(IN)                      :: PC_x, PC_y, DD 
integer(kind=irg),INTENT(IN)                    :: roi_distance
integer(kind=irg), INTENT(IN)                   :: N_ROI
integer(kind=irg),intent(inout)                 :: roi_centre(N_ROI,2)
real(kind=dbl),intent(inout)                    :: r(3, N_ROI)

integer(kind=irg)                               :: i
real(kind=dbl)                                  :: x

do i=1,N_ROI
  if (i.lt.N_ROI) then
    x = dble(i)*2.D0*cPi/dble(N_ROI-1)
    roi_centre(i,1:2)=floor((/ dble(Lx/2)+roi_distance*cos(x), dble(Ly/2)+roi_distance*sin(x) /))
  else
    roi_centre(i,1:2)= dble((/ Lx/2, Ly/2 /))
  end if
  r(1:3, i)=(/ roi_centre(i,1)-PC_x, Ly-PC_y-roi_centre(i,2), DD /)
end do

end subroutine setROI

!--------------------------------------------------------------------------
! the main subroutine for computing the QR decomposition and subsequent solution
! using only linear algebra instead of optimizers; this is known to be sensitive 
! to outliers and such, but it might provide a good sanity check for test cases.
recursive function QRattempt(N, r, q, qu, C_c, R_tilt) result(Ftensor)

use mod_math
use mod_rotations

implicit NONE

integer(kind=irg), INTENT(in) :: N
real(kind=dbl), INTENT(INOUT) :: r(3, N), q(3, N), C_c(6,6), R_tilt(9)
type(q_T),INTENT(INOUT)       :: qu
real(kind=dbl)                :: Ftensor(9)

integer(kind=irg)             :: ires, i, j, NB, ILAENV, K, M, NN 
integer(kind=irg),parameter   :: PP = 0
real(kind=dbl)                :: gs2c(3,3), gc2s(3,3), RM(6,6), RN(6,6), RM_inv(6,6), C_s(6,6), x, DD
real(kind=dbl)                :: A(2*N+PP,9), QQ(2*N+PP,9), Asave(2*N+PP,9), B(2*N+PP),  RR(9,9), Qb(9), RRinv(9,9), Ainv(9,2*N)

! generate the A matrix that has the r coordinates in it as well as 
! the traction-free boundary condition in terms of a series of elastic 
! moduli transformed into the correct reference frame (similar to what 
! is done in the constrained optimization, but now this constraint is just 
! one of the rows of the A matrix... )
A = 0.D0 
Asave = 0.D0 
B = 0.D0 
QQ = 0.D0 
RR = 0.D0 
Qb = 0.D0
NN = 9 
M = 2*N+PP
K = NN

! write (*,*) 'DD : ', r(3,6)

! do i=1,N 
!   j = 2*i-1
!   A(j,1:9) = (/ r(1,i), 0.D0, -r(1,i)**2/r(3,i), r(2,i), 0.D0, -r(1,i)*r(2,i)/r(3,i), r(3,i), 0.D0, -r(1,i) /)
!   B(j) = q(1,i)
!   j = 2*i
!   A(j,1:9) = (/ 0.D0, r(1,i), -r(1,i)*r(2,i)/r(3,i), 0.D0, r(2,i), -r(2,i)**2/r(3,i), 0.D0, r(3,i), -r(2,i) /)
!   B(j) = q(2,i)
!   ! j = 3*i
!   ! A(j,1:9) = (/ r(1,i)*r(2,i)/r(3,i), -r(1,i)**2/r(3,i),0.D0, r(2,i)**2/r(3,i), -r(1,i)*r(2,i)/r(3,i),0.D0, r(2,i), -r(1,i),0.D0 /)
!   ! B(j) = (r(2,i)*q(1,i)-r(1,i)*q(2,i))/r(3,i)
! end do
! and the final row of A requires the elastic moduli tensor transformation 
DD = r(3,1)

do i=1,N 
  j = 2*i-1
  A(j,1:9) = (/ r(1,i), 0.D0, -r(1,i)*(r(1,i)+q(1,i))/DD, r(2,i), 0.D0, -r(2,i)*(r(1,i)+q(1,i))/DD, DD, 0.D0, -(r(1,i)+q(1,i)) /)
  B(j) = q(1,i)
  j = 2*i
  A(j,1:9) = (/ 0.D0, r(1,i), -r(1,i)*(r(2,i)+q(2,i))/DD, 0.D0, r(2,i), -r(2,i)*(r(2,i)+q(2,i))/DD, 0.D0, DD, -(r(2,i)+q(2,i)) /)
  B(j) = q(2,i)
end do

! call StiffnessRotation(qu, gs2c, gc2s, RM, RN) 
! call inv(6, 6, RM, RM_inv)

! ! Rotate stiffness tensor from crystal frame into sample frame
! ! C_s = matmul(matmul(RM_inv,C_c),RN)
! C_s = C_c
! ! A(2*N+1,:) = (/ C_s(3,1), C_s(3,6), C_s(3,5), C_s(3,6), C_s(3,2), C_s(3,4), C_s(3,5), C_s(3,4), C_s(3,3) /)
! ! A(2*N+1,:) = (/ C_s(3,1), C_s(3,6), C_s(3,5), C_s(3,6), C_s(3,2), C_s(3,4), C_s(3,5), C_s(3,4), C_s(3,3) /)

! ! A(2*N+2,:) = (/ 1.0D0, 0.0D0, 0.0D0, 0.0D0, 1.0D0, 0.0D0, 0.0D0, 0.0D0, 1.0D0 /)

Asave = A 

! call qr_decompose(2*N+PP,9,A,QQ,RR)
! Qb = matmul ( transpose(QQ), B ) 

! do i=1,K 
!   write (*,*) RR(i,:)
! end do 
! write (*,*) 'B : ', B 
! write (*,*) 'Qb: ', Qb 
! write (*,*) 'diff: ', maxval(abs(matmul(QQ,RR)-Asave))

! call inv(9,9,RR,RRinv)

! Ftensor = matmul( RRinv , Qb )

Ftensor = pseudo_inv(2*N, 9, 2*N, 2*N, 9, A, B )

! solve the equations 
! Ftensor(K) = Qb(K) / RR(K,K)
! do i=K-1,1,-1
!   x = Qb(i)
!   do j=K,i+1,-1
!     x = x - RR(i,j) * Ftensor(j)
!   end do 
!   Ftensor(i) = x / RR(i,i)
! end do 
write (*,*) 'diffB: ', matmul(Asave,Ftensor)-B

Ftensor = Ftensor + (/1.0D0, 0.0D0, 0.0D0, 0.0D0, 1.0D0, 0.0D0, 0.0D0, 0.0D0, 1.0D0/) 

write (*,*) 'F: ', Ftensor

end function QRattempt


!--------------------------------------------------------------------------
! the main subroutine for computing the QR decomposition and subsequent solution
! using only linear algebra instead of optimizers; this is known to be sensitive 
! to outliers and such, but it might provide a good sanity check for test cases.
! in this particular routine we use the Villert appendix formulation of the problem.
recursive function Villert(N, r, q, qu, C_c, R_tilt) result(Ftensor)

use mod_math
use mod_rotations

implicit NONE

integer(kind=irg), INTENT(in) :: N
real(kind=dbl), INTENT(INOUT) :: r(3, N), q(3, N), C_c(6,6), R_tilt(9)
type(q_T),INTENT(INOUT)       :: qu
real(kind=dbl)                :: Ftensor(9)

integer(kind=irg)             :: ires, i, j, ij, ik, im, in, JJ, KK, NB, ILAENV, K, M, NN 
integer(kind=irg),parameter   :: map(3,3) = reshape( (/1,2,3,4,5,6,7,8,9/),(/3,3/))
real(kind=dbl)                :: gs2c(3,3), gc2s(3,3), RM(6,6), RN(6,6), RM_inv(6,6), C_s(6,6), x, DD
real(kind=dbl)                :: A(2*N,9), QQ(2*N,9), Asave(2*N,9), B(2*N), B2(2*N), RR(9,9), Qb(9), RRinv(9,9), Ainv(9,2*N)


A = 0.D0 
Asave = 0.D0 
B = 0.D0 
QQ = 0.D0 
RR = 0.D0 
Qb = 0.D0
NN = 9 
K = NN

DD = r(3,1)

do i=1,N 
  j = 2*i-1
  A(j,1:9) = (/ r(1,i), 0.D0, -r(1,i)*(r(1,i)+q(1,i))/DD, r(2,i), 0.D0, -r(2,i)*(r(1,i)+q(1,i))/DD, DD, 0.D0, -(r(1,i)+q(1,i)) /)
  B(j) =  q(1,i)
  j = 2*i
  A(j,1:9) = (/ 0.D0, r(1,i), -r(1,i)*(r(2,i)+q(2,i))/DD, 0.D0, r(2,i), -r(2,i)*(r(2,i)+q(2,i))/DD, 0.D0, DD, -(r(2,i)+q(2,i)) /)
  B(j) =  q(2,i)
end do

write (*,*) ' max (A,B) : ', maxval(abs(A)), maxval(abs(B)) 

open (unit=10,file='arrays.csv', status='unknown', form='formatted')

do i=1,2*N 
  write(10,"(9(F14.8,','),F14.8)") A(i,:), B(i)
end do 

close(unit=10,status='keep')

! and the final row of A requires the elastic moduli tensor transformation 

! call StiffnessRotation(qu, gs2c, gc2s, RM, RN) 
! call inv(6, 6, RM, RM_inv)

! Rotate stiffness tensor from crystal frame into sample frame
! C_s = matmul(matmul(RM_inv,C_c),RN)
! C_s = C_c
! A(2*N+1,:) = (/ C_s(3,1), C_s(3,6), C_s(3,5), C_s(3,6), C_s(3,2), C_s(3,4), C_s(3,5), C_s(3,4), C_s(3,3) /)
! A(2*N+1,:) = (/ C_s(3,1), C_s(3,6), C_s(3,5), C_s(3,6), C_s(3,2), C_s(3,4), C_s(3,5), C_s(3,4), C_s(3,3) /)

! A(2*N+2,:) = (/ 1.0D0, 0.0D0, 0.0D0, 0.0D0, 1.0D0, 0.0D0, 0.0D0, 0.0D0, 1.0D0 /)

Asave = A 

! call qr_decompose(2*N,9,A,QQ,RR)
! Qb = matmul ( transpose(QQ), B ) 

! do i=1,K 
!   write (*,*) RR(i,:)
! end do 
! write (*,*) 'B : ', B 
! write (*,*) 'Qb: ', Qb 
! write (*,*) 'diff: ', maxval(abs(matmul(QQ,RR)-Asave))

! ! call inv(9,9,RR,RRinv)

! ! Ftensor = matmul( RRinv , Qb )

! ! solve the equations 
! Ftensor(K) = Qb(K) / RR(K,K)
! do i=K-1,1,-1
!   x = Qb(i)
!   do j=K,i+1,-1
!     x = x - RR(i,j) * Ftensor(j)
!   end do 
!   Ftensor(i) = x / RR(i,i)
! end do 

! Ainv = pseudo_inv(2*N, 9, 2*N, 2*N, 9, A )
! Ftensor = matmul( Ainv, B )
Ftensor = pseudo_inv(2*N, 9, 2*N, 2*N, 9, A, B )

B2 = matmul(Asave,Ftensor)
write (*,*) 'i, Qb, B'
do i=1,81
  write (*,*) i, B2(i), B(i)
end do 
! Ftensor = Ftensor + (/1.0D0, 0.0D0, 0.0D0, 0.0D0, 1.0D0, 0.0D0, 0.0D0, 0.0D0, 1.0D0/) 

write (*,*) 'F: ', Ftensor

end function Villert


  recursive function get_entry(NN,j,k,m,n,DD,r,q) result(val)

  implicit NONE

  integer(kind=irg), INTENT(in) :: NN
  real(kind=dbl), INTENT(IN)    :: r(3, NN), q(3, NN)
  integer(kind=irg), INTENT(IN) :: j, k, m, n 
  real(kind=dbl),INTENT(IN)     :: DD 
  real(kind=dbl)                :: val 

  real(kind=dbl),parameter      :: delta(3,3) = reshape( dble((/1,0,0,0,1,0,0,0,1/)),(/3,3/))

  ! val = r(n,j) * ( DD*delta(k,m) - (r(k,j) + q(k,j))*delta(3,m) )
  val = r(n,j) * ( delta(k,m) - (r(k,j) + q(k,j))*delta(3,m)/DD )

  end function get_entry



! the main subroutine for computing the bounded constrained optimization
recursive subroutine main_minf(N, r, q, qu, C_c, Ftensor, minf, R_tilt)

use mod_math
use mod_rotations

implicit NONE

real(kind=dbl), INTENT(out)   :: Ftensor(9), minf
integer(kind=irg), INTENT(in) :: N
type(q_T),INTENT(INOUT)       :: qu
real(kind=dbl), INTENT(in)    :: r(3, N), q(3, N), C_c(6,6), R_tilt(9)

real(kind=dbl)    :: lb(9), ub(9), f(6,N), x(9), tol
integer*8         :: opt
integer(kind=irg) ::  ires, i
real(kind=dbl) :: gs2c(3,3), gc2s(3,3), RM(6,6), RN(6,6), RM_inv(6,6), C_s(6,6), C(9,9)

include 'nlopt.f'

! The nlopt_opt type corresponds to integer*8. 
! (Technically, we could use any type that is big enough to hold a pointer on all platforms;
! integer*8 is big enough for pointers on both 32-bit and 64-bit machines.)
opt = 0

! Optimization algorithms that support nonlinear equality constraints
! NLOPT_LD_SLSQP: Sequential Quadratic Programming (SQP) algorithm 
! need all the partial derivatives 
! Local derivative based optimization method with nonlinear equality constraint

! The COBYLA might hang on several instances 
! Reported issue: https://github.com/stevengj/nlopt/issues/118
! NLOPT_LN_COBYLA: COBYLA (Constrained Optimization BY Linear Approximations)
! Local derivative free method with nonlinear equality constraint

! create the nlopt_opt (opt) C type object (a pointer)
! If the constructor succeeds, opt will be nonzero after nlo_create, 
! so you can check for an error by checking whether opt is zero 
! nlo_create(nlopt object, algorithm, number of variables)
call nlo_create(opt, NLOPT_LN_COBYLA, 9)

! get the default lower/upper bounds (+-infinity) 
! integer: ires (positive: sucess, negative: failed)
call nlo_get_upper_bounds(ires, opt, ub)
call nlo_get_lower_bounds(ires, opt, lb)

! set the lower/upper bounds (bounded by +-0.05)
lb = (/1.0D0, 0.0D0, 0.0D0, 0.0D0, 1.0D0, 0.0D0, 0.0D0, 0.0D0, 1.0D0/)-0.03D0
ub = (/1.0D0, 0.0D0, 0.0D0, 0.0D0, 1.0D0, 0.0D0, 0.0D0, 0.0D0, 1.0D0/)+0.03D0
call nlo_set_lower_bounds(ires, opt, lb)
call nlo_set_upper_bounds(ires, opt, ub)

! myfunc defines the objective function to be used          
f(1:3,:) = r
f(4:6,:) = q
! write(*,*) 'ROI positions = '
! do i = 1, ubound(r, 2)
!   write(*,*) r(:, i)
! end do

! write(*,*) 'refined shift vectors = '
! do i = 1, ubound(q, 2)
!   write(*,*) q(:, i)
! end do
! set objective function
call nlo_set_min_objective(ires, opt, myfunc, f)
! determine the orientation matrices and rotation matrices for 
! stiffness tensor coordinate transformation

call StiffnessRotation(qu, gs2c, gc2s, RM, RN) 
call inv(6, 6, RM, RM_inv)

! Rotate stiffness tensor from crystal frame into sample frame
C_s = matmul(matmul(RM_inv,C_c),RN)
C = 0.D0
C(1:6,1:6) = C_s
C(7,1:9) = R_tilt

! add equality constraint (partial contraction boundary condition)
call nlo_add_equality_constraint(ires, opt, myconstraint, C, 1.0D-6)

! step tolerance (|change in x|/|xtl_abs|<xtol_rel)
call nlo_set_xtol_rel(ires, opt, 1.0D-6)

! function tolerance (|change in f(x)|/|f_abs|<ftol_abs)
tol = 1.0D-6
call nlo_set_ftol_rel(ires, opt, tol)

! set maximum number of evalutions
call nlo_set_maxeval(ires, opt, 800)

! initial value for the deformation gradient tensor (unit matrix)
x = (/1.0D0, 0.0D0, 0.0D0, 0.0D0, 1.0D0, 0.0D0, 0.0D0, 0.0D0, 1.0D0/)

! initiate the optimization
call nlo_optimize(ires, opt, x, minf)
 
Ftensor = x
if (ires.lt.0) then
  write(*,*) 'nlopt failed!'
  stop 1
else

  write(*,*) 'found min at x = ', x
  write(*,*) 'min |f(x)| = ', minf
endif

call nlo_destroy(opt)

end subroutine main_minf

! my objective function
recursive subroutine myfunc(val, n, x, grad, need_gradient, f)

IMPLICIT NONE 

double precision val, x(n), grad(n), DD, f(6,81)
integer n, need_gradient

double precision, allocatable :: r1(:,:), r2(:,:), r3(:,:)
double precision, allocatable :: q1(:,:), q2(:,:), q3(:,:)
double precision, allocatable :: row1(:,:), row2(:,:), row3(:,:)
integer dims2(2)

!TODO: the number of region of interest (ROIs) default to 21
DD = f(3,1)
dims2 = shape(f)

! allocate the variables
allocate(r1(3,dims2(2)),r2(3,dims2(2)),r3(3,dims2(2)))
allocate(q1(3,dims2(2)),q2(3,dims2(2)),q3(3,dims2(2)))
allocate(row1(3,dims2(2)),row2(3,dims2(2)),row3(3,dims2(2)))

! extract the coordinates of ROIs
r1=spread(f(1,:),1,3)
r2=spread(f(2,:),1,3)
r3=spread(f(3,:),1,3)

! extract the shift vectors
q1=spread(f(4,:),1,3)
q2=spread(f(5,:),1,3)
q3=spread(f(6,:),1,3)

! used for the objective function
row1=0.D0
row1(1,:)=1.D0
row2=0.D0
row2(2,:)=1.D0 
row3=0.D0
row3(3,:)=1.D0 

! for gradient based algorithms only
if (need_gradient.ne.0) then 
    ! partial derivatives of the objective function (not used for derivative free algorithm)      
    grad(1)= 0.5*sum(norm2((DD*r1*row1)/(x(3)*r1 + x(6)*r2 + x(9)*r3),1)**2)
    grad(2)= 0.5*sum(norm2((DD*r1*row2)/(x(3)*r1 + x(6)*r2 + x(9)*r3),1)**2)
    grad(3)= 0.5*sum(norm2((DD*r1*row3)/(x(3)*r1 + x(6)*r2 + x(9)*r3) - (DD*r1*(x(1)*r1*row1 + x(2)*r1*row2 + &
      x(3)*r1*row3 + x(4)*r2*row1 + x(5)*r2*row2 + x(6)*r2*row3 + x(7)*r3*row1 + x(8)*r3*row2 + x(9)*r3*row3))/(x(3)*r1 + &
      x(6)*r2 + x(9)*r3)**2,1)**2)
    grad(4)= 0.5*sum(norm2((DD*r2*row1)/(x(3)*r1 + x(6)*r2 + x(9)*r3),1)**2)
    grad(5)= 0.5*sum(norm2((DD*r2*row2)/(x(3)*r1 + x(6)*r2 + x(9)*r3),1)**2)
    grad(6)= 0.5*sum(norm2((DD*r2*row3)/(x(3)*r1 + x(6)*r2 + x(9)*r3) - (DD*r2*(x(1)*r1*row1 + &
      x(2)*r1*row2 + x(3)*r1*row3 + x(4)*r2*row1 + x(5)*r2*row2 + x(6)*r2*row3 + &
      x(7)*r3*row1 + x(8)*r3*row2 + x(9)*r3*row3))/(x(3)*r1 + x(6)*r2 + x(9)*r3)**2,1)**2)
    grad(7)= 0.5*sum(norm2((DD*r3*row1)/(x(3)*r1 + x(6)*r2 + x(9)*r3),1)**2)
    grad(8)= 0.5*sum(norm2((DD*r3*row2)/(x(3)*r1 + x(6)*r2 + x(9)*r3),1)**2)
    grad(9)= 0.5*sum(norm2((DD*r3*row3)/(x(3)*r1 + x(6)*r2 + x(9)*r3) - (DD*r3*(x(1)*r1*row1 + &
      x(2)*r1*row2 + x(3)*r1*row3 + x(4)*r2*row1 + x(5)*r2*row2 + x(6)*r2*row3 + &
      x(7)*r3*row1 + x(8)*r3*row2 + x(9)*r3*row3))/(x(3)*r1 + x(6)*r2 + x(9)*r3)**2,1)**2)   
    print *, "objective function gradients = ", grad  
endif

! value of the objective function 
val = 0.5*sum(norm2((DD/(x(3)*r1+x(6)*r2+x(9)*r3))*(x(1)*(r1*row1)+x(4)*(r2*row1)+x(7)*(r3*row1)+&
x(2)*(r1*row2)+x(5)*(r2*row2)+x(8)*(r3*row2)+x(3)*(r1*row3)+x(6)*(r2*row3)+x(9)*(r3*row3))-&
((r1+q1)*row1+(r2+q2)*row2+(r3+q3)*row3),1)**2)

end subroutine myfunc

! my traction free boundary constraint
recursive subroutine myconstraint(val, n, x, grad, need_gradient, C)

integer(kind=irg),INTENT(IN)        :: n, need_gradient
real(kind=dbl),INTENT(OUT)          :: val, grad(n)
real(kind=dbl),INTENT(IN)           :: x(n), C(9,9)

real(kind=dbl)                      :: F_s(3,3), beta(3,3), e(3,3), C_s(6,6), e_s(6,1), s_s(6,1), R(3,3)

! extract the rotation matrix for sample tilt
R = reshape(C(7,:),(/3,3/))
! extract the stiffness matrix components
C_s = C(1:6,1:6)

if (need_gradient.ne.0) then
  ! partial derivatives of the objective function (not used for derivative free algorithm)      
  grad(1)=abs(C(3,1))
  grad(2)=abs(C(3,6))
  grad(3)=abs(C(3,5))
  grad(4)=abs(C(3,6))
  grad(5)=abs(C(3,2))
  grad(6)=abs(C(3,4))
  grad(7)=abs(C(3,5))
  grad(8)=abs(C(3,4))
  grad(9)=abs(C(3,3))
  print *, "constraints gradients = ", grad
endif
  
! partial traction boundary condition (sigma_33=0)
F_s= matmul(matmul(R, reshape(x, (/3,3/))),transpose(R))
beta= F_s-reshape((/1.0D0, 0.0D0, 0.0D0, 0.0D0, 1.0D0, 0.0D0, 0.0D0, 0.0D0, 1.0D0/),(/3,3/))
e = 0.5*(beta+transpose(beta))
e_s = reshape((/ e(1,1), e(2,2), e(3,3), 2*e(2,3), 2*e(1,3), 2*e(1,2)/), (/6,1/))
s_s = matmul(C_s,e_s)
val = s_s(3,1)

end subroutine myconstraint



recursive subroutine StiffnessRotation(qu, gs2c, gc2s, RM, RN)
  ! Reference:
  ! Salvati, E., Sui, T. and Korsunsky, A.M., 2016. 
  ! Uncertainty quantification of residual stress evaluation by the FIB–DIC 
  ! ring-core method due to elastic anisotropy effects. 
  ! International Journal of Solids and Structures, 87, pp.61-69

use mod_rotations
use mod_math

IMPLICIT NONE
type(q_T),INTENT(INOUT)     :: qu                  
real(kind=dbl), intent(out) :: gs2c(3,3), gc2s(3,3), RM(6,6), RN(6,6) 

type(o_T)                   :: om

real(kind=dbl)              :: R(3,3)

! Orientation matrix (sample to crystal) 
om = qu%qo()
gs2c = om%o_copyd()
! Orientation matrix (crystal to sample)
gc2s = transpose(gs2c)

! % T_sigma =
! %  
! % [   R11^2,   R12^2,   R13^2,         2*R12*R13,         2*R11*R13,         2*R11*R12]
! % [   R21^2,   R22^2,   R23^2,         2*R22*R23,         2*R21*R23,         2*R21*R22]
! % [   R31^2,   R32^2,   R33^2,         2*R32*R33,         2*R31*R33,         2*R31*R32]
! % [ R21*R31, R22*R32, R23*R33, R22*R33 + R23*R32, R21*R33 + R23*R31, R21*R32 + R22*R31]
! % [ R11*R31, R12*R32, R13*R33, R12*R33 + R13*R32, R11*R33 + R13*R31, R11*R32 + R12*R31]
! % [ R11*R21, R12*R22, R13*R23, R12*R23 + R13*R22, R11*R23 + R13*R21, R11*R22 + R12*R21]
R=gs2c
RM=reshape((/R(1,1)**2, R(1,2)**2, R(1,3)**2, 2*R(1,2)*R(1,3), 2*R(1,3)*R(1,1), 2*R(1,1)*R(1,2),&
             R(2,1)**2, R(2,2)**2, R(2,3)**2, 2*R(2,2)*R(2,3), 2*R(2,3)*R(2,1), 2*R(2,1)*R(2,2),&
             R(3,1)**2, R(3,2)**2, R(3,3)**2, 2*R(3,2)*R(3,3), 2*R(3,3)*R(3,1), 2*R(3,1)*R(3,2),&
             R(2,1)*R(3,1), R(2,2)*R(3,2), R(2,3)*R(3,3), R(2,2)*R(3,3)+R(2,3)*R(3,2), R(2,1)*R(3,3)+R(2,3)*R(3,1), &
             R(2,2)*R(3,1)+R(2,1)*R(3,2),R(1,1)*R(3,1), R(1,2)*R(3,2), R(1,3)*R(3,3), R(1,2)*R(3,3)+R(1,3)*R(3,2), &
             R(1,3)*R(3,1)+R(1,1)*R(3,3), R(1,1)*R(3,2)+R(1,2)*R(3,1),R(1,1)*R(2,1), R(1,2)*R(2,2), R(1,3)*R(2,3), &
             R(1,2)*R(2,3)+R(1,3)*R(2,2), R(1,3)*R(2,1)+R(1,1)*R(2,3), R(1,1)*R(2,2)+R(1,2)*R(2,1)/),(/6,6/))
RM=transpose(RM)
! % T_epsilon =
! %  
! % [     R11^2,     R12^2,     R13^2,           R12*R13,           R11*R13,           R11*R12]
! % [     R21^2,     R22^2,     R23^2,           R22*R23,           R21*R23,           R21*R22]
! % [     R31^2,     R32^2,     R33^2,           R32*R33,           R31*R33,           R31*R32]
! % [ 2*R21*R31, 2*R22*R32, 2*R23*R33, R22*R33 + R23*R32, R21*R33 + R23*R31, R21*R32 + R22*R31]
! % [ 2*R11*R31, 2*R12*R32, 2*R13*R33, R12*R33 + R13*R32, R11*R33 + R13*R31, R11*R32 + R12*R31]
! % [ 2*R11*R21, 2*R12*R22, 2*R13*R23, R12*R23 + R13*R22, R11*R23 + R13*R21, R11*R22 + R12*R21]
RN=reshape((/R(1,1)**2, R(1,2)**2, R(1,3)**2, R(1,2)*R(1,3), R(1,3)*R(1,1), R(1,1)*R(1,2),&
             R(2,1)**2, R(2,2)**2, R(2,3)**2, R(2,2)*R(2,3), R(2,3)*R(2,1), R(2,1)*R(2,2), &
             R(3,1)**2, R(3,2)**2, R(3,3)**2, R(3,2)*R(3,3), R(3,3)*R(3,1), R(3,1)*R(3,2), &
             2*R(2,1)*R(3,1), 2*R(2,2)*R(3,2), 2*R(2,3)*R(3,3), R(2,2)*R(3,3)+R(2,3)*R(3,2), &
             R(2,1)*R(3,3)+R(2,3)*R(3,1), R(2,2)*R(3,1)+R(2,1)*R(3,2), 2*R(1,1)*R(3,1), &
             2*R(1,2)*R(3,2), 2*R(1,3)*R(3,3), R(1,2)*R(3,3)+R(1,3)*R(3,2), &
             R(1,3)*R(3,1)+R(1,1)*R(3,3), R(1,1)*R(3,2)+R(1,2)*R(3,1), 2*R(1,1)*R(2,1), 2*R(1,2)*R(2,2), &
             2*R(1,3)*R(2,3), R(1,2)*R(2,3)+R(1,3)*R(2,2), R(1,3)*R(2,1)+R(1,1)*R(2,3),&
              R(1,1)*R(2,2)+R(1,2)*R(2,1)/),(/6,6/))
RN=transpose(RN)

end subroutine StiffnessRotation

recursive subroutine Rot2LatRot(R_finite, w)

use mod_rotations

implicit NONE

real(kind=dbl),intent(in)    :: R_finite(3,3)
real(kind=dbl),intent(inout) :: w(3,3)

type (v_T)                   :: v
type (o_T)                   :: om

real(kind=dbl)               :: rotation_vector(3)

! convert the rotation matrix to axis-angle pair
om = o_T( odinp = R_finite )
v = om%ov()
rotation_vector = v%v_copyd()

! population the lattice rotation matrix
w(1,2) = -rotation_vector(3)
w(1,3) = rotation_vector(2)
w(2,3) = -rotation_vector(1)
w(2,1) = -w(1,2)
w(3,1) = -w(1,3)
w(3,2) = -w(2,3)
! sample frame rotation
w = transpose(w)

end subroutine Rot2LatRot

! interpolation based remapping of diffraction pattern 
recursive subroutine fRemapbicubic(binx, biny, R, PC, image, image_rotated)

use mod_rotations
use mod_math

IMPLICIT NONE

integer(kind=irg),intent(in)    :: binx, biny
real(kind=dbl),intent(in)       :: R(3,3), PC(3), image(biny, binx)
real(kind=dbl),intent(out)       :: image_rotated(biny, binx)
real(kind=dbl)       :: yy, xx, P(4,4), rp(3,1), r_rot(3), row1, col1, &
                       dx, dy, a, b, intensity
integer(kind=irg)   :: row2, col2, x1, y1

do row2=1,biny
  do col2=1,binx
    yy = -(row2-PC(2))
    xx = col2-PC(1)

    rp=reshape((/xx,yy,PC(3)/),(/3,1/))
    r_rot = reshape(matmul((PC(3) / DOT_PRODUCT(reshape(matmul(R,rp),(/3/)),&
    (/0.D0, 0.D0, 1.D0/)))*R, rp),(/3/))
    row1 = -r_rot(2) + PC(2)
    col1 = r_rot(1) + PC(1)
    
    b = row1
    a = col1
    x1 = floor(a)
    y1 = floor(b)

    if ((x1.ge.2).and.(y1.ge.2).and.(x1.le.binx-2).and.(y1.le.biny-2)) then
        P = image(y1 -1:y1 +2, x1-1:x1+2)
        ! interpolation weights
        dx = a-x1
        dy = b-y1
        intensity = bicubicInterpolate(P, dx, dy)
    else
        intensity = 0.D0
    end if
    image_rotated(row2,col2) = intensity
  end do

end do

end subroutine fRemapbicubic


recursive function bicubicInterpolate(p, x, y) result (q)

implicit NONE
real(kind=dbl), intent(in) :: p(4,4), x, y
real(kind=dbl)  :: q, q1, q2, q3, q4

q1 = cubicInterpolate(p(1,:), x)
q2 = cubicInterpolate(p(2,:), x)
q3 = cubicInterpolate(p(3,:), x)
q4 = cubicInterpolate(p(4,:), x)
q = cubicInterpolate((/q1, q2, q3, q4/), y)

end function bicubicInterpolate

recursive function cubicInterpolate(p, x) result (q)

implicit NONE

real(kind=dbl), intent(in) :: p(4), x

real(kind=dbl)  :: q

q = p(2) + 0.5 * x*(p(3) - p(1) + x*(2.0*p(1) - 5.0*p(2) + 4.0*p(3) - p(4) + x*(3.0*(p(2) - p(3)) + p(4) - p(1))))

end function cubicInterpolate

! Returns the inverse of a matrix A(nn,mm) calculated by finding the LU
! decomposition.  Depends on LAPACK.
recursive subroutine inv(nn, mm, A, Ainv)

implicit NONE

integer(kind=irg), intent(in) :: nn, mm
real(kind=dbl), dimension(nn,mm), intent(in) :: A
real(kind=dbl), dimension(nn,mm), intent(out) :: Ainv

real(kind=dbl),allocatable  :: work(:)  ! work array for LAPACK
integer(kind=irg), dimension(nn) :: ipiv   ! pivot indices
integer(kind=irg) :: n, info, LWORK

! ! External procedures defined in LAPACK
! external DGETRF
! external DGETRI

! Store A in Ainv to prevent it from being overwritten by LAPACK
Ainv = A
n = size(A,1)

! DGETRF computes an LU factorization of a general M-by-N matrix A
! using partial pivoting with row interchanges.
call DGETRF(n, n, Ainv, n, ipiv, info)

if (info /= 0) then
   stop 'Matrix is numerically singular!'
end if

! DGETRI computes the inverse of a matrix using the LU factorization
! computed by DGETRF.
LWORK = -1
allocate( work(1) )
call DGETRI(n, Ainv, n, ipiv, work, LWORK, info)
LWORK = work(1)
deallocate (work )
allocate( work(LWORK) )
call DGETRI(n, Ainv, n, ipiv, work, LWORK, info)

if (info /= 0) then
   stop 'Matrix inversion failed!'
end if

end subroutine inv



!--------------------------------------------------------------------------
recursive subroutine HannWindow(roi_size, window)
!DEC$ ATTRIBUTES DLLEXPORT :: HannWindow
!
!> @brief Hann windowing function for pattern region of interest
!
!> @date 07/02/23 MDG 1.0 original
!--------------------------------------------------------------------------

IMPLICIT NONE

integer(kind=irg),INTENT(IN)           :: roi_size
real(kind=dbl),INTENT(INOUT)           :: window(roi_size, roi_size)

integer(kind=irg)                      :: i, j
real(kind=dbl)                         :: fr

fr = cPi / dble(roi_size)

do i=1,roi_size
  do j=1,roi_size
    window(i,j)=cos((dble(i-roi_size/2) * fr))*cos((dble(roi_size/2-j) * fr))      
  end do
end do

end subroutine HannWindow

!--------------------------------------------------------------------------
recursive subroutine initBandPassFilter_(self, dims, high_pass, low_pass) 
!DEC$ ATTRIBUTES DLLEXPORT :: initBandPassFilter_
!
!> @brief Initialize high pass filter and low pass filter with predefined cut-off frequencies
!
!> @date 07/02/23 MDG 1.0 original
!--------------------------------------------------------------------------

use mod_FFTW3

IMPLICIT NONE

class(HREBSD_T),INTENT(INOUT)           :: self
integer(kind=irg),INTENT(IN)            :: dims(2)
real(kind=dbl),INTENT(IN)               :: high_pass, low_pass

integer(kind=irg)                       :: i, j
real(kind=dbl)                          :: grid(dims(1)), hh4, hh2, hm4, ll4, ll2, lm4
real(kind=dbl)                          :: X_grid(dims(1),dims(2)), Y_grid(dims(1),dims(2)), D(dims(1),dims(2))
complex(kind=dbl)                       :: val
real(kind=dbl)                          :: hpmask(dims(1),dims(2)), lpmask(dims(1),dims(2))

hh2 = high_pass/2.D0 
hh4 = high_pass+high_pass/4.D0 
hm4 = high_pass-high_pass/4.D0 

! generate the meshgrid in the frequency space
grid(1:dims(1)) = (/ (-1.D0 + dble(i-1)*(2.D0/dble(dims(1)-1)), i=1,dims(1)) /)
X_grid = spread( grid, 1, dims(1) )
Y_grid = spread( grid, 2, dims(1) )

! distance from the zero frequency to a cut-off frequency
D = 0.5D0*sqrt(X_grid**2+Y_grid**2)

! generate the mask for high pass filter
hpmask = 0.D0
do i = 1,dims(1)
  do j = 1,dims(2)
    if (D(i,j).ge.hh4) then
      hpmask(i,j) = 1.D0
    else if ((D(i,j).ge.hm4).AND.(D(i,j).le.hh4)) then
      hpmask(i,j) = (D(i,j)-hm4)/hh2
    end if
  end do
end do

self%hpmask_shifted = 0.D0
call ifftshift(dims, hpmask, self%hpmask_shifted)

ll2 = low_pass/2.D0 
lm4 = low_pass-low_pass/4.D0 
ll4 = low_pass+low_pass/4.D0 

! generate the mask for low pass filter
lpmask = 0.D0
do i = 1,dims(1)
  do j = 1,dims(2)
    if (D(i,j).le.lm4) then
      lpmask(i,j) = 1.D0
    else if ((D(i,j).ge.lm4).AND.(D(i,j).le.ll4)) then
      lpmask(i,j) = 1.D0-(D(i,j)-lm4)/ll2
    end if
  end do
end do

self%lpmask_shifted = 0.0
call ifftshift(dims, lpmask, self%lpmask_shifted)

! then we set up the fftw plans for forward and reverse transforms
self%planf = fftw_plan_dft_2d(dims(2), dims(1), self%inp, self%outp, FFTW_FORWARD, FFTW_ESTIMATE)
self%planb = fftw_plan_dft_2d(dims(2), dims(1), self%inp, self%outp, FFTW_BACKWARD, FFTW_ESTIMATE)

end subroutine initBandPassFilter_

!--------------------------------------------------------------------------
recursive subroutine ifftshift(dims, X, Y)
!DEC$ ATTRIBUTES DLLEXPORT :: ifftshift
!
!> @brief shift the array by the center vector
!
!> @date 07/02/23 MDG 1.0 original
!--------------------------------------------------------------------------

IMPLICIT NONE

integer(kind=irg),intent(in)                    :: dims(2)
real(kind=dbl),intent(in)                       :: X(dims(1),dims(2))
real(kind=dbl),intent(out)                      :: Y(dims(1),dims(2))

! shift the quadrants
if (mod(dims(1),2).eq.0) then
  Y(dims(1)/2+1:dims(1),1:dims(2)/2)=X(1:dims(1)/2,dims(2)/2+1:dims(2))
  Y(1:dims(1)/2,dims(2)/2+1:dims(2))=X(dims(1)/2+1:dims(1),1:dims(2)/2)
  Y(1:dims(1)/2,1:dims(2)/2)=X(dims(1)/2+1:dims(1),dims(2)/2+1:dims(2))
  Y(dims(1)/2+1:dims(1),dims(2)/2+1:dims(2))=X(1:dims(1)/2,1:dims(2)/2)
else
  Y(dims(1)/2+2:dims(1),1:dims(2)/2)=X(1:dims(1)/2,dims(2)/2+2:dims(2))
  Y(1:dims(1)/2,dims(2)/2+2:dims(2))=X(dims(1)/2+2:dims(1),1:dims(2)/2)
  Y(1:dims(1)/2,1:dims(2)/2)=X(dims(1)/2+1:dims(1),dims(2)/2+1:dims(2))
  Y(dims(1)/2+2:dims(1),dims(2)/2+2:dims(2))=X(1:dims(1)/2,1:dims(2)/2)
endif

end subroutine ifftshift


!--------------------------------------------------------------------------
recursive function applyBandPassFilter_(self, rdata, dims) result(fdata)
!DEC$ ATTRIBUTES DLLEXPORT :: applyBandPassFilter_
!
!> @brief Apply high pass filter and low pass filter with predefined cut-off frequencies
!
!> @date 07/03/23 MDG 1.0 original
!--------------------------------------------------------------------------

use mod_FFTW3

IMPLICIT NONE

class(HREBSD_T),INTENT(INOUT)           :: self
integer(kind=irg),INTENT(IN)            :: dims(2)
real(kind=dbl),INTENT(IN)               :: rdata(dims(1),dims(2))

real(kind=dbl)                          :: fdata(dims(1),dims(2))
integer(kind=irg)                       :: j, k
complex(kind=dbl)                       :: hpmask_complex(dims(1),dims(2)), lpmask_complex(dims(1),dims(2))

! apply the hi-pass mask to rdata
do j=1,dims(1)
 do k=1,dims(2)
  self%inp(j,k) = cmplx(rdata(j,k),0.D0)    
  hpmask_complex(j,k) = cmplx(self%hpmask_shifted(j,k),0.D0)    
  lpmask_complex(j,k) = cmplx(self%hpmask_shifted(j,k),0.D0) 
 end do
end do

call fftw_execute_dft(self%planf, self%inp, self%outp)
self%inp = self%outp * hpmask_complex * lpmask_complex

call fftw_execute_dft(self%planb, self%inp, self%outp) 
fdata(1:dims(1),1:dims(2)) = real(self%outp)

end function applyBandPassFilter_

_

subroutine qr_decompose(m,n,a,q,r)
!**********************************************************************************
!This subroutine performs qr decomposition using gram schmidt orthogonalisation process
![input]~   m,n is the dimension of [m * n] input ' a' matrix
!     a is the input matrix
![output]~  q is the orthogonal matrix
!     r is the upper triangular matrix
!**********************************************************************************

implicit none

integer(kind=irg), INTENT(IN)     :: m,n
real(kind=dbl), INTENT(IN)        :: a(m,n)    ! a is the main [m by n] matrix
real(kind=dbl), INTENT(OUT)       :: q(m,n)    ! q is the orthogonal [m by n] matrix
real(kind=dbl), INTENT(OUT)       :: r(n,n)    ! r is the upper triangular [n by n] matrix

integer(kind=irg)                 :: k, i, i1, j1, j
real(kind=dbl)                    :: u(m,n), e(m,n), b(m,n)
 
!-------------------------put zero for all matrix (except 'a')-------------------------
e = 0.D0
u = 0.D0
b = 0.D0
q = 0.D0
r = 0.D0

! this is the modified QR decomposition (numerically more stable)

!-------------------------first calculate e(:,1) manually-------------------------
u = a
do j = 1,n
  r(j,j) = norm2(u(:,j))
  q(:,j) = u(:,j)/r(j,j)
  do k = j+1,n
    r(j,k) = dot_product(q(:,j),u(:,k))
    u(:,k) = u(:,k) - r(j,k) * q(:,j)
  end do
end do


! !-------------------------first calculate e(:,1) manually-------------------------
! u(:,1) = a(:,1)                 ! copy the first column of 'a' to first column of 'u'
! e(:,1) = u(:,1)/norm2(u(:,1))   ! normalise u(:,1)

! !-------------------------iterate for the other e's-------------------------
! do k = 1,n-1
!   b = 0.D0                       ! put b=0.0 each time
!   do i = 1,k
!     b(:,i) = (dot_product(a(:,k+1),e(:,i)))*e(:,i)  ! calculate (a.u)*e and store it in 'b' columnwise  
!   end do

!   u(:,k+1) = a(:,k+1)-sum(b, dim=2)     ! find u
!   e(:,k+1) = u(:,k+1)/norm2(u(:,k+1))   ! normalise u
! end do

! !-------------------------transfer e to q matrix-------------------------
! q = e

! !-------------------------find the r matrix-------------------------    
! do i1 = 1, n 
!   do j1 = i1, n 
!     r(i1,j1) = dot_product(a(:,j1),e(:,i1))
!   end do
! end do

end subroutine qr_decompose

recursive function pseudo_inv(m, n, LDA, LDU, LDVT, A, B) result(Api)

IMPLICIT NONE

integer(kind=irg),INTENT(IN):: m, n, LDA, LDU, LDVT
real(kind=dbl),INTENT(IN)   :: A(m,n), B(m)
real(kind=dbl)              :: Api(n)

integer(kind=irg)           :: MM, NN, LWORK, INFO, LWMAX=1000, i 
real(kind=dbl)              :: S(n), U(LDU,m), VT(LDVT,n), Sinv(n,m), Asave(m,n), &
                               u1(LDU,n-1), v1(n,n-1), s1(n-1,n-1), s1_inv(n-1,n-1), tmp(n,n), & 
                               a1(n-1,n), b1(n-1), x1(n), v0(n)
real(kind=dbl),allocatable  :: WORK(:)
character(1)                :: JOBU, JOBV

Asave = A
LWORK = -1
allocate( WORK(LWMAX) )
CALL DGESVD( 'All', 'All', M, N, Asave, LDA, S, U, LDU, VT, LDVT, WORK, LWORK, INFO )
LWORK = MIN( LWMAX, INT( WORK( 1 ) ) )
deallocate( WORK )
write (*,*) ' optimal LWORK = ', LWORK
allocate( WORK(LWORK) )
CALL DGESVD( 'All', 'All', M, N, Asave, LDA, S, U, LDU, VT, LDVT, WORK, LWORK, INFO )

! set the smallest inverse singular value to zero
! and rescale the solution by subtracting a multiple of the last singular eigenvector
! this is based on an email conversation with Gregery Buzzard (Purdue University)
u1 = u(:,1:n-1)
tmp = transpose(VT(1:n,1:n))
v1 = tmp(:,1:n-1)
write (*,*) ' v1 : ', v1(1,:)
s1 = 0.D0 
s1_inv = 0.D0
do i=1,n-1
  s1(i,i) = s(i)
  s1_inv(i,i) = 1.D0/s(i)
end do
a1 = matmul(s1, transpose(v1))
b1 = matmul(transpose(u1),b)
x1 = matmul(v1,matmul(s1_inv,b1))
v0 = tmp(:,n)

! and rescale the solution x1 by a multiple of the singular vector ... 
write (*,*) ' original solution : ', x1
write (*,*) 'original trace : ', x1(1)+x1(5)+x1(9)
write (*,*) ' correction vector : ', x1(1) / v0(1) * v0
Api = x1 - (x1(1) + x1(5)+x1(9)) / (v0(1)+v0(5)+v0(9)) * v0
write (*,*) ' Api = ', Api 
write (*,*) ' trace = ', Api(1) + Api(5) + Api(9) 
 
end function pseudo_inv

end module mod_HREBSD
