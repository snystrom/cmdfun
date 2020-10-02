## Test environments
* local R installation, R 3.6.2
* windows-latest (release) - github actions
* macOS-latest (release) - github actions
* ubuntu-20.04 (release and devel) - github actions
* Windows Server 2008 R2 SP1 (devel) - rhub
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* New submission
X-CRAN-Comment: Archived on 2020-09-22 for policy violation
Checking creates 'meme' in home.
  Maintainer: 'Spencer Nystrom <nystromdev@gmail.com>'

This was an error in the vignette which I had not correctly sandboxed to prevent
from running during vignette rebuilds. To fix this issue, I have moved the
original vignette to the package website in order to better provide high-quality
examples to users. The new vignette directs users to this webpage and no longer
contains code which writes to userspace.

Also note, this error was caught a day after initial publication to CRAN. Kurt
Hornik informed me that the usual 30 day waiting period for resubmission could
be relaxed since this issue was not caught during the usual new submission
package review period.
