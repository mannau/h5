AC_INIT([h5], 0.9.2, [mario.annau@gmail.com])

dnl Now find the compiler and compiler flags to use
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi

AC_CHECK_PROG(H5CPP,h5c++,`which h5c++`)
if test -z "${H5CPP}"; then
  AC_MSG_ERROR(['h5c++' does not seem to be installed on your platform.
  	Please install the hdf5 library with C++ interface enabled (--enable-cxx=yes).
  	The required HDF5 library files can be installed as follows:
  	- Debian-based (e.g Ubuntu): 'sudo apt-get install libhdf5-dev'
        - Old Debian-based (squeeze, precise): 'sudo apt-get install libhdf5-serial-dev'
        - OS X using Homebrew: 'brew install homebrew/science/hdf5 --enable-cxx'
        - RPM-based (e.g Fedora): 'sudo yum install hdf5-devel'])
fi

if test -z "${LIBS}"; then
  dnl prefer ggrep if installed
  AC_CHECK_PROG(GREP,ggrep,`which ggrep`, `which grep`)
  
  dnl check if $GREP supports option -o
  AC_MSG_CHECKING([whether ${GREP} accepts -o])
  if `echo '' | ${GREP} -o '' >/dev/null 2>&1`
  then
    AC_MSG_RESULT([yes])
  else
    AC_MSG_RESULT([no])
    AC_MSG_ERROR([${GREP} does not support option -o])
  fi
  [
  LIBS=`${H5CPP} -show | ${GREP} -o "\-L[^[:space:]]\+" | tr "\n" " "` 
  ]
  
  dnl Fix for newer Debian versions
  LIBS="${LIBS} -L. -lhdf5_cpp -lhdf5 -lz -lm"

fi

if test -z "${CPPFLAGS}"; then
  CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`
  dnl Fix for newer Debian versions
  if test -d /usr/include/hdf5/serial; then
    CPPFLAGS="${CPPFLAGS} -I/usr/include/hdf5/serial"
  fi
fi

dnl substitute CPPFLAGS and LIBS
AC_SUBST(CPPFLAGS)
AC_SUBST(LIBS)
dnl and do subsitution in the src/Makevars.in 
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
